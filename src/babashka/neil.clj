(ns babashka.neil
  {:no-doc true})

(require '[babashka.cli :as cli]
         '[babashka.curl :as curl]
         '[babashka.fs :as fs]
         '[borkdude.rewrite-edn :as r]
         '[cheshire.core :as cheshire]
         '[clojure.edn :as edn]
         '[clojure.set :as set]
         '[clojure.string :as str])

(def spec {:lib {:desc "Fully qualified library name."}
           :version {:desc "Optional. When not provided, picks newest version from Clojars or Maven Central."}
           :sha {:desc "When provided, assumes lib refers to Github repo."}
           :latest-sha {:desc "When provided, assumes lib refers to Github repo and then picks latest SHA from it."}
           :deps/root {:desc "Sets deps/root to give value."}
           :as {:desc "Use as dependency name in deps.edn"
                :coerce :symbol}
           :alias {:ref "<alias>"
                   :desc "Add to alias <alias>."
                   :coerce :keyword}
           :deps-file {:ref "<file>"
                       :desc "Add to <file> instead of deps.edn."
                       :default "deps.edn"}
           :limit {:coerce :long}})
           

(import java.net.URLEncoder)

(def version "0.0.33")

(def windows? (str/includes? (System/getProperty "os.name") "Windows"))

(def bb? (System/getProperty "babashka.version"))

(defn url-encode [s] (URLEncoder/encode s "UTF-8"))

(def curl-opts
  {:throw false
   :compressed (not windows?)})
(defn- curl-get-json [url]
  (-> (curl/get url curl-opts)
      :body (cheshire/parse-string true)))

(defn- get-clojars-artifact [qlib]
  (curl-get-json
   (format "https://clojars.org/api/artifacts/%s"
           qlib)))

(defn latest-clojars-version [qlib]
  (get (get-clojars-artifact qlib) :latest_release))

(defn clojars-versions [qlib {:keys [limit] :or {limit 10}}]
  (let [body (get-clojars-artifact qlib)]
    (->> body
         :recent_versions
         (map :version)
         (take limit))))

(defn- search-mvn [qlib limit]
  (:response
   (curl-get-json
    (format "https://search.maven.org/solrsearch/select?q=g:%s+AND+a:%s&rows=%s"
            (namespace qlib)
            (name qlib)
            (str limit)))))

(defn latest-mvn-version [qlib]
  (-> (search-mvn qlib 1)
      :docs
      first
      :latestVersion))

(defn mvn-versions [qlib {:keys [limit] :or {limit 10}}]
  (let [payload (search-mvn qlib limit)]
    (->> payload
         :docs
         (map :v))))

(defn default-branch [lib]
  (get (curl-get-json (format "https://api.github.com/repos/%s/%s"
                              (namespace lib) (name lib)))
       :default_branch))

(defn clean-github-lib [lib]
  (let [lib (str/replace lib "com.github." "")
        lib (str/replace lib "io.github." "")
        lib (symbol lib)]
    lib))

(defn latest-github-sha [lib]
  (let [lib (clean-github-lib lib)
        branch (default-branch lib)]
    (get (curl-get-json (format "https://api.github.com/repos/%s/%s/commits/%s"
                                (namespace lib) (name lib) branch))
         :sha)))

(defn list-github-tags [lib]
  (let [lib (clean-github-lib lib)]
    (curl-get-json (format "https://api.github.com/repos/%s/%s/tags"
                           (namespace lib) (name lib)))))

(defn latest-github-tag [lib]
  (-> (list-github-tags lib)
      first))

(defn find-github-tag [lib tag]
  (->> (list-github-tags lib)
       (filter #(= (:name %) tag))
       first))

(def deps-template
  (str/triml "
{:deps {}
 :aliases {}}
"))

(def bb-template
  (str/triml "
{:deps {}
 :tasks
 {
 }}
"))

(defn ensure-deps-file [opts]
  (let [target (:deps-file opts)]
    (when-not (fs/exists? target)
      (spit target (if (= "bb.edn" target)
                     bb-template
                     deps-template)))))

(defn edn-string [opts] (slurp (:deps-file opts)))

(defn edn-nodes [edn-string] (r/parse-string edn-string))

(def cognitect-test-runner-alias
  "
{:extra-paths [\"test\"]
 :extra-deps {io.github.cognitect-labs/test-runner
               {:git/tag \"v0.5.0\" :git/sha \"b3fd0d2\"}}
 :main-opts [\"-m\" \"cognitect.test-runner\"]
 :exec-fn cognitect.test-runner.api/test}")

(defn indent [s n]
  (let [spaces (apply str (repeat n " "))
        lines (str/split-lines s)]
    (str/join "\n" (map (fn [s]
                          (if (str/blank? s) s
                              (str spaces s))) lines))))

(defn clean-trailing-whitespace [s]
  (str/join "\n" (map str/trimr (str/split-lines s))))

(defn add-alias [opts alias-kw alias-contents]
  (ensure-deps-file opts)
  (let [edn-string (edn-string opts)
        edn-nodes (edn-nodes edn-string)
        edn (edn/read-string edn-string)
        alias (or (:alias opts)
                  alias-kw)
        alias-node (r/parse-string (str "\n " alias " ;; added by neil"))]
    (if-not (get-in edn [:aliases alias])
      (let [s (->> (r/update edn-nodes :aliases
                             (fn [aliases]
                               (let [s (indent alias-contents 1)
                                     alias-nodes (r/parse-string s)]
                                 (r/assoc aliases alias-node alias-nodes))))
                   str)
            s (clean-trailing-whitespace s)
            s (str s "\n")]
        (spit (:deps-file opts) s))
      (do (println (format "[neil] Project deps.edn already contains alias %s" (str alias ".")))
          ::update))))

(defn add-cognitect-test-runner [{:keys [opts]}]
  (add-alias opts :test cognitect-test-runner-alias))

(def kaocha-alias
  "
{:extra-deps {lambdaisland/kaocha {:mvn/version \"1.0.887\"}}}")

(defn add-kaocha [{:keys [opts]}]
  (add-alias opts :kaocha kaocha-alias))

(defn build-alias [opts]
  (let [latest-tag (latest-github-tag 'clojure/tools.build)
        tag (:name latest-tag)
        sha (-> latest-tag :commit :sha (subs 0 7))
        s (format "
{:deps {io.github.clojure/tools.build {:git/tag \"%s\" :git/sha \"%s\"}{{deps-deploy}}}
 :ns-default build}"
                  tag sha)]
    {:s (str/replace s "{{deps-deploy}}"
                     (if (:deps-deploy opts)
                       "\n        slipset/deps-deploy {:mvn/version \"0.2.0\"}"
                       ""))
     :tag tag
     :sha sha}))

(defn build-file
  [opts]
  (let [base "(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'my/lib1)
(def version (format \"1.2.%s\" (b/git-count-revs nil)))
(def class-dir \"target/classes\")
(def basis (b/create-basis {:project \"deps.edn\"}))
(def uber-file (format \"target/%s-%s-standalone.jar\" (name lib) version))
(def jar-file (format \"target/%s-%s.jar\" (name lib) version))

(defn clean [_]
  (b/delete {:path \"target\"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs [\"src\"]})
  (b/copy-dir {:src-dirs [\"src\" \"resources\"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs [\"src\" \"resources\"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :src-dirs [\"src\"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis}))
"]
    (if (:deps-deploy opts)
      (str base
           "
(defn deploy [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
    (merge {:installer :remote
                       :artifact jar-file
                       :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
                    opts))
  opts)
")
      base)))

(defn add-build [{:keys [opts]}]
  (if-not (fs/exists? "build.clj")
    (spit "build.clj" (build-file opts))
    (println "[neil] Project build.clj already exists."))
  (ensure-deps-file opts)
  (let [ba (build-alias opts)]
    (when (= ::update (add-alias opts :build (:s ba)))
      (println "[neil] Updating tools build to newest git tag + sha.")
      (let [edn-string (edn-string opts)
            edn (edn/read-string edn-string)
            build-alias (get-in edn [:aliases :build :deps 'io.github.clojure/tools.build])
            [tag-key sha-key]
            (cond (and
                   (:tag build-alias)
                   (:sha build-alias))
                  [:tag :sha]
                  (and
                   (:git/tag build-alias)
                   (:git/sha build-alias))
                  [:git/tag :git/sha])]
        (when (and tag-key sha-key)
          (let [nodes (edn-nodes edn-string)
                nodes (r/assoc-in nodes [:aliases :build :deps 'io.github.clojure/tools.build tag-key]
                                  (:tag ba))
                nodes (r/assoc-in nodes [:aliases :build :deps 'io.github.clojure/tools.build sha-key]
                                  (:sha ba))
                s (str (str/trim (str nodes)) "\n")]
            (spit (:deps-file opts) s)))))))

(defn print-dep-add-help []
  (println "Usage: neil add dep [lib] [options]")
  (println "Options:")
  (println (cli/format-opts
            {:spec spec
             :order [:lib :version :sha :latest-sha :deps/root :as :alias :deps-file]})))

(defn dep-add [{:keys [opts]}]
  (if (:help opts)
    (print-dep-add-help)
    (do
      (ensure-deps-file opts)
      (let [edn-string (edn-string opts)
            edn-nodes (edn-nodes edn-string)
            lib (:lib opts)
            lib (symbol lib)
            explicit-git? (or (:sha opts)
                              (:latest-sha opts))
            [version git?] (if explicit-git?
                             [(or (:sha opts)
                                  (latest-github-sha lib)) true]
                             (or
                              (when-let [v (:version opts)]
                                [v false])
                              (when-let [v (latest-clojars-version lib)]
                                [v false])
                              (when-let [v (latest-mvn-version lib)]
                                [v false])
                              (when-let [v (latest-github-sha lib)]
                                [v true])))
            mvn? (not git?)
            git-url (when git?
                      (or (:git/url opts)
                          (str "https://github.com/" (clean-github-lib lib))))
            as (or (:as opts) lib)
            ;; force newline
            edn-nodes (-> edn-nodes (r/assoc-in [:deps as] nil) str r/parse-string)
            nodes (cond
                    mvn?
                    (r/assoc-in edn-nodes [:deps as]
                                {:mvn/version version})
                    git?
                    ;; multiple steps to force newlines
                    (-> edn-nodes
                        (r/assoc-in
                         [:deps as :git/url] git-url)
                        str
                        r/parse-string
                        (r/assoc-in
                         [:deps as :git/sha] version)))
            nodes (if-let [root (and git? (:deps/root opts))]
                    (-> nodes
                        (r/assoc-in [:deps as :deps/root] root))
                    nodes)
            s (str (str/trim (str nodes)) "\n")]
        (spit (:deps-file opts) s)))))

(defn dep-versions [{:keys [opts]}]
  (let [lib (:lib opts)
        lib (symbol lib)
        versions (or (seq (clojars-versions lib opts))
                     (seq (mvn-versions lib opts)))]
    (if-not versions
      (binding [*out* *err*]
        (println "Unable to find" lib "on Clojars or Maven.")
        (System/exit 1))
      (doseq [v versions]
        (println :lib lib :version v)))))

(defn dep-search [{:keys [opts]}]
  (let [search-term (:search-term opts)
        url (str "https://clojars.org/search?format=json&q=\"" (url-encode search-term) "\"")
        {search-results :results
         results-count :count} (curl-get-json url)]
    (when (zero? results-count)
      (binding [*out* *err*]
        (println "Unable to find" search-term  "on Clojars.")
        (System/exit 1)))
    (doseq [search-result search-results]
      (println :lib (format  "%s/%s"
                             (:group_name search-result)
                             (:jar_name search-result))
               :version (:version search-result)
               :description (pr-str (:description search-result))))))

(defn- built-in-template?
  "Returns true if the template name maps to a function in org.corfield.new."
  [template]
  (contains? (set (map name (keys (ns-publics 'org.corfield.new)))) template))

(defn- github-repo-http-url [lib]
  (str "https://github.com/" (clean-github-lib lib)))

(def github-repo-ssh-regex #"^git@github.com:([^/]+)/([^\.]+)\.git$")
(def github-repo-http-regex #"^https://github.com/([^/]+)/([^\.]+)(\.git)?$")

(defn- parse-git-url [git-url]
  (let [[[_ gh-user repo-name]] (or (re-seq github-repo-ssh-regex git-url)
                                    (re-seq github-repo-http-regex git-url))]
    (if (and gh-user repo-name)
      {:gh-user gh-user :repo-name repo-name}
      (throw (ex-info "Failed to parse :git/url" {:git/url git-url})))))

(defn- git-url->lib-sym [git-url]
  (when-let [{:keys [gh-user repo-name]} (parse-git-url git-url)]
    (symbol (str "io.github." gh-user) repo-name)))

(def lib-opts->template-deps-fn
  "A map to define valid CLI options for deps-new template deps.

  - Each key is a sequence of valid combinations of CLI opts.
  - Each value is a function which returns a tools.deps lib map."
  {[#{:local/root}]
   (fn [lib-sym lib-opts]
     {lib-sym (select-keys lib-opts [:local/root])})

   [#{} #{:git/url}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           {:keys [name commit]} (latest-github-tag (git-url->lib-sym url))]
       {lib-sym {:git/url url :git/tag name :git/sha (:sha commit)}}))

   [#{:git/tag} #{:git/url :git/tag}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           tag (:git/tag lib-opts)
           {:keys [commit]} (find-github-tag (git-url->lib-sym url) tag)]
       {lib-sym {:git/url url :git/tag tag :git/sha (:sha commit)}}))

   [#{:git/sha} #{:git/url :git/sha}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           sha (:git/sha lib-opts)]
       {lib-sym {:git/url url :git/sha sha}}))

   [#{:latest-sha} #{:git/url :latest-sha}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           sha (latest-github-sha (git-url->lib-sym url))]
       {lib-sym {:git/url url :git/sha sha}}))

   [#{:git/url :git/tag :git/sha}]
   (fn [lib-sym lib-opts]
     {lib-sym (select-keys lib-opts [:git/url :git/tag :git/sha])})})

(def valid-lib-opts
  "The set of all valid combinations of deps-new template deps opts."
  (into #{} cat (keys lib-opts->template-deps-fn)))

(defn- deps-new-cli-opts->lib-opts
  "Returns parsed deps-new template deps opts from raw CLI opts."
  [cli-opts]
  (-> cli-opts
      (set/rename-keys {:sha :git/sha})
      (select-keys (into #{} cat valid-lib-opts))))

(defn- invalid-lib-opts-error [provided-lib-opts]
  (ex-info (str "Provided invalid combination of CLI options for deps-new "
                "template deps.")
           {:provided-opts (set (keys provided-lib-opts))
            :valid-combinations valid-lib-opts}))

(defn- find-template-deps-fn
  "Returns a template-deps-fn given lib-opts parsed from raw CLI opts."
  [lib-opts]
  (some (fn [[k v]] (and (contains? (set k) (set (keys lib-opts))) v))
        lib-opts->template-deps-fn))

(defn- template-deps
  "Returns a tools.deps lib map for the given CLI opts."
  [template cli-opts]
  (let [lib-opts (deps-new-cli-opts->lib-opts cli-opts)
        lib-sym (edn/read-string template)
        template-deps-fn (find-template-deps-fn lib-opts)]
    (if-not template-deps-fn
      (throw (invalid-lib-opts-error lib-opts))
      (template-deps-fn lib-sym lib-opts))))

(def create-opts-deny-list
  [:deps-file :dry-run :git/sha :git/url :latest-sha :local/root :sha])

(defn- deps-new-plan
  "Returns a plan for calling org.corfield.new/create.

  :template-deps - These deps will be added with babashka.deps/add-deps before
                   calling the create function.

  :create-opts   - This map contains the options that will be passed to the
                   create function."
  [cli-opts]
  (let [create-opts (merge {:template "scratch"}
                           (apply dissoc cli-opts create-opts-deny-list))
        tpl-deps (when (and bb? (not (built-in-template? (:template create-opts))))
                   (template-deps (:template create-opts) cli-opts))]
    (merge (when tpl-deps {:template-deps tpl-deps})
           {:create-opts create-opts})))

(defn- deps-new-create [create-opts]
  ((requiring-resolve 'org.corfield.new/create) create-opts))

(defn print-new-help []
  (println (str/trim "
Usage: neil new [template] [name] [target-dir] [options]

Runs the org.corfield.new/create function from deps-new.

All of the deps-new options can be provided as CLI options:

https://github.com/seancorfield/deps-new/blob/develop/doc/options.md

Both built-in and external templates are supported. Built-in templates use
unqualified names (e.g. scratch) whereas external templates use fully-qualified
names (e.g. io.github.kit/kit-clj).

If an external template is provided, the babashka.deps/add-deps function will be
called automatically before running org.corfield.new/create. The deps for the
template are inferred automatically from the template name. The following
options can be used to control the add-deps behavior:

  --local/root
    Override the :deps map to use the provided :local/root path.

  --git/url
    Override the :git/url in the :deps map. If no URL is provided, a template
    name starting with io.github or com.github is expected and the URL will
    point to GitHub.

  --git/tag
    Override the :git/tag in the :deps map. If no SHA or tag is provided, the
    latest tag from the default branch of :git/url will be used.

  --sha
  --git/sha
    Override the :git/sha in the :deps map. If no SHA or tag is provided, the
    latest tag from the default branch of :git/url will be used.

  --latest-sha
    Override the :git/sha in the :deps map with the latest SHA from the
    default branch of :git/url.")))

(defn- deps-new-set-classpath
  "The java.class.path required by org.corfield.new/create."
  []
  (let [classpath ((requiring-resolve 'babashka.classpath/get-classpath))]
    (System/setProperty "java.class.path" classpath)))

(defn- deps-new-add-template-deps
  "Adds template deps at runtime."
  [template-deps]
  ((requiring-resolve 'babashka.deps/add-deps) {:deps template-deps}))

(defn run-deps-new
  "Runs org.corfield.new/create using the provided CLI options.

  To support the dry run feature, side-effects should be described in the plan
  provided by deps-new-plan. This function's job is to execute side-effects
  using the plan to provide repeatability."
  [{:keys [opts]}]
  (if (:help opts)
    (print-new-help)
    (do
      (require 'org.corfield.new)
      (let [plan (deps-new-plan opts)
            {:keys [template-deps create-opts]} plan]
        (if (:dry-run opts)
          (do (prn plan) nil)
          (do
            (when template-deps (deps-new-add-template-deps template-deps))
            (when bb? (deps-new-set-classpath))
            (deps-new-create create-opts)
            nil))))))

(defn print-help [_]
  (println (str/trim "
Usage: neil <subcommand> <options>

Most subcommands support the options:
  --alias      Override alias name.
  --deps-file  Override deps.edn file name.

Subcommands:

add
  dep    Alias for `neil dep add`.
  test   adds cognitect test runner to :test alias.
  build  adds tools.build build.clj file and :build alias.
    Options:
    --deps-deploy Adds deps-deploy as dependency and deploy task in build.clj
  kaocha adds kaocha test runner to :koacha alias.

dep
  add: Adds --lib, a fully qualified symbol, to deps.edn :deps.
    Run neil add dep --help to see all options.

new:
  Create a project using deps-new
    Run neil new --help to see all options.

  Examples:
    neil new scratch foo --overwrite
    neil new io.github.rads/neil-new-test-template foo2 --latest-sha

license
  list   Lists commonly-used licenses available to be added to project. Takes an optional search string to filter results.
  search Alias for `list`
  add    Writes license text to a file
    Options:
    --license The key of the license to use (e.g. epl-1.0, mit, unlicense). --license option name may be elided when license key is provided as first argument.
    --file    The file to write. Defaults to 'LICENSE'.
")))

;; licenses
(def licenses-api-url "https://api.github.com/licenses")

(defn license-search [{:keys [opts]}]
  (let [search-term (:search-term opts)
        license-vec (->> (str licenses-api-url "?per_page=50")
                         curl-get-json
                         (map #(select-keys % [:key :name])))
        search-results (if search-term
                         (filter #(str/includes?
                                   (str/lower-case (:name %))
                                   (str/lower-case search-term))
                                 license-vec)
                         license-vec)]
    (if (empty? search-results)
      (binding [*out* *err*]
        (println "No licenses found")
        (System/exit 1))
      (doseq [result search-results]
        (println :license (:key result) :name (pr-str (:name result)))))))

(defn license-to-file [{:keys [opts]}]
  (let [license-key (:license opts)
        output-file (or (:file opts) "LICENSE")
        {:keys [message name body]} (some->> license-key url-encode
                                             (str licenses-api-url "/")
                                             curl-get-json)]
    (cond
      (not license-key) (throw (ex-info "No license key provided." {}))
      (= message "Not Found")
      (throw (ex-info (format "License '%s' not found." license-key) {:license license-key}))
      (not body)
      (throw (ex-info (format "License '%s' has no body text." (or name license-key))
                      {:license license-key}))
      :else (spit output-file body))))

(defn add-license [opts]
  (try
    (license-to-file opts)
    (catch Exception e
      (binding [*out* *err*]
        (println (ex-message e))
        (System/exit 1)))))

(defn print-version [_]
  (println "neil" version))

(defn -main [& _args]
  (cli/dispatch
   [{:cmds ["add" "dep"] :fn dep-add :cmds-opts [:lib]}
    {:cmds ["add" "test"] :fn add-cognitect-test-runner}
    {:cmds ["add" "build"] :fn add-build}
    {:cmds ["add" "kaocha"] :fn add-kaocha}
    {:cmds ["dep" "versions"] :fn dep-versions :cmds-opts [:lib]}
    {:cmds ["dep" "add"] :fn dep-add :cmds-opts [:lib]}
    {:cmds ["dep" "search"] :fn dep-search :cmds-opts [:search-term]}
    {:cmds ["license" "list"] :fn license-search :cmds-opts [:search-term]}
    {:cmds ["license" "search"] :fn license-search :cmds-opts [:search-term]}
    {:cmds ["license" "add"] :fn add-license :cmds-opts [:license]}
    {:cmds ["new"] :fn run-deps-new :cmds-opts [:template :name :target-dir]}
    {:cmds ["version"] :fn print-version}
    {:cmds ["help"] :fn print-help}
    {:cmds [] :fn (fn [{:keys [opts] :as m}]
                    (if (:version opts)
                      (print-version m)
                      (print-help m)))}]
   *command-line-args*
   {:spec spec
    :exec-args {:deps-file "deps.edn"}}))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
