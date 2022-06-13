(ns babashka.neil
  {:no-doc true})

(require '[babashka.cli :as cli]
         '[babashka.curl :as curl]
         '[babashka.fs :as fs]
         '[borkdude.rewrite-edn :as r]
         '[cheshire.core :as cheshire]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(import java.net.URLEncoder)

(def version "0.0.31")

(def windows? (str/includes? (System/getProperty "os.name") "Windows"))

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

(defn latest-github-tag [lib]
  (let [lib (clean-github-lib lib)]
    (first
     (curl-get-json (format "https://api.github.com/repos/%s/%s/tags"
                            (namespace lib) (name lib))))))

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

(defn add-dep [{:keys [opts]}]
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
    (spit (:deps-file opts) s)))

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

(defn print-help [_]
  (println (str/trim "
Usage: neil <subcommand> <options>

Most subcommands support the options:

- :alias - override alias name
- :deps-file - override deps.edn file name

Subcommands:

add

  - dep: alias for `neil dep add`. Deprecated.

  - test: adds cognitect test runner to :test alias.

  - build: adds tools.build build.clj file and :build alias.

    Options:

    :deps-deploy true - adds deps-deploy as dependency and deploy task in build.clj

  - kaocha: adds kaocha test runner to :koacha alias.

dep

  - add: adds :lib, a fully qualified symbol, to deps.edn :deps.

    Options:

    :lib - Fully qualified symbol. :lib keyword may be elided when lib name is provided as first option.
    :version - Optional version. When not provided, picks newest version from Clojars or Maven Central.
    :sha - When provided, assumes lib refers to Github repo.
    :latest-sha - When provided, assumes lib refers to Github repo and then picks latest SHA from it.
    :deps/root - Set :deps/root to given value
    :as - Use as dependency name in deps.edn

  - search: lists available libraries on Clojars matching a search string.

  - versions: lists available versions of :lib. Suppports Clojars/Maven coordinates, no Git deps yet.

    Options:

    :lib - Fully qualified symbol. :lib keyword may be elided when lib name is provided as first option.

license

  - list: lists commonly-used licenses available to be added to project. Takes an optional search string
          to filter results.

  - search: alias for `list`

  - add: writes license text to a file

    Options:

    :license - The key of the license to use (e.g. epl-1.0, mit, unlicense). :license keyword may be
               elided when license key is provided as first argument.
    :file - The file to write. Defaults to 'LICENSE'.

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
   [{:cmds ["add" "dep"] :fn add-dep :cmds-opts [:lib]}
    {:cmds ["add" "test"] :fn add-cognitect-test-runner}
    {:cmds ["add" "build"] :fn add-build}
    {:cmds ["add" "kaocha"] :fn add-kaocha}
    {:cmds ["dep" "versions"] :fn dep-versions :cmds-opts [:lib]}
    {:cmds ["dep" "add"] :fn add-dep :cmds-opts [:lib]}
    {:cmds ["dep" "search"] :fn dep-search :cmds-opts [:search-term]}
    {:cmds ["license" "list"] :fn license-search :cmds-opts [:search-term]}
    {:cmds ["license" "search"] :fn license-search :cmds-opts [:search-term]}
    {:cmds ["license" "add"] :fn add-license :cmds-opts [:license]}
    {:cmds ["version"] :fn print-version}
    {:cmds ["help"] :fn print-help}
    {:cmds [] :fn (fn [{:keys [opts] :as m}]
                    (if (:version opts)
                      (print-version m)
                      (print-help m)))}]
   *command-line-args*
   {:coerce {:deps-deploy parse-boolean
             :as symbol
             :alias keyword
             :limit parse-long}
    :exec-args {:deps-file "deps.edn"}}))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
