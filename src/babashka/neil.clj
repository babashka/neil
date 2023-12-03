(ns babashka.neil
  {:no-doc true}
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [babashka.neil.curl :refer [curl-get-json url-encode]]
   [babashka.neil.git :as git]
   [babashka.neil.new :as new]
   [babashka.neil.project :as proj]
   [babashka.neil.utils :refer [req-resolve]]
   [babashka.neil.rewrite :as rw]
   [babashka.neil.test :as neil-test]
   [babashka.neil.version :as neil-version]
   [borkdude.rewrite-edn :as r]
   [babashka.http-client :as http]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.set :as set]))

(def *neil-opts* (atom nil))

(def spec {:lib {:desc "Fully qualified library name."}
           :version {:desc "Optional. When not provided, picks newest version from Clojars or Maven Central."
                     :coerce :string}
           :sha {:desc "When provided, assumes lib refers to Github repo."
                 :coerce :string}
           :latest-sha {:coerce :boolean :desc "When provided, assumes lib refers to Github repo and then picks latest SHA from it."}
           :tag {:desc "When provided, assumes lib refers to Github repo."
                 :coerce :string}
           :latest-tag {:coerce :boolean :desc "When provided, assumes lib refers to Github repo and then picks latest tag from it."}
           :deps/root {:desc "Sets deps/root to give value."}
           :as {:desc "Use as dependency name in deps.edn"
                :coerce :symbol}
           :alias {:ref "<alias>"
                   :desc "Add to alias <alias>."
                   :coerce :keyword}
           :deps-file {:ref "<file>"
                       :desc "Add to <file> instead of deps.edn."
                       :coerce :string
                       :default "deps.edn"}
           :limit {:coerce :long}
           :dry-run {:coerce :boolean
                     :desc "dep upgrade only. Prevents updates to deps.edn."}
           :no-aliases {:coerce :boolean
                        :desc "Prevents updates to alias :extra-deps when upgrading."}})

(def windows? (fs/windows?))

(def bb? (System/getProperty "babashka.version"))

(defn- get-clojars-artifact [qlib]
  (curl-get-json
   (format "https://clojars.org/api/artifacts/%s"
           qlib)))

(defn stable-version?
  [version-str]
  (let [vparse (req-resolve 'version-clj.core/parse)] ; We lazy-load version-clj to improve average script startup time
    (empty? (set/intersection (:qualifiers (vparse version-str))
                              #{"rc" "alpha" "beta" "snapshot" "milestone"}))))

(defn first-stable-version [versions]
  (->> versions
       (filter stable-version?)
       first))

(defn clojars-versions [qlib {:keys [limit] :or {limit 10}}]
  (let [body (get-clojars-artifact qlib)]
    (->> body
         :recent_versions
         (map :version)
         (take limit))))

(defn latest-stable-clojars-version
  [qlib]
  (first-stable-version (clojars-versions qlib {:limit 100})))

(defn latest-clojars-version [qlib]
  (first (clojars-versions qlib {:limit 100})))

(defn- search-mvn [qlib limit]
  (:response
   (curl-get-json
    (format "https://search.maven.org/solrsearch/select?q=g:%s+AND+a:%s&rows=%s&core=gav&wt=json"
            (namespace qlib)
            (name qlib)
            (str limit)))))

(defn mvn-versions [qlib {:keys [limit] :or {limit 10}}]
  (let [payload (search-mvn qlib limit)]
    (->> payload
         :docs
         (map :v))))

(defn latest-stable-mvn-version [qlib]
  (first-stable-version (mvn-versions qlib {:limit 100})))

(defn latest-mvn-version [qlib]
  (first (mvn-versions qlib {:limit 100})))

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

(defn add-alias [opts alias-kw alias-contents]
  (ensure-deps-file opts)
  (let [edn-string (edn-string opts)
        edn-nodes (edn-nodes edn-string)
        edn (edn/read-string edn-string)
        alias (or (:alias opts)
                  alias-kw)
        existing-aliases (get-in edn [:aliases])
        alias-node (r/parse-string
                    (str (when (seq existing-aliases) "\n ")
                         alias
                         " ;; added by neil"))]
    (if-not (get existing-aliases alias)
      (let [s (-> (if-not (seq existing-aliases)
                                        ; If there are no existing aliases, we assoc an empty map
                                        ; before updating to prevent borkdude.rewrite-edn/update
                                        ; from removing the newline preceding the :aliases key.
                    (r/assoc edn-nodes :aliases {})
                    edn-nodes)
                  (r/update :aliases
                            (fn [aliases]
                              (let [s (rw/indent alias-contents 1)
                                    alias-nodes (r/parse-string s)
                                    aliases' (r/assoc aliases alias-node alias-nodes)]
                                (if-not (seq existing-aliases)
                                        ; If there are no existing aliases, add an
                                        ; explicit newline after the :aliases key.
                                  (r/parse-string (str "\n" (rw/indent (str aliases') 1)))
                                  aliases'))))

                  str)
            s (rw/clean-trailing-whitespace s)
            s (str s "\n")]
        (spit (:deps-file opts) s))
      (do (println (format "[neil] Project deps.edn already contains alias %s" (str alias ".")))
          ::update))))

(declare print-help)

(defn add-cognitect-test-runner [{:keys [opts] :as cmd}]
  (if (:help opts)
    (print-help cmd)
    (do (add-alias opts :test cognitect-test-runner-alias)

        (when-let [pn (proj/project-name opts)]
          (let [test-ns (symbol (str (str/replace pn "/" ".") "-test"))
                test-path (-> (str test-ns)
                              (str/replace "-" "_")
                              (str/replace "." fs/file-separator)
                              (str ".clj"))
                test-path (fs/file "test" test-path)]
            (when (or (not (fs/exists? "test"))
                      (zero? (count (fs/list-dir "test"))))
              (fs/create-dirs (fs/parent test-path))
              (spit test-path
                    (format "(ns %s
  (:require [clojure.test :as t :refer [deftest is testing]]))

(deftest %s-test
  (testing \"TODO: fix\"
    (is (= :foo :bar))))
" test-ns (name pn)))))))))

(defn kaocha-alias []
  (format "
{:extra-deps {lambdaisland/kaocha {:mvn/version \"%s\"}}
 :main-opts [\"-m\" \"kaocha.runner\"]}"
          (latest-stable-clojars-version 'lambdaisland/kaocha)))

(defn add-kaocha [{:keys [opts] :as cmd}]
  (if (:help opts)
    (print-help cmd)
    (do
      (add-alias opts :kaocha (kaocha-alias))
      (println (str/trim "
If you wish to create a `bin/kaocha` file, copy and run the following:

mkdir -p bin && \\
echo '#!/usr/bin/env bash
clojure -M:kaocha \"$@\"' > bin/kaocha && \\
chmod +x bin/kaocha
")))))

(defn nrepl-alias []
  (format "
{:extra-deps {nrepl/nrepl {:mvn/version \"%s\"}}
 :main-opts [\"-m\" \"nrepl.cmdline\" \"--interactive\" \"--color\"]}"
          (latest-stable-clojars-version 'nrepl/nrepl)))

(defn add-nrepl [{:keys [opts] :as cmd}]
  (if (:help opts)
    (print-help cmd)
    (add-alias opts :nrepl (nrepl-alias))))

(defn build-alias [_opts]
  (let [latest-tag (git/latest-github-tag 'clojure/tools.build)
        tag (:name latest-tag)
        sha (-> latest-tag :commit :sha (subs 0 7))
        s (format "
{:deps {io.github.clojure/tools.build {:git/tag \"%s\" :git/sha \"%s\"}
        slipset/deps-deploy {:mvn/version \"0.2.0\"}}
 :ns-default build}"
                  tag sha)]
    {:s s
     :tag tag
     :sha sha}))

(defn build-file
  [_opts]
  (let [base "(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.edn :as edn]))

(def project (-> (edn/read-string (slurp \"deps.edn\"))
                 :aliases :neil :project))
(def lib (or (:name project) 'my/lib1))

;; use neil project set version 1.2.0 to update the version in deps.edn

(def version (or (:version project)
                 \"1.2.0\"))
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

(defn install [_]
  (jar {})
  (b/install {:basis basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

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

(defn deploy [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
    (merge {:installer :remote
                       :artifact jar-file
                       :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
                    opts))
  opts)

"]
    base))

(defn add-build [{:keys [opts] :as cmd}]
  (if (:help opts)
    (print-help cmd)
    (do
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
                (spit (:deps-file opts) s)))))))))

(defn print-dep-add-help []
  (println "Usage: neil add dep [lib] [options]")
  (println "Options:")
  (println (cli/format-opts
            {:spec spec
             :order [:lib :version :sha :latest-sha :tag :latest-tag :deps/root :as :alias :deps-file]})))

(defn log [& xs]
  (binding [*out* *err*]
    (apply prn xs)))

(defn dep-add [{:keys [opts]}]
  (if (or (:help opts) (:h opts) (not (:lib opts)))
    (print-dep-add-help)
    (do
      (ensure-deps-file opts)
      (let [edn-string (edn-string opts)
            edn-nodes (edn-nodes edn-string)
            lib (:lib opts)
            lib (symbol lib)
            lib (symbol (or (namespace lib) (name lib))
                        (name lib))
            alias (:alias opts)
            explicit-git-sha? (or (:sha opts) (:latest-sha opts))
            explicit-git-tag? (or (:tag opts) (:latest-tag opts))
            [version coord-type?]
            (cond explicit-git-tag?
                  [(or (and (:tag opts)
                            (git/find-github-tag lib (:tag opts)))
                       (git/latest-github-tag lib)) :git/tag]
                  explicit-git-sha?
                  [(or (:sha opts) (git/latest-github-sha lib)) :git/sha]
                  :else
                  (or
                   (when-let [v (:version opts)]
                     [v :mvn])
                   (when-let [v (latest-stable-clojars-version lib)]
                     [v :mvn])
                   (when-let [v (latest-stable-mvn-version lib)]
                     [v :mvn])
                   (when-let [v (git/latest-github-sha lib)]
                     [v :git/sha])
                   (when-let [v (latest-clojars-version lib)]
                     [v :mvn])
                   (when-let [v (latest-mvn-version lib)]
                     [v :mvn])))
            missing? (nil? version)
            mvn? (= coord-type? :mvn)
            git-sha? (= coord-type? :git/sha)
            git-tag? (= coord-type? :git/tag)
            git-url (when (or git-sha? git-tag?)
                      (or (:git/url opts)
                          (str "https://github.com/" (git/clean-github-lib lib))))
            as (or (:as opts) lib)
            existing-aliases (-> edn-string edn/read-string :aliases)
            path (if alias
                   [:aliases
                    alias
                    (if (get-in existing-aliases [alias :deps]) :deps :extra-deps)
                    as]
                   [:deps as])
            nl-path (if (and alias
                             (not (contains? existing-aliases alias)))
                      [:aliases alias]
                      path)
            edn-nodes (if (r/get-in edn-nodes nl-path)
                        ;; if this dep already exists, don't touch it.
                        ;; We risk loosing :exclusions and other properties.
                        edn-nodes
                        ;; otherwise, force newlines!
                        ;; force newline in
                        ;;
                        ;;     [:deps as] if no alias
                        ;;     [:aliases alias] if alias DNE
                        ;;     [:aliases alias :deps as] if :deps present
                        ;;     [:aliases alias :extra-deps as] if alias exists
                        (-> edn-nodes (r/assoc-in nl-path nil) str r/parse-string))
            nodes (cond
                    missing? edn-nodes
                    mvn?
                    (r/assoc-in edn-nodes (conj path :mvn/version) version)
                    git-sha?
                    ;; multiple steps to force newlines
                    (-> edn-nodes
                        (r/assoc-in (conj path :git/url) git-url)
                        str
                        r/parse-string
                        (r/assoc-in (conj path :git/sha) version)
                        (r/update-in path r/dissoc :sha))

                    git-tag?
                    ;; multiple steps to force newlines
                    (-> edn-nodes
                        (r/assoc-in (conj path :git/url) git-url)
                        str
                        r/parse-string
                        (r/assoc-in (conj path :git/tag) (-> version :name))
                        str
                        r/parse-string
                        (r/assoc-in (conj path :git/sha)
                                    (some-> version :commit :sha (subs 0 7)))))
            nodes (if-let [root (and (or git-sha? git-tag?) (:deps/root opts))]
                    (-> nodes
                        (r/assoc-in (conj path :deps/root) root))
                    nodes)
            s (str (str/trim (str nodes)) "\n")]
        (when-not missing?
          (spit (:deps-file opts) s))))))

(defn dep-versions [{:keys [opts]}]
  (when (or (:help opts) (:h opts))
    (println (str/trim "
Usage: neil dep versions LIB

List available versions of a Clojure dependency. Only supports Clojars.

  $ neil dep versions http-kit/http-kit
  :lib http-kit/http-kit :version 2.7.0-alpha1
  :lib http-kit/http-kit :version 2.7.0-SNAPSHOT
  :lib http-kit/http-kit :version 2.6.0
  :lib http-kit/http-kit :version 2.6.0-RC1
  :lib http-kit/http-kit :version 2.6.0-alpha1
  :lib http-kit/http-kit :version 2.5.3
  :lib http-kit/http-kit :version 2.5.3-SNAPSHOT
  :lib http-kit/http-kit :version 2.5.2
  :lib http-kit/http-kit :version 2.5.1
  :lib http-kit/http-kit :version 2.5.0"))
    (System/exit 0))
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

(defn print-dep-search-help []
  (println (str/trim "
Usage: neil dep search [lib]

Search Clojars for a string in any attribute of an artifact:

  $ neil dep search \"babashka.nrepl\"
  :lib babashka/babashka.nrepl :version 0.0.6

You can also search for the fully ns-qualified library:

  $ neil dep search \"babashka/babashka.nrepl\"
  :lib babashka/babashka.nrepl :version 0.0.6

To search for all artifacts in a group:

  $ neil dep search \"group-id:babashka\"
  :lib babashka/babashka :version 1.1.173
  :lib babashka/fs :version 0.2.15
  ...

A search string can also be matched in a library's description:

  $ neil dep search \"test framework\"

will return libraries with 'test framework' in their description.

See http://github.com/clojars/clojars-web/wiki/Search-Query-Syntax for
details on the search syntax.")))

(defn dep-search-maven [search-term]
  (let [url (format
             "https://search.maven.org/solrsearch/select?q=%s&rows=20&wt=json"
             (url-encode search-term))
        keys-m {:g :group_name
                :a :jar_name
                :timestamp :created
                :latestVersion :version}
        add-desc (fn [{:keys [group_name jar_name] :as m}]
                   ;; Maven doesn't provide package description through its API,
                   ;; but that doesn't mean we should just leave it blank
                   (assoc
                    m :description
                    (format "%s/%s on Maven" group_name jar_name)))
        res (->> url curl-get-json :response :docs
                    (map #(some->
                           %
                           (clojure.set/rename-keys keys-m)
                           (select-keys (vals keys-m))
                           add-desc)))]
    (if (empty? res)
      (binding [*out* *err*]
        (println "Unable to find" search-term "on Maven."))
      res)))

(defn dep-search-clojars [search-term]
  (let [url (format
             "https://clojars.org/search?format=json&q=%s"
             (url-encode search-term))
        {search-results :results
         results-count :count} (curl-get-json url)]
    (if (zero? results-count)
      (binding [*out* *err*]
        (println "Unable to find" search-term "on Clojars."))
      search-results)))

(defn dep-search [{:keys [opts]}]
  (let [{:keys [search-term]} opts]
    (if (or (:help opts)
            (not (string? search-term))
            (str/blank? search-term))
      (print-dep-search-help)
      (let [search-results (->> [dep-search-maven
                                 dep-search-clojars]
                                (map #(% search-term))
                                (apply concat))]
        (when (empty? search-results) (System/exit 1))
        (doseq [search-result search-results]
          (prn :lib (symbol (:group_name search-result)
                            (:jar_name search-result))
               :version (:version search-result)
               :description (:description search-result)))))))

(defn git-url->lib [git-url]
  (when git-url
    (when (str/starts-with? git-url "https://github.com/")
      (-> (str/replace git-url "https://github.com/" "")
          symbol))))

(defn dep->upgrade
  "Given a lib (hiccup/hiccup) and a version ({:mvn/version \"1.0.4\"}), return an
  upgrade ({:mvn/version \"1.0.5\"}), otherwise return nil.

  Supports different kinds of coordinate formats.

    (dep->latest-stable {:lib 'hiccup/hiccup
                         :current {:mvn/version \"1.0.4\"}})
    ;; => {:mvn/version \"1.0.5\"}

    (dep->upgrade {:lib 'clj-kondo/clj-kondo :current {:git/sha \"247e538\"}})
    ;; => {:git/sha \"...\"}
  "
  [{:keys [lib current unstable]}]
  ;; for now, just upgrade to stable versions
  (let [current (set/rename-keys current {:sha :git/sha
                                          :tag :git/tag})]
    (cond
      ;; if there's a git tag, find the latest git tag.
      (:git/tag current)
      (let [lib (or (git-url->lib (:git/url current))
                    lib)]
        (when-let [tag (git/latest-github-tag lib)]
          (when (not= tag (:git/tag current))
            {:git/tag (:name tag)
             :git/sha (-> tag :commit :sha (subs 0 7))})))

      ;; if there's a git sha, find the latest git sha.
      (:git/sha current)
      (let [lib (or (git-url->lib (:git/url current))
                    lib)]
        (when-let [sha (git/latest-github-sha lib)]
          (when (not= sha (:git/sha current))
            {:git/sha sha})))

      ;; when :unstable is set, find the lastest version whatsoever
      unstable
      (when-let [version (or (first (clojars-versions lib {:limit 100}))
                             (first (mvn-versions lib {:limit 100})))]
        (let [v-older? (req-resolve 'version-clj.core/older?)]
          (when (v-older? (:mvn/version current) version)
            {:mvn/version version})))

      ;; if `current` is a stable maven/clojars version, find the latest stable
      ;; maven clojars dep
      (and (:mvn/version current) (stable-version? (:mvn/version current)))
      (when-let [version (or (first (filter stable-version? (clojars-versions lib {:limit 100})))
                             (first (filter stable-version? (mvn-versions lib {:limit 100}))))]
        ;; only upgrade to a newer version than the current. Useful when
        ;; developing a new version locally.
        (let [v-older? (req-resolve 'version-clj.core/older?)]
          (when (v-older? (:mvn/version current) version)
            {:mvn/version version})))

      ;; if `current` is an unstable maven/clojars version, preferrably upgrade
      ;; to a more recent stable version. Otherwise, look for a more recent
      ;; unstable version.
      ;;
      ;; Useful when depending on an unstable version under active development.
      (and (:mvn/version current)
           (not (stable-version? (:mvn/version current))))
      (let [clojars-candidates (clojars-versions lib {:limit 100})
            maven-candidates (mvn-versions lib {:limit 100})
            v-older? (req-resolve 'version-clj.core/older?)]
        (or
         ;; prefer a more recent stable
         (when-let [candidate (or (first (filter stable-version? clojars-candidates))
                                  (first (filter stable-version? maven-candidates)))]
           (when (v-older? (:mvn/version current) candidate)
             {:mvn/version candidate}))

         ;; otherwise, provide a more recent unstable
         (when-let [candidate (or (first clojars-candidates) (first maven-candidates))]
           (when (v-older? (:mvn/version current) candidate)
             candidate)))))))

         ;; otherwise, do nothing (fall through to nil)
         

(defn opts->specified-deps
  "Returns all :deps and :alias :extra-deps for the deps.edn indicated by `opts`."
  [opts]
  (let [lib                    (some-> opts :lib symbol)
        alias                  (some-> opts :alias)
        no-aliases?            (:no-aliases opts)
        {:keys [deps aliases]} (-> (edn-string opts) edn/read-string)
        current-deps
        (->> deps (map (fn [[lib current]]
                         (cond-> {:lib lib :current current}
                           (:unstable opts) (assoc :unstable true)))))
        alias-deps
        (if no-aliases? []
            (->> aliases (mapcat (fn [[alias def]]
                                   (->> (:extra-deps def)
                                        (map (fn [[lib current]]
                                               {:alias   alias
                                                :lib     lib
                                                :current current})))))))]
    (->> (concat current-deps alias-deps)
         (filter (fn [dep] (if alias (= alias (:alias dep)) true)))
         (filter (fn [dep] (if lib (= lib (:lib dep)) true))))))

(defn do-dep-upgrade
  "Updates the deps version in deps.edn for a single lib,
  as described in `dep-upgrade`, which is a map with
  `:lib`, `:current`, `:latest` (the current and latest dep coords), and optionally `:alias`.

  When `:current` and `:latest` do not match, `:latest` is written to deps.edn via `dep-add`.
  Supports `:dry-run` in the passed `opts` to instead just print the update."
  [opts {:keys [lib current latest alias] :as _dep-upgrade}]
  (let [{:keys [git/tag git/sha mvn/version]} latest
        log-args
        (concat (when alias [:alias alias]) [:lib lib]
                [:current-version ((some-fn :git/tag :git/sha :mvn/version) current)]
                (when tag [:tag tag])
                (when (and (not tag) sha) [:sha sha])
                (when version [:version version]))
        log-action (fn [action] (apply println :action (str "\"" action "\"") log-args))]
    (when (or (and tag (not= (:git/tag current) tag))
              (and sha (not= (:git/sha current) sha))
              (and version (not= (:mvn/version current) version)))
      (if (:dry-run opts)
        (log-action "upgrading (dry run)")
        (do (log-action "upgrading")
            (dep-add {:opts (cond-> opts
                              lib     (assoc :lib lib)
                              alias   (assoc :alias alias)
                              version (assoc :version version)
                              tag     (assoc :tag tag)
                              (and (not tag) sha) (assoc :sha sha))}))))))

(defn dep-upgrade [{:keys [opts]}]
  (when (or (:h opts) (:help opts))
    (println "Usage: neil dep upgrade [lib] [options]")
    (println "")
    (println "  neil dep upgrade [options]            Upgrade all libraries")
    (println "  neil dep upgrade LIB [options]        Upgrade a single library")
    (println "  neil dep upgrade --lib LIB [options]  Upgrade a single library")
    (println "")
    (println "Options:")
    (println "")
    (println (cli/format-opts
              {:spec spec
               :order [:lib :dry-run :alias :no-aliases]}))
    (println "")
    (println (str/trim "
Examples:

  neil dep upgrade                           ; upgrade all deps.
  neil dep upgrade --dry-run                 ; print deps that would be upgraded.
  neil dep upgrade --alias lint              ; update only deps for the `lint` alias.
  neil dep upgrade :lib clj-kondo/clj-kondo  ; update a single dep.
"))
    (System/exit 0))

  (let [lib           (some-> opts :lib symbol)
        alias         (some-> opts :alias)
        deps-to-check (opts->specified-deps opts)
        upgrades      (->> deps-to-check
                           (pmap (fn [dep] (merge dep {:latest (dep->upgrade dep)})))
                           ;; keep if :latest version was found
                           (filter (fn [dep] (some? (:latest dep)))))]
    (when lib
      ;; logging and early-exit when :lib is specified
      (cond (not (seq deps-to-check))
            (binding [*out* *err*]
              (if alias
                (println "Local dependency not found:" lib "for alias:" alias)
                (println "Local dependency not found:" lib))
              (println "Use `neil dep add` to add dependencies.")
              (System/exit 1))

            (not (seq upgrades))
            (binding [*out* *err*]
              ;; note this could also mean we've hit github's rate limit
              (println "No remote version found for" lib)
              (System/exit 1))))

    (doseq [dep-upgrade upgrades] (do-dep-upgrade opts dep-upgrade))))

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
  kaocha adds kaocha test runner to :kaocha alias.
  nrepl  adds nrepl server to :nrepl alias.

dep
  add: Adds --lib, a fully qualified symbol, to deps.edn :deps.
    Run `neil dep add --help` to see all options.

  search: Search Clojars for a string in any attribute of an artifact
    Run `neil dep search --help` to see all options.

  upgrade: Upgrade libs in the deps.edn file.
    Run `neil dep upgrade --help` to see all options.

  versions: List available versions of a library (Clojars libraries only)
    Run `neil dep versions -h` to see all options.

  update: Alias for `upgrade`.

license
  list   Lists commonly-used licenses available to be added to project. Takes an optional search string to filter results.
  search Alias for `list`
  add    Writes license text to a file
    Options:
    --license The key of the license to use (e.g. epl-1.0, mit, unlicense). --license option name may be elided when license key is provided as first argument.
    --file    The file to write. Defaults to 'LICENSE'.

new
  Create a project using deps-new
    Run `neil new --help` to see all options.

version
  Commands for managing the :version key in the deps.edn project config.
    Run `neil version --help` to see all options.

test
  Run tests. Assumes `neil add test`. Run `neil test --help` to see all options.
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

(defn neil-test [{:keys [opts]}]
  (neil-test/neil-test opts))

(defn exec [& opts]
  (println :opts opts)
  (println :exec)
  (let [repo-name (-> opts first :args first)
        ext-raw-url (format "https://raw.githubusercontent.com/%s/main/neil-ext.clj" repo-name)]
    (println :repo-name repo-name)
    (println :ext-raw-url ext-raw-url)
    (reset! *neil-opts* opts)
    (io/copy
     (:body (http/get ext-raw-url
                      {:as :stream}))
     (io/file "/tmp/neil-ext.clj"))
    (load-file "/tmp/neil-ext.clj")))

  


(defn -main [& _args]
  (cli/dispatch
   [{:cmds ["exec"] :fn exec :args->opts []}
    {:cmds ["add" "dep"] :fn dep-add :args->opts [:lib]}
    {:cmds ["add" "test"] :fn add-cognitect-test-runner}
    {:cmds ["add" "build"] :fn add-build}
    {:cmds ["add" "kaocha"] :fn add-kaocha}
    {:cmds ["add" "nrepl"] :fn add-nrepl}
    {:cmds ["dep" "versions"] :fn dep-versions :args->opts [:lib]}
    {:cmds ["dep" "add"] :fn dep-add :args->opts [:lib]}
    {:cmds ["dep" "search"] :fn dep-search :args->opts [:search-term]}
    {:cmds ["dep" "upgrade"] :fn dep-upgrade}
    {:cmds ["dep" "update"] :fn dep-upgrade} ;; supported as an alias
    {:cmds ["license" "list"] :fn license-search :args->opts [:search-term]}
    {:cmds ["license" "search"] :fn license-search :args->opts [:search-term]}
    {:cmds ["license" "add"] :fn add-license :args->opts [:license]}
    {:cmds ["new"] :fn new/run-deps-new
     :args->opts [:template :name :target-dir]
     :spec {:name {:coerce proj/coerce-project-name}}}
    {:cmds ["version" "tag"]
     :fn (partial neil-version/neil-version :tag)
     :aliases {:h :help}
     :spec neil-version/version-spec}
    {:cmds ["version" "set"]
     :fn (partial neil-version/neil-version :set)
     :args->opts [:version]
     :spec neil-version/version-spec
     :aliases {:h :help}}
    {:cmds ["version" "major"]
     :fn (partial neil-version/neil-version :major)
     :args->opts [:version]
     :spec neil-version/version-spec
     :aliases {:h :help}}
    {:cmds ["version" "minor"]
     :fn (partial neil-version/neil-version :minor)
     :args->opts [:version]
     :spec neil-version/version-spec
     :aliases {:h :help}}
    {:cmds ["version" "patch"]
     :fn (partial neil-version/neil-version :patch)
     :spec neil-version/version-spec
     :args->opts [:version]
     :aliases {:h :help}}
    {:cmds ["version"]
     :fn neil-version/neil-version
     :aliases {:h :help}
     :spec neil-version/version-spec}
    {:cmds ["help"] :fn print-help}
    {:cmds ["test"] :fn neil-test
     :spec neil-test/neil-test-spec
     :alias neil-test/neil-test-aliases}
    {:cmds []
     :spec {:version {:coerce :boolean}}
     :fn (fn [{:keys [opts] :as m}]
           (if (:version opts)
             (neil-version/print-version)
             (print-help m)))}]
   *command-line-args*
   {:spec spec
    :exec-args {:deps-file "deps.edn"}})
  nil)

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
