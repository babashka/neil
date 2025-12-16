(ns babashka.neil.new
  {:no-doc true}
  (:require
   [babashka.fs :as fs]
   [babashka.neil.git :as git]
   [babashka.neil.project :as proj]
   [babashka.neil.utils :refer [req-resolve]]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn- built-in-template?
  "Returns true if the template name maps to a function in org.corfield.new."
  [template]
  (contains? (set (map name (keys (ns-publics 'org.corfield.new)))) template))

(defn- built-in-templates []
  (require 'org.corfield.new)
  (let [non-templates #{'create}]
    (->> (ns-publics 'org.corfield.new)
         (remove (fn [[sym _]] (contains? non-templates sym)))
         (into {}))))

(defn- github-repo-http-url [lib]
  (str "https://github.com/" (git/clean-github-lib lib)))

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
           tag (git/latest-github-tag (git-url->lib-sym url))]
       (if tag
         {lib-sym {:git/url url :git/tag (:name tag) :git/sha (-> tag :commit :sha)}}
         (let [sha (git/latest-github-sha (git-url->lib-sym url))]
           {lib-sym {:git/url url :git/sha sha}}))))

   [#{:git/tag} #{:git/url :git/tag}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           tag (:git/tag lib-opts)
           {:keys [commit]} (git/find-github-tag (git-url->lib-sym url) tag)]
       {lib-sym {:git/url url :git/tag tag :git/sha (:sha commit)}}))

   [#{:git/sha} #{:git/url :git/sha}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           sha (:git/sha lib-opts)]
       {lib-sym {:git/url url :git/sha sha}}))

   [#{:latest-sha} #{:git/url :latest-sha}]
   (fn [lib-sym lib-opts]
     (let [url (or (:git/url lib-opts) (github-repo-http-url lib-sym))
           sha (git/latest-github-sha (git-url->lib-sym url))]
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

(def bb? (System/getProperty "babashka.version"))

(def create-opts-deny-list
  [:deps-file :dry-run :git/sha :git/url :latest-sha :local/root :sha])

(defn- cli-opts->create-opts
  "Returns options for org.corfield.new/create based on the cli-opts.

  If no template is provided, the scratch template is filled in.

  When using the scratch template, we also provide the :scratch create-opt as
  a default. This changes the default scratch.clj file and namespace to match
  the :name option instead."
  [cli-opts]
  (let [no-template (not (:template cli-opts))
        scratch-template (= (str (:template cli-opts)) "scratch")]
    (merge (when (or no-template scratch-template)
             {:template "scratch"
              :scratch (:name cli-opts)})
           (apply dissoc cli-opts create-opts-deny-list))))

(defn- deps-new-plan
  "Returns a plan for calling org.corfield.new/create.

  :template-deps - These deps will be added with babashka.deps/add-deps before
                   calling the create function.

  :create-opts   - This map contains the options that will be passed to the
                   create function."
  [cli-opts]
  (let [create-opts (cli-opts->create-opts cli-opts)
        tpl-deps (when (and bb? (not (built-in-template? (:template create-opts))))
                   (template-deps (:template create-opts) cli-opts))]
    (merge (when tpl-deps {:template-deps tpl-deps})
           {:create-opts create-opts})))

(defn- deps-new-create [create-opts]
  ((req-resolve 'org.corfield.new/create) create-opts))

(defn print-new-help []
  (println
   (str/join "\n\n"
             (map str/trim
                  ["
Usage: neil new [template] [name] [target-dir] [options]

Runs the org.corfield.new/create function from deps-new.

All of the deps-new options can be provided as CLI options:

https://github.com/seancorfield/deps-new/blob/develop/doc/options.md

Both built-in and external templates are supported. Built-in templates use
unqualified names (e.g. scratch) whereas external templates use fully-qualified
names (e.g. io.github.kit/kit-clj)."
                   (str
                    "The provided built-in templates are:\n\n"
                    (str/join "\n" (for [sym (sort (keys (built-in-templates)))]
                                     (str "    " sym))))
                   "
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
    default branch of :git/url.

Examples:
  neil new scratch foo --overwrite
  neil new io.github.rads/neil-new-test-template foo2 --latest-sha"]))))

(defn- deps-new-set-classpath
  "Sets the java.class.path property.

  This is required by org.corfield.new/create. In Clojure it's set by default,
  but in Babashka it must be set explicitly."
  []
  (let [classpath ((req-resolve 'babashka.classpath/get-classpath))]
    (System/setProperty "java.class.path" classpath)))

(defn- deps-new-add-template-deps
  "Adds template deps at runtime."
  [template-deps]
  ((req-resolve 'babashka.deps/add-deps) {:deps template-deps}))

(defn run-deps-new
  "Runs org.corfield.new/create using the provided CLI options.

  To support the dry run feature, side-effects should be described in the plan
  provided by deps-new-plan. This function's job is to execute side-effects
  using the plan to provide repeatability."
  [{:keys [opts]}]
  (if (or (:help opts) (not (:name opts)))
    (print-new-help)
    (do
      (require 'org.corfield.new)
      (let [plan (deps-new-plan opts)
            {:keys [template-deps create-opts]} plan
            project-name (symbol (:name create-opts))
            dir (or (:target-dir create-opts)
                    (name project-name))]
        (if (:dry-run opts)
          (do (prn plan) nil)
          (do
            (when template-deps (deps-new-add-template-deps template-deps))
            (when bb? (deps-new-set-classpath))
            (deps-new-create create-opts)
            (when (fs/exists? (proj/resolve-deps-file dir (:deps-file opts)))
              (proj/assoc-project-meta! (assoc opts :dir dir :k :name :v project-name)))))))))
