(ns babashka.neil.git
  {:no-doc true}
  (:require [babashka.fs :as fs]
            [babashka.neil.curl :refer [curl-get-json]]
            [babashka.process :refer [sh]]
            [clojure.string :as str]))

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

(defn parse-status [line]
  (let [[[_ status path]] (re-seq #"^(.{2}) (.+)$" line)]
    {:status status :path path}))

(defn status [git-opts]
  (let [cmd "git status --porcelain=v1"
        {:keys [out err]} (sh cmd git-opts)]
    (if-not (str/blank? err)
      {:err err}
      {:statuses (if (str/blank? out)
                   '()
                   (map parse-status (str/split-lines out)))})))

(defn commit [message git-opts]
  (let [cmd ["git" "commit" "-m" message]
        {:keys [err]} (sh cmd git-opts)]
    (when (seq err) (throw (ex-info err {})))))

(defn tag [message git-opts]
  (let [cmd ["git" "tag" "-a" message "-m" message]
        {:keys [err]} (sh cmd git-opts)]
    (when (seq err) (throw (ex-info err {})))))

(defn unstaged-files [status-result]
  (->> (:statuses status-result)
       (filter #(re-seq #"^ " (:status %)))
       (map :path)))

(defn staged-files [status-result]
  (->> (:statuses status-result)
       (filter #(re-seq #"^[ARMD]" (:status %)))
       (map :path)))

(defn resolve-repo [deps-file]
  (if deps-file
    (fs/file (fs/parent deps-file) ".git")
    (fs/file ".git")))

(defn repo? [deps-file]
  (fs/exists? (resolve-repo deps-file)))

(defn commit-count [git-opts]
  (-> (sh "git rev-list --count HEAD" git-opts)
      :out
      str/trim
      Integer/parseInt))

(defn ensure-repo [git-opts]
  (sh "git init -b main" git-opts)
  (sh "git config user.name 'Neil Tests'" git-opts)
  (sh "git config user.email '<>'" git-opts)
  (sh "git add ." git-opts)
  (sh "git commit -m 'First commit'" git-opts))

(defn tag-contents [tag git-opts]
  (let [cmd ["git" "for-each-ref" (str "refs/tags/" tag)
             "--format" "%(contents)"]
        {:keys [out]} (sh cmd git-opts)]
    (not-empty (str/trim out))))

(defn add [files git-opts]
  (sh (concat ["git" "add"] files) git-opts))

(defn describe [git-opts]
  (not-empty (str/trim (:out (sh ["git" "describe"] git-opts)))))

(defn show [git-opts]
  (not-empty (str/trim (:out (sh ["git" "show" "-s" "--format=%B"] git-opts)))))
