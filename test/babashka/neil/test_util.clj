(ns babashka.neil.test-util
  (:require [babashka.fs :as fs]
            [babashka.neil :as neil-main]
            [babashka.process :refer [sh] :as process]
            [babashka.tasks :as tasks]
            [clojure.edn :as edn]))

(def test-dir (str (fs/file (fs/temp-dir) "neil")))

(defn test-file [name]
  (doto (fs/file test-dir name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn ensure-git-repo []
  (sh "git init" {:dir test-dir})
  (sh "git add ." {:dir test-dir})
  (sh "git commit -m 'First commit'" {:dir test-dir}))

(defn neil [cli-args & {:keys [out] :or {out :edn}}]
  (let [deps-file (str (test-file "deps.edn"))
        cli-args' (concat (process/tokenize cli-args) [:deps-file deps-file])]
    (binding [*command-line-args* cli-args']
      (let [s (with-out-str (tasks/exec `neil-main/-main))]
        {:out (if (#{:edn} out) (edn/read-string s) s)}))))
