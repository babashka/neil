(ns babashka.neil.test-util
  (:require [babashka.fs :as fs]
            [babashka.neil :as neil-main]
            [babashka.process :as process]
            [babashka.tasks :as tasks]
            [clojure.edn :as edn]))

(defn test-file [name]
  (doto (fs/file (fs/temp-dir) "neil" name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn neil [cli-args & {:keys [out] :or {out :edn}}]
  (let [deps-file (str (test-file "deps.edn"))
        cli-args' (concat (process/tokenize cli-args) [:deps-file deps-file])]
    (binding [*command-line-args* cli-args']
      (let [s (with-out-str (tasks/exec `neil-main/-main))]
        {:out (if (#{:edn} out) (edn/read-string s) s)}))))
