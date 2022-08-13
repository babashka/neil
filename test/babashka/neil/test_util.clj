(ns babashka.neil.test-util
  (:require [babashka.fs :as fs]
            [babashka.neil :as neil-main]
            [babashka.process :as process]
            [babashka.tasks :as tasks]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def test-dir (str (fs/file (fs/temp-dir) "neil")))

(defn reset-test-dir []
  (fs/delete-tree test-dir))

(defn test-file [name]
  (doto (fs/file test-dir name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn set-deps-edn! [x]
  (spit (test-file "deps.edn") (pr-str x)))

(defn neil [cli-args & {:keys [out] :or {out :string}}]
  (let [deps-file (str (test-file "deps.edn"))
        cli-args' (concat (if (string? cli-args)
                            (process/tokenize cli-args)
                            cli-args)
                          [:deps-file deps-file])]
    (binding [*command-line-args* cli-args']
      (let [s (with-out-str (tasks/exec `neil-main/-main))]
        {:out (if (#{:edn} out) (edn/read-string s) (str/trim s))}))))
