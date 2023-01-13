(ns babashka.neil.test-util
  (:require [babashka.fs :as fs]
            [babashka.neil :as neil-main]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def test-dir (str (fs/file (fs/temp-dir) "neil")))

(defn reset-test-dir []
  (fs/delete-tree test-dir))

(defn test-file [name]
  (doto (fs/file test-dir name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn read-deps-edn []
  (edn/read-string (slurp (test-file "deps.edn"))))

(defn set-deps-edn! [x]
  (spit (test-file "deps.edn") (pr-str x)))

(defn neil [cli-args & {:keys [deps-file dry-run out] :or {out :string}}]
  (let [backup-deps-file (str (test-file "deps.edn"))
        cli-args'        (concat (if (string? cli-args)
                                   (process/tokenize cli-args)
                                   cli-args)
                                 [:deps-file (or deps-file backup-deps-file)]
                                 (when dry-run [:dry-run "true"]))
        cli-args' (mapv str cli-args')]
    (binding [*command-line-args* cli-args']
      (let [s (with-out-str (apply neil-main/-main cli-args'))]
        {:out (if (#{:edn} out) (edn/read-string s) (str/trim s))}))))
