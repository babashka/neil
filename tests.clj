(ns tests
  (:require
   [babashka.fs :as fs]
   [babashka.process :refer [tokenize]]
   [babashka.tasks :as tasks]
   [clojure.edn :as edn]
   [clojure.test :as t :refer [deftest is]]))

(load-file "neil")

(defn neil [arg & args]
  (let [tmp-file (doto (fs/file (fs/temp-dir) "neil"  "deps.edn")
                   (-> fs/parent (fs/create-dirs))
                   (fs/delete-on-exit))]
    (apply tasks/shell "./neil"
           (concat (tokenize arg) [:deps-file tmp-file] args))
    (let [s (slurp tmp-file)]
      {:raw s
       :edn (edn/read-string s)})))

(deftest add-dep-test
  (let [{:keys [edn]} (neil "add dep clj-kondo/clj-kondo")]
    (is (-> edn :deps (get 'clj-kondo/clj-kondo)))))

(when (= *file* (System/getProperty "babashka.file"))
  (t/run-tests *ns*))
