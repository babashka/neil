(ns babashka.neil.dep-upgrade-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :refer [tokenize]]
   [babashka.tasks :as tasks]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is testing]]))

(defn test-file [name]
  (doto (fs/file (fs/temp-dir) "neil" name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn neil [arg & args]
  (let [tmp-file (test-file "deps.edn")]
    (apply tasks/shell "./neil"
           (concat (tokenize arg) [:deps-file tmp-file] args))
    (let [s (slurp tmp-file)]
      {:raw s
       :edn (edn/read-string s)})))

(deftest dep-upgrade-test
  (testing "When adding a fresh dependency, there are no available upgrades"
    (let [tmp-file (test-file "deps.edn")
          _ (neil "dep add :lib clj-kondo/clj-kondo" :deps-file tmp-file)
          dep-upgrade-report (with-out-str (neil "dep upgrade" :deps-file tmp-file :dry-run))]
      (is (str/blank? (str/trim dep-upgrade-report)))))

  (testing "There are available updates to old versions of babashka/fs"
    (let [tmp-file (test-file "deps.edn")
          _ (neil "add dep :lib babashka/fs :version 0.1.2" :deps-file tmp-file)
          dep-upgrade-report (with-out-str (neil "dep upgrade --dry-run" :deps-file tmp-file))]
      (is (not (str/blank? (str/trim dep-upgrade-report)))))))
