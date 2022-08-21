(ns babashka.neil.dep-upgrade-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :refer [tokenize]]
   [babashka.tasks :as tasks]
   [clojure.edn :as edn]
   [babashka.neil.test-util :as test-util]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is testing]]))

(defn test-file [name]
  (doto (fs/file (fs/temp-dir) "neil" name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(deftest dep-upgrade-test
  (testing "When adding a fresh dependency, there are no available upgrades"
    (let [tmp-file (test-file "deps.edn")
          _ (test-util/neil "dep add :lib clj-kondo/clj-kondo" :deps-file tmp-file)
          dep-upgrade-report (with-out-str (neil "dep upgrade" :deps-file tmp-file :dry-run))]
      (is (str/blank? (str/trim dep-upgrade-report)))))

  (testing "There are available updates to old versions of babashka/fs"
    (let [tmp-file (test-file "deps.edn")
          _ (test-util/neil "add dep :lib babashka/fs :version 0.1.2" :deps-file tmp-file)
          dep-upgrade-report (with-out-str (neil "dep upgrade --dry-run" :deps-file tmp-file))]
      (is (not (str/blank? (str/trim dep-upgrade-report)))))))
