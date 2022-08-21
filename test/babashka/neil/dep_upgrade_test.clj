(ns babashka.neil.dep-upgrade-test
  (:require
   [babashka.neil.test-util :as test-util]
   [clojure.string :as str]
   [babashka.process :refer [process check]]
   [clojure.test :as t :refer [deftest is testing]]))

(deftest dep-upgrade-test
  (is (= 1 1))

  (testing "can run without feiling on a small deps.edn file"
    (let [tmp-file (test-util/test-file "deps.edn")
          _ (test-util/neil "dep add :lib clj-kondo/clj-kondo" :deps-file tmp-file)
          dep-upgrade-report (with-out-str (test-util/neil "dep upgrade" :deps-file tmp-file :dry-run true))]
      (is (str/blank? (str/trim dep-upgrade-report)))))

  #_

  (testing "When adding a fresh dependency, there are no available upgrades"
    (let [tmp-file (test-util/test-file "deps.edn")
          _ (prn tmp-file)
          _ 1
          _ (test-util/neil "dep add :lib clj-kondo/clj-kondo" :deps-file tmp-file)
          _ (-> (process '[cat "/tmp/neil/deps.edn"] {:out :string}) check :out prn)
          #_#_
          dep-upgrade-report (with-out-str (test-util/neil "dep upgrade" :deps-file tmp-file :dry-run))
          dep-upgrade-report (with-out-str (test-util/neil "dep upgrade"   ))
          _ (prn dep-upgrade-report)
          ]
      #_
      (is (str/blank? (str/trim dep-upgrade-report)))))

  #_
  (testing "There are available updates to old versions of babashka/fs"
    (let [tmp-file (test-util/test-file "deps.edn")
          _ (test-util/neil "add dep :lib babashka/fs :version 0.1.2" :deps-file tmp-file)
          dep-upgrade-report (with-out-str (test-util/neil "dep upgrade --dry-run" :deps-file tmp-file))]
      (is (not (str/blank? (str/trim dep-upgrade-report)))))))
