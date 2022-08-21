(ns babashka.neil.dep-upgrade-test
  (:require
   [babashka.neil.test-util :as test-util]
   [clojure.test :as t :refer [deftest is testing]]
   [clojure.edn :as edn]))

(deftest dep-upgrade-test
  (let [test-file-path (str (test-util/test-file "deps.edn"))
        get-dep-version (fn [dep-name] (-> test-file-path slurp edn/read-string
                                           :deps (get dep-name) :mvn/version))]

    (testing "a fresh project is up-to-date"
      (spit test-file-path "{}")
      (test-util/neil "dep add :lib clj-kondo/clj-kondo" :deps-file test-file-path)
      (let [clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)
            _ (test-util/neil "dep upgrade" :deps-file test-file-path :dry-run true)
            clj-kondo-version-upgraded (get-dep-version 'clj-kondo/clj-kondo)]
        (is (= clj-kondo-version-original clj-kondo-version-upgraded))))

    (testing "an old dependency can be upgraded"
      (spit test-file-path "{}")
      (test-util/neil "dep add :lib clj-kondo/clj-kondo :version 2022.01.01" :deps-file test-file-path)
      (let [clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)
            _ (test-util/neil "dep upgrade" :deps-file test-file-path :dry-run true)
            clj-kondo-version-upgraded (get-dep-version 'clj-kondo/clj-kondo)]
        (is (= clj-kondo-version-original clj-kondo-version-upgraded))))))
