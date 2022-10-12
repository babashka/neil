(ns babashka.neil.dep-upgrade-test
  (:require
   [babashka.neil.test-util :as test-util]
   [clojure.test :as t :refer [deftest is testing]]
   [clojure.edn :as edn]))

(def test-file-path (str (test-util/test-file "deps.edn")))
(defn get-dep-version [dep-name]
  (-> test-file-path slurp edn/read-string :deps (get dep-name)))

(deftest dep-upgrade-test
  (testing "a fresh project is up-to-date"
    (spit test-file-path "{}")
    (test-util/neil "dep add :lib clj-kondo/clj-kondo" :deps-file test-file-path)
    (let [clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)]
      (test-util/neil "dep upgrade" :deps-file test-file-path)
      (is (= clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)))))

  (testing "an old dependency can be upgraded, and --dry-run is respected"
    (spit test-file-path "{}")
    (test-util/neil "dep add :lib clj-kondo/clj-kondo :version 2022.01.01" :deps-file test-file-path)
    (let [clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)]

      ;; should be the same version after --dry-run
      (test-util/neil "dep upgrade" :deps-file test-file-path :dry-run true)
      (is (= clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)))

      ;; after a non-dry-run, the version should be changed
      (test-util/neil "dep upgrade" :deps-file test-file-path)
      (is (not (= clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo)))))))

(deftest dep-upgrade-test-one-lib
  (testing "specifying :lib only updates one dep"
    (spit test-file-path "{}")
    (test-util/neil "dep add :lib clj-kondo/clj-kondo :version 2022.01.01" :deps-file test-file-path)
    (test-util/neil "dep add :lib babashka/fs :version 0.0.1" :deps-file test-file-path)
    (let [clj-kondo-original (get-dep-version 'clj-kondo/clj-kondo)
          fs-original        (get-dep-version 'babashka/fs)]
      (test-util/neil "dep upgrade :lib clj-kondo/clj-kondo" :deps-file test-file-path)
      (let [clj-kondo-upgraded (get-dep-version 'clj-kondo/clj-kondo)
            fs-upgraded        (get-dep-version 'babashka/fs)]
        (is (not (= clj-kondo-original clj-kondo-upgraded)))
        (is (= fs-original fs-upgraded))))))

(deftest dep-upgrade-test-maintain-vcs
  (testing "upgrading a :git/sha dep should maintain :git/sha"
    (spit test-file-path "{}")
    (test-util/neil "dep add :lib clj-kondo/clj-kondo :latest-sha true" :deps-file test-file-path)
    (let [clj-kondo-original (get-dep-version 'clj-kondo/clj-kondo)]
      ;; this upgrade should return the same latest sha, NOT a :mvn/version
      (test-util/neil "dep upgrade" :deps-file test-file-path)
      (let [{:keys [git/url git/sha]} (get-dep-version 'clj-kondo/clj-kondo)]
        (is url)
        (is sha)
        (is (= clj-kondo-original (get-dep-version 'clj-kondo/clj-kondo))))))

  (testing "upgrading an older :git/sha dep should set the latest :git/sha"
    (spit test-file-path "{}")
    (test-util/neil "dep add :lib clj-kondo/clj-kondo :sha 6ffc3934cb83d2c4fff16d84198c73b40cd8a078"
                    :deps-file test-file-path)
    (let [clj-kondo-original (get-dep-version 'clj-kondo/clj-kondo)]
      ;; here we upgrade and then assert that the sha is different, but still :git/sha based
      (test-util/neil "dep upgrade" :deps-file test-file-path)
      (let [{:keys [git/url git/sha]} (get-dep-version 'clj-kondo/clj-kondo)]
        (is url)
        (is sha)
        ;; should be a different sha
        (is (not (= sha (:git/sha clj-kondo-original)))))))
  )
