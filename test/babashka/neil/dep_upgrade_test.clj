(ns babashka.neil.dep-upgrade-test
  (:require
   [babashka.neil.test-util :as test-util]
   [clojure.test :as t :refer [deftest is testing]]
   [clojure.edn :as edn]))

(def test-file-path (str (test-util/test-file "deps.edn")))
(defn get-dep-version [dep-name]
  (-> test-file-path slurp edn/read-string
      :deps (get dep-name) :mvn/version))

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
      (is (not (= clj-kondo-version-original (get-dep-version 'clj-kondo/clj-kondo))))))

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
