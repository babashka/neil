(ns babashka.neil.version-test
  (:require [babashka.fs :as fs]
            [babashka.neil.version :as version]
            [babashka.neil.test-util :refer [neil test-file test-dir
                                             ensure-git-repo]]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (re-seq #"^neil \d+\.\d+\.\d+(-\w+)?\n$" out))))

(deftest root-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (let [{:keys [out]} (neil "version")]
    (is (nil? out))))

(defn read-deps-edn-version []
  (get-in (edn/read-string (slurp (test-file "deps.edn")))
          version/version-path))

(deftest bump-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (ensure-git-repo)
  (let [{:keys [out]} (neil "version minor")
        v {:major 0 :minor 1 :patch 0}]
    (is (= v out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version patch")
        v {:major 0 :minor 1 :patch 1}]
    (is (= v out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version minor 3")
        v {:major 0 :minor 3 :patch 0}]
    (is (= v out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version major")
        v {:major 1 :minor 0 :patch 0}]
    (is (= v out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version")
        v {:major 1 :minor 0 :patch 0}]
    (is (= v out))
    (is (= v (read-deps-edn-version)))))

(comment
  (clojure.test/run-tests))
