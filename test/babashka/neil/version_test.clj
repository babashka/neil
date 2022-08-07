(ns babashka.neil.version-test
  (:require [babashka.fs :as fs]
            [babashka.neil.test-util :refer [neil test-file test-dir
                                             ensure-git-repo]]
            [clojure.test :refer [deftest is]]))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (re-seq #"^neil \d+\.\d+\.\d+(-\w+)?\n$" out))))

(deftest root-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (let [{:keys [out]} (neil "version")]
    (is (nil? out))))

(deftest bump-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (ensure-git-repo)
  (let [{:keys [out]} (neil "version minor")]
    (is (= {:major 0 :minor 1 :patch 0} out)))
  (let [{:keys [out]} (neil "version patch")]
    (is (= {:major 0 :minor 1 :patch 1} out)))
  (let [{:keys [out]} (neil "version minor 3")]
    (is (= {:major 0 :minor 3 :patch 0} out)))
  (let [{:keys [out]} (neil "version major")]
    (is (= {:major 1 :minor 0 :patch 0} out)))
  (let [{:keys [out]} (neil "version")]
    (is (= {:major 1 :minor 0 :patch 0} out))))

(comment
  (clojure.test/run-tests))
