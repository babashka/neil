(ns babashka.neil.version-test
  (:require [babashka.neil.meta :as meta]
            [babashka.neil.test-util :refer [neil set-deps-edn! reset-test-dir]]
            [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (= "neil :version-not-set" out)))
  (with-redefs [meta/version "1.0.0"]
    (let [{:keys [out]} (neil "--version" :out :string)]
      (is (= "neil 1.0.0" out)))))

(deftest v1-test
  (let [{:keys [out]} (neil "version" :out :string)]
    (is (= "neil :version-not-set" out)))
  (with-redefs [meta/version "1.0.0"]
    (let [{:keys [out]} (neil "version" :out :string)]
      (is (= "neil 1.0.0" out)))))

(deftest root-test
  (with-redefs [meta/version "1.0.0"]
    (reset-test-dir)
    (testing "Project version is :version-not-set when not set"
      (set-deps-edn! {})
      (let [{:keys [out]} (neil "version --v2" :out :edn)]
        (is (= {:neil "1.0.0" :project :version-not-set} out))))
    (testing "Fatal error when project version is not a string"
      (set-deps-edn! {:aliases {:neil {:project {:version {:major 2}}}}})
      (is (thrown-with-msg? ExceptionInfo #"Project version must be a string"
                            (neil "version --v2" :out :edn))))
    (testing "Project version is returned as a string"
      (set-deps-edn! {:aliases {:neil {:project {:version "2.0.0"}}}})
      (let [{:keys [out]} (neil "version --v2" :out :edn)]
        (is (= {:neil "1.0.0" :project "2.0.0"} out))))
    (testing "Project version can be any string"
      (doseq [v ["2021a3" "9.4.146.24-node.21" "1.1.1q+quic" "93" "70.1"]]
        (set-deps-edn! {:aliases {:neil {:project {:version v}}}})
        (let [{:keys [out]} (neil "version --v2" :out :edn)]
          (is (= {:neil "1.0.0" :project v} out)))))))

(comment
  (clojure.test/run-tests))
