(ns babashka.neil.version-test
  (:require [babashka.neil.git :as git]
            [babashka.neil.meta :as meta]
            [babashka.neil.test-util :refer [neil set-deps-edn! reset-test-dir
                                             test-dir test-file]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (= "neil :version-not-set" out)))
  (with-redefs [meta/version "1.0.0"]
    (let [{:keys [out]} (neil "--version" :out :string)]
      (is (= "neil 1.0.0" out)))))

(deftest help-test
  (set-deps-edn! {})
  (doseq [cmd ["version --help"
               "version -h"]]
    (let [{:keys [out]} (neil cmd :out :string)]
      (is (str/starts-with? out "Usage: neil version [set|major|minor|patch] ")))))

(deftest root-test
  (with-redefs [meta/version "1.0.0"]
    (reset-test-dir)
    (testing "Project version is :version-not-set when not set"
      (set-deps-edn! {})
      (let [{:keys [out]} (neil "version" :out :edn)]
        (is (= {:neil "1.0.0" :project :version-not-set} out))))
    (testing "Fatal error when project version is not a string"
      (set-deps-edn! {:aliases {:neil {:project {:version {:major 2}}}}})
      (is (thrown-with-msg? ExceptionInfo #"Project version must be a string"
                            (neil "version" :out :edn))))
    (testing "Project version is returned as a string"
      (set-deps-edn! {:aliases {:neil {:project {:version "2.0.0"}}}})
      (let [{:keys [out]} (neil "version" :out :edn)]
        (is (= {:neil "1.0.0" :project "2.0.0"} out))))
    (testing "Project version can be any string"
      (doseq [v ["2021a3" "9.4.146.24-node.21" "1.1.1q+quic" "93" "70.1"]]
        (set-deps-edn! {:aliases {:neil {:project {:version v}}}})
        (let [{:keys [out]} (neil "version" :out :edn)]
          (is (= {:neil "1.0.0" :project v} out)))))))

(def git-opts
  {:dir test-dir
   :err :inherit})

(deftest tag-test
  (reset-test-dir)
  (set-deps-edn! {})
  (git/ensure-repo git-opts)
  (is (= 1 (git/commit-count git-opts)))
  (testing "Assert at least one staged file"
    (is (thrown-with-msg? ExceptionInfo #"Requires at least one staged file"
                          (neil "version tag" :out :string))))
  (testing "Assert all files staged"
    (set-deps-edn! {:deps {}})
    (spit (test-file "b") "b")
    (git/add ["b"] git-opts)
    (is (thrown-with-msg? ExceptionInfo #"Requires all files to be staged"
                          (neil "version tag" :out :string))))
  (testing "Create commit and tag"
    (let [v "0.1.0"]
      (set-deps-edn! {:aliases {:neil {:project {:version v}}}})
      (git/add ["deps.edn"] git-opts)
      (let [{:keys [out]} (neil "version tag")]
        (is (= (str "v" v) out)
            "Tag is printed as output")
        (is (= 2 (git/commit-count git-opts))
            "New commit created")
        (is (= (str "v" v) (git/describe git-opts))
            "Tag points to new commit")
        (is (= (str "v" v) (git/show git-opts))
            "Latest commit message is same as version")
        (is (= (str "v" v) (git/tag-contents (str "v" v) git-opts))
            "Latest annotated tag message is same as version")))))

(comment
  (clojure.test/run-tests))
