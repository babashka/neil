(ns babashka.neil.version-test
  (:require [babashka.neil.git :as git]
            [babashka.neil.meta :as meta]
            [babashka.neil.version :as version]
            [babashka.neil.test-util :refer [neil read-deps-edn set-deps-edn!
                                             reset-test-dir test-dir test-file]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(defn- read-version-string []
  (version/deps-edn->project-version-string (read-deps-edn)))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (= "neil :version-not-set" out)))
  (with-redefs [meta/version "1.0.0"]
    (let [{:keys [out]} (neil "--version" :out :string)]
      (is (= "neil 1.0.0" out)))))

(deftest help-test
  (set-deps-edn! {})
  (doseq [cmd ["version --help"
               "version -h"
               "version set -h"
               "version major -h"
               "version minor -h"
               "version patch -h"]]
    (let [{:keys [out]} (neil cmd :out :string)]
      (is (str/starts-with? out "Usage: neil version")))))

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

(deftest set-test
  (reset-test-dir)
  (set-deps-edn! {:aliases {:neil {:project {:version "1.0.0-alpha2"}}}})
  (git/ensure-repo git-opts)
  (testing "Update deps.edn file with SemVer version"
    (let [v "2022.8.1"
          {:keys [out]} (neil ["version" "set" v] :out :string)]
      (is (= v (read-version-string))
          "Version is updated in deps.edn")
      (is (= (str "v" v) out)
          "Tag is printed as output")
      (is (= 2 (git/commit-count git-opts))
          "New commit created")
      (is (= (str "v" v) (git/describe git-opts))
          "Tag points to new commit")
      (is (= (str "v" v) (git/show git-opts))
          "Latest commit message is same as version")
      (is (= (str "v" v) (git/tag-contents (str "v" v) git-opts))
          "Latest annotated tag message is same as version")))
  (testing "Update deps.edn file with raw string version"
    (let [v "2021a3"
          {:keys [out]} (neil ["version" "set" v] :out :string)]
      (is (= v (read-version-string))
          "Version is updated in deps.edn")
      (is (= v out)
          "Tag is printed as output")
      (is (= 3 (git/commit-count git-opts))
          "New commit created")
      (is (= v (git/describe git-opts))
          "Tag points to new commit")
      (is (= v (git/show git-opts))
          "Latest commit message is same as version")
      (is (= v (git/tag-contents v git-opts))
          "Latest annotated tag message is same as version")))
  (testing "No commit or tag when --no-tag is set"
    (let [prev-v (read-version-string)
          prev-commit-count (git/commit-count git-opts)
          prev-tag-count (count (git/list-tags git-opts))]
      (doseq [args [[":no-tag"]
                    [":no-git-tag-version"]
                    [":tag" "false"]
                    [":git-tag-version" "false"]]]
        (set-deps-edn! {:aliases {:neil {:project {:version prev-v}}}})
        (let [next-v "2021a4"
              {:keys [out]} (neil (concat ["version" "set" next-v] args) :out :string)]
          (is (= next-v (read-version-string))
              "Version is updated in deps.edn")
          (is (= next-v out)
              "Version is printed as output")
          (is (= prev-commit-count (git/commit-count git-opts))
              "No commit created")
          (is (= prev-tag-count (count (git/list-tags git-opts)))
              "No tag created"))))))

(deftest bump-test
  (reset-test-dir)
  (set-deps-edn! {})
  (git/ensure-repo git-opts)
  (testing "Set initial version when no version in deps.edn"
    (let [{:keys [out]} (neil "version minor 4 --no-tag" :out :string)
          v "0.4.0"]
      (is (= (str "v" v) out))
      (is (= v (read-version-string))))
    (set-deps-edn! {})
    (let [{:keys [out]} (neil "version minor" :out :string)
          v "0.1.0"]
      (is (= (str "v" v) out))
      (is (= v (read-version-string)))))
  (testing "Bump patch version"
    (let [{:keys [out]} (neil "version patch" :out :string)
          v "0.1.1"]
      (is (= (str "v" v) out))
      (is (= v (read-version-string)))))
  (testing "Set minor version"
    (let [{:keys [out]} (neil "version minor 3" :out :string)
          v "0.3.0"]
      (is (= (str "v" v) out))
      (is (= v (read-version-string)))))
  (testing "Bump major version"
    (let [{:keys [out]} (neil "version major" :out :string)
          v "1.0.0"]
      (is (= (str "v" v) out))
      (is (= v (read-version-string)))))
  (let [{:keys [out]} (neil "version" :out :edn)]
    (is (= "1.0.0" (:project out)))))

(deftest version-map->str-test
  (doseq [v ["0.0.10-dev" "0.0.10+build" "0.0.10-dev+build"]]
    (let [version-map (version/str->version-map v)]
      (is (= v (version/version-map->str version-map))))))

(comment
  (clojure.test/run-tests))
