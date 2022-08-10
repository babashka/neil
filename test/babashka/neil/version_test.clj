(ns babashka.neil.version-test
  (:require [babashka.fs :as fs]
            [babashka.neil.version :as version]
            [babashka.neil.test-util
             :refer [neil test-file test-dir ensure-git-repo git-commit-count
                     git-add git-tag git-describe git-show]]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (re-seq #"^neil \d+\.\d+\.\d+(-\w+)?$" out))))

(deftest root-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (let [{:keys [out]} (neil "version")]
    (is (= "nil" out))))

(defn read-deps-edn-version []
  (-> (edn/read-string (slurp (test-file "deps.edn")))
      (get-in [:aliases :neil :project :version])))

(deftest bump-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (ensure-git-repo)
  (let [{:keys [out]} (neil "version minor")
        v "0.1.0"]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version patch")
        v "0.1.1"]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version minor 3")
        v "0.3.0"]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version major")
        v "1.0.0"]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version))))
  (let [{:keys [out]} (neil "version")
        v "1.0.0"]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version)))))

(deftest tag-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (ensure-git-repo)
  (is (= 1 (git-commit-count)))
  (testing "Assert at least one staged file"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Requires at least one staged file"
                          (neil "version tag"))))
  (testing "Assert all files staged"
    (neil "version minor --no-tag")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Requires all files to be staged"
                          (neil "version tag"))))
  (testing "Create commit and tag"
    (git-add ".")
    (neil "version tag")
    (let [v "v0.1.0"]
      (is (= 2 (git-commit-count))
          "New commit created")
      (is (= v (git-describe))
          "Tag points to new commit")
      (is (= v (git-show))
          "Latest commit message is same as version")
      (is (= v (git-tag v))
          "Latest annotated tag message is same as version"))))

(deftest set-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (ensure-git-repo)
  (let [{:keys [out]} (neil "version minor")
        v "0.1.0"]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version))))
  (let [v "2022.8.1"
        {:keys [out]} (neil ["version" "set" v])]
    (is (= (str "v" v) out))
    (is (= v (read-deps-edn-version)))))

(comment
  (clojure.test/run-tests))
