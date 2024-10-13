(ns babashka.neil.project-test
  (:require [babashka.fs :as fs]
            [babashka.neil.project :refer [assoc-project-meta!]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest assoc-project-meta!-test
  (let [deps-dir (-> (fs/create-temp-dir {:prefix "neil"}) .toFile)
        _        (.deleteOnExit deps-dir)]
    (testing "when deps file exists, k/v gets added"
      (let [deps-file-name "deps.edn" 
            deps-file (fs/file deps-dir deps-file-name)
            _ (.deleteOnExit deps-file)
            ; create file to be updated
            _ (spit deps-file "{}")]
        (assoc-project-meta! {:dir deps-dir :deps-file deps-file-name
                              :k :test :v :foo})
        (is (every? #(str/includes? (slurp deps-file) %) [":test" ":foo"]))))
    (testing "when deps file doesn't exist, no action is taken"
      (assoc-project-meta! {:dir deps-dir :deps-file "missing" :k :test :v :foo})
      (is (not (fs/exists? (fs/file deps-dir "missing")))))))
