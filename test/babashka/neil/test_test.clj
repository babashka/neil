(ns babashka.neil.test-test
  (:require
   [babashka.cli :as cli]
   [babashka.neil.test :as neil-test]
   [clojure.test :refer [deftest is testing]]))

(defn- parse [args]
  (cli/parse-opts args {:spec neil-test/neil-test-spec
                        :alias neil-test/neil-test-aliases}))

(deftest neil-test-aliases-test
  (testing "-e sets :excludes"
    (is (= {:excludes [:slow]} (parse ["-e" ":slow"]))))
  (testing "-i sets :includes"
    (is (= {:includes [:slow]} (parse ["-i" ":slow"])))))
