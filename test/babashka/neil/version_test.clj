(ns babashka.neil.version-test
  (:require [babashka.tasks :as tasks]
            [babashka.neil :as neil]
            [babashka.neil.meta :as meta]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest version-root-test
  (binding [*command-line-args* ["version"]]
    (let [out (with-out-str (tasks/exec `neil/-main))]
      (is (str/starts-with? out "neil :version-not-set\n"))))
  (with-redefs [meta/version "1.0.0"]
    (binding [*command-line-args* ["version"]]
      (let [out (with-out-str (tasks/exec `neil/-main))]
        (is (str/starts-with? out "neil 1.0.0\n"))))))
