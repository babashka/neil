(ns babashka.neil.version-test
  (:require [babashka.tasks :as tasks]
            [babashka.neil :as neil]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest version-root-test
  (binding [*command-line-args* ["version"]]
    (let [out (with-out-str (tasks/exec `neil/-main))]
      (is (str/starts-with? out "neil 0.1.")))))
