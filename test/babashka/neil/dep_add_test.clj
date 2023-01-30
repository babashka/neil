(ns babashka.neil.dep-add-test
  (:require
   [babashka.neil :as neil]
   [clojure.test :as t :refer [deftest is]]))

(deftest latest-version-test
  (is (= "1.0.5" (neil/latest-clojars-version 'hiccup/hiccup)))
  (is (= "1.11.1" (neil/latest-mvn-version 'org.clojure/clojure))))
