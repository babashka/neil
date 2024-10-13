(ns babashka.neil.dep-add-test
  (:require
   [babashka.neil :as neil]
   [clojure.test :as t :refer [deftest is]]))

(deftest latest-version-test
  (is (= "1.0.5" (neil/latest-stable-clojars-version 'hiccup/hiccup)))
  (is (= "2.0.0-RC3" (neil/latest-clojars-version 'hiccup/hiccup)))
  (is (= "1.12.0" (neil/latest-stable-mvn-version 'org.clojure/clojure))))
