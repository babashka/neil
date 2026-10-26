(ns babashka.neil.dep-add-test
  (:require
   [babashka.neil :as neil]
   [clojure.test :as t :refer [deftest is]]
   [version-clj.core :as version-clj]))

(deftest latest-version-test
  (let [config (neil/mvn-config {})]
    (is (= "2.0.0" (neil/latest-stable-mvn-version 'hiccup/hiccup config)))
    (is (= "2.0.0" (neil/latest-mvn-version 'hiccup/hiccup config)))
    (is (not (neg? (version-clj/version-compare
                    (neil/latest-stable-mvn-version 'org.clojure/clojure config) "1.12.5"))))))
