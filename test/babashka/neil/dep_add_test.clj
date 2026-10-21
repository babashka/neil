(ns babashka.neil.dep-add-test
  (:require
   [babashka.neil :as neil]
   [clojure.test :as t :refer [deftest is]]
   [version-clj.core :as version-clj]))

(deftest latest-version-test
  (is (= "2.0.0" (neil/latest-stable-clojars-version 'hiccup/hiccup)))
  (is (= "2.0.0" (neil/latest-clojars-version 'hiccup/hiccup)))
  (is (not (neg? (version-clj/version-compare
                  (neil/latest-stable-mvn-version 'org.clojure/clojure) "1.12.5")))))
