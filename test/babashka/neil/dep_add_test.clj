(ns babashka.neil.dep-add-test
  (:require
   [babashka.neil :as neil]
   [babashka.neil.test-util :as test-util]
   [clojure.test :as t :refer [deftest is testing]]
   [version-clj.core :as version-clj]))

(deftest latest-version-test
  (let [config (neil/mvn-config {})]
    (is (= "2.0.0" (neil/latest-stable-mvn-version 'hiccup/hiccup config)))
    (is (= "2.0.0" (neil/latest-mvn-version 'hiccup/hiccup config)))
    (is (not (neg? (version-clj/version-compare
                    (neil/latest-stable-mvn-version 'org.clojure/clojure config) "1.12.5"))))))

(deftest dep-add-replaces-coord-test
  (testing ":mvn/version replaces :git/url and :git/sha and keeps :exclusions"
    (test-util/set-deps-edn! '{:deps {clj-kondo/clj-kondo {:git/url "https://github.com/clj-kondo/clj-kondo"
                                                           :git/sha "6ffc3934cb83d2c4fff16d84198c73b40cd8a078"
                                                           :exclusions [foo/bar]}}})
    (test-util/neil "dep add clj-kondo/clj-kondo :version 2024.03.13")
    (is (= '{clj-kondo/clj-kondo {:exclusions [foo/bar] :mvn/version "2024.03.13"}}
           (:deps (test-util/read-deps-edn)))))
  (testing ":git/sha replaces :mvn/version"
    (test-util/set-deps-edn! '{:deps {clj-kondo/clj-kondo {:mvn/version "2024.03.13"}}})
    (test-util/neil "dep add clj-kondo/clj-kondo :sha 6ffc3934cb83d2c4fff16d84198c73b40cd8a078")
    (is (= '{clj-kondo/clj-kondo {:git/url "https://github.com/clj-kondo/clj-kondo"
                                  :git/sha "6ffc3934cb83d2c4fff16d84198c73b40cd8a078"}}
           (:deps (test-util/read-deps-edn))))))
