(ns babashka.neil.add-alias-test
  (:require
   [babashka.neil :as neil]
   [borkdude.rewrite-edn :as r]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def kaocha-alias "\n{:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}\n :main-opts [\"-m\" \"kaocha.runner\"]}")

(defn trim= [s1 s2]
  (= (str/trim s1)
     (str/trim s2)))

(deftest add-alias-str-empty-deps-edn
    (is (trim= "
{:aliases {:kaocha {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
                    :main-opts [\"-m\" \"kaocha.runner\"]}}}
"
               (:deps-file-str (neil/add-alias-str "{}" :kaocha kaocha-alias)))))

(deftest add-alias-str-deps-edn-with-dev-alias
    (is (trim= "
{:aliases
 {:dev {}
  :kaocha {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
           :main-opts [\"-m\" \"kaocha.runner\"]}}}
"
               (:deps-file-str (neil/add-alias-str "{:aliases
 {:dev {}}}" :kaocha kaocha-alias)))))

(deftest add-alias-str-simplified-neil-deps-edn
  (let [input-deps-edn (str/trim "
{:deps {org.babashka/http-client {:mvn/version \"0.1.4\"}
        org.babashka/cli {:mvn/version \"0.8.58\"}
        cheshire/cheshire {:mvn/version \"5.11.0\"}
        version-clj/version-clj {:mvn/version \"2.0.2\"}}
 :aliases
 {:dev
  {:extra-paths [\"test\"]
   :extra-deps {io.github.cognitect-labs/test-runner
                {:git/url \"https://github.com/cognitect-labs/test-runner\"
                 :git/tag \"v0.5.1\"
                 :git/sha \"dfb30dd\"}}}}}
")
        expected-deps-edn (str/trim "
{:deps {org.babashka/http-client {:mvn/version \"0.1.4\"}
        org.babashka/cli {:mvn/version \"0.8.58\"}
        cheshire/cheshire {:mvn/version \"5.11.0\"}
        version-clj/version-clj {:mvn/version \"2.0.2\"}}
 :aliases
 {:dev
  {:extra-paths [\"test\"]
   :extra-deps {io.github.cognitect-labs/test-runner
                {:git/url \"https://github.com/cognitect-labs/test-runner\"
                 :git/tag \"v0.5.1\"
                 :git/sha \"dfb30dd\"}}}
  :kaocha {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
           :main-opts [\"-m\" \"kaocha.runner\"]}}}
")]
    (is (trim= expected-deps-edn
               (:deps-file-str (neil/add-alias-str input-deps-edn :kaocha kaocha-alias))))))

(comment
  ;; Some helper code to generate the strings for the tests

  (defn escape-quote [s]
    (str/escape s {\" "\\\""}))

  (def println2 (comp println escape-quote))

  (let [s (str/trim "
{:aliases
 {:dev {}}}
")
        ]
    (-> (:deps-file-str (neil/add-alias-str s :kaocha kaocha-alias))
        println2))

  :rcf)
