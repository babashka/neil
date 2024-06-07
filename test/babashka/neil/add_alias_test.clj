(ns babashka.neil.add-alias-test
  (:require
   [babashka.neil :as neil]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(deftest value->assoc-in-pairs-test
  (is (= (neil/value->assoc-in-pairs [:x] 1)
         '([[:x] 1])))
  (is (= (list [[:p :x] 1]
               [[:p :y] 2])
         (neil/value->assoc-in-pairs [] {:p {:x 1 :y 2}}))))

(def kaocha-alias
  '{:extra-deps {lambdaisland/kaocha {:mvn/version "1.91.1392"}},
    :main-opts ["-m" "kaocha.runner"]})

(def nrepl-alias
  '{:extra-deps
    {nrepl/nrepl {:mvn/version "1.1.2"},
     cider/cider-nrepl {:mvn/version "0.49.0"},
     refactor-nrepl/refactor-nrepl {:mvn/version "3.10.0"}},
    :main-opts
    ["-m" "nrepl.cmdline" "--interactive" "--color"
     "--middleware" "[cider.nrepl/cider-middleware,refactor-nrepl.middleware/wrap-refactor]"]})

(defn trim= [s1 s2]
  (= (str/trim s1)
     (str/trim s2)))

(deftest add-alias-kaocha-small
    (is (trim= "
{:aliases {:kaocha {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
                    :main-opts [\"-m\" \"kaocha.runner\"]}}}
"
               (:deps-file-str (neil/add-alias-str "{}" :kaocha kaocha-alias)))))

(deftest add-alias-kaocha-bigger-1
    (is (trim= "
{:aliases
 {:dev {}
  :kaocha {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
           :main-opts [\"-m\" \"kaocha.runner\"]}}}
"
               (:deps-file-str (neil/add-alias-str "{:aliases
 {:dev {}}}" :kaocha kaocha-alias)))))

(deftest add-alias-kaocha-bigger-2
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
