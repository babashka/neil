(ns babashka.neil.add-alias-test
  (:require
   [babashka.neil :as neil]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def kaocha-alias "\n{:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}\n :main-opts [\"-m\" \"kaocha.runner\"]}")

(defn trim= [s1 s2]
  (= (str/trim s1)
     (str/trim s2)))

(deftest add-alias-str-good1
  (is (trim= "
{:aliases
 {:kaocha ;; added by neil
  {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
   :main-opts [\"-m\" \"kaocha.runner\"]}}}"
             (:deps-file-str (neil/add-alias-str "{}" :kaocha kaocha-alias)))))

(deftest add-alias-str-weird1
  ;; weird space in between.
  (is (trim= "
{:aliases {:dev {}

 :kaocha ;; added by neil
 {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
  :main-opts [\"-m\" \"kaocha.runner\"]}}}"
             (:deps-file-str (neil/add-alias-str "{:aliases {:dev {}}}" :kaocha kaocha-alias)))))

(deftest add-alias-str-weird2
  ;; weird space in between.
  (is (trim= "
{:aliases
 {:dev {}

 :kaocha ;; added by neil
 {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
  :main-opts [\"-m\" \"kaocha.runner\"]}}}"
             (:deps-file-str (neil/add-alias-str (str/trim "
{:aliases
 {:dev {}}}"
                                                           )
                                                 :kaocha kaocha-alias)))))

(comment
  (defn escape-quote [s]
    (str/escape s {\" "\\\""}))

  (-> "hello \"you\"!" escape-quote escape-quote escape-quote)
  (def println2 (comp println escape-quote))

  (println)

  (-> (:deps-file-str (neil/add-alias-str (str/trim "
{:aliases
 {:dev {}}}"
                                                    )
                                          :kaocha kaocha-alias))
      println2)

  )
