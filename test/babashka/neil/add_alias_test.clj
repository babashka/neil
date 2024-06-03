(ns babashka.neil.add-alias-test
  (:require
   [babashka.neil :as neil]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def kaocha-alias "\n{:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}\n :main-opts [\"-m\" \"kaocha.runner\"]}")

(defn trim= [s1 s2]
  (= (str/trim s1)
     (str/trim s2)))

(deftest add-alias-str
  (is (trim= "
{:aliases
 {:kaocha ;; added by neil
  {:extra-deps {lambdaisland/kaocha {:mvn/version \"1.91.1392\"}}
   :main-opts [\"-m\" \"kaocha.runner\"]}}}"
             (:deps-file-str (neil/add-alias-str "{}" :kaocha kaocha-alias)))))
