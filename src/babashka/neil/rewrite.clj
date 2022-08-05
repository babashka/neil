(ns babashka.neil.rewrite
  (:require [clojure.string :as str]))

(defn indent [s n]
  (let [spaces (apply str (repeat n " "))
        lines (str/split-lines s)]
    (str/join "\n" (map (fn [s]
                          (if (str/blank? s) s
                              (str spaces s))) lines))))

(defn clean-trailing-whitespace [s]
  (str/join "\n" (map str/trimr (str/split-lines s))))
