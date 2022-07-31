(ns babashka.neil.git
  {:no-doc true}
  (:require [babashka.neil.curl :refer [curl-get-json]]
            [clojure.string :as str]))

(defn default-branch [lib]
  (get (curl-get-json (format "https://api.github.com/repos/%s/%s"
                              (namespace lib) (name lib)))
       :default_branch))

(defn clean-github-lib [lib]
  (let [lib (str/replace lib "com.github." "")
        lib (str/replace lib "io.github." "")
        lib (symbol lib)]
    lib))

(defn latest-github-sha [lib]
  (let [lib (clean-github-lib lib)
        branch (default-branch lib)]
    (get (curl-get-json (format "https://api.github.com/repos/%s/%s/commits/%s"
                                (namespace lib) (name lib) branch))
         :sha)))

(defn list-github-tags [lib]
  (let [lib (clean-github-lib lib)]
    (curl-get-json (format "https://api.github.com/repos/%s/%s/tags"
                           (namespace lib) (name lib)))))

(defn latest-github-tag [lib]
  (-> (list-github-tags lib)
      first))

(defn find-github-tag [lib tag]
  (->> (list-github-tags lib)
       (filter #(= (:name %) tag))
       first))
