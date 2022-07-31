(ns babashka.neil.curl
  {:no-doc true}
  (:require
   [babashka.curl :as curl]
   [babashka.fs :as fs]
   [cheshire.core :as cheshire]))

(import java.net.URLEncoder)

(defn url-encode [s] (URLEncoder/encode s "UTF-8"))

(def curl-opts
  {:throw false
   :compressed (not (fs/windows?))})

(defn curl-get-json [url]
  (-> (curl/get url curl-opts)
      :body (cheshire/parse-string true)))
