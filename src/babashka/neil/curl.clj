(ns babashka.neil.curl
  {:no-doc true}
  (:require
   [babashka.curl :as curl]
   [babashka.fs :as fs]
   [cheshire.core :as cheshire]
   [clojure.string :as string]))

(import java.net.URLEncoder)

(defn url-encode [s] (URLEncoder/encode s "UTF-8"))

(def github-user (or (System/getenv "NEIL_GITHUB_USER")
                     (System/getenv "BABASHKA_NEIL_DEV_GITHUB_USER")))
(def github-token (or (System/getenv "NEIL_GITHUB_TOKEN")
                      (System/getenv "BABASHKA_NEIL_DEV_GITHUB_TOKEN")))

(def curl-opts
  (merge {:throw      false
          :compressed (not (fs/windows?))}
         (when (and github-user github-token)
           {:basic-auth [github-user github-token]})))

(defn curl-get-json [url]
  (let [response    (curl/get url curl-opts)
        parsed-body (-> response :body (cheshire/parse-string true))]
    (cond
      (and (= 403 (:status response))
           (string/includes? url "api.github")
           (string/includes? (:message parsed-body) "rate limit"))
      (binding [*out* *err*]
        (println "You've hit the github rate-limit (60 reqs/hr).
  You can set an API Token to increase the limit.
  See neil's readme for details.")
        (System/exit 1))

      :else
      parsed-body)))
