(ns babashka.neil.curl
  {:no-doc true}
  (:require
   [babashka.http-client :as curl]
   [cheshire.core :as cheshire]
   [clojure.string :as string]))

(import java.net.URLEncoder)

(def unexceptional-statuses
  #{200 201 202 203 204 205 206 207 300 301 302 303 304 307})

(defn url-encode [s] (URLEncoder/encode s "UTF-8"))

(def github-user (or (System/getenv "NEIL_GITHUB_USER")
                     (System/getenv "BABASHKA_NEIL_DEV_GITHUB_USER")))
(def github-token (or (System/getenv "NEIL_GITHUB_TOKEN")
                      (System/getenv "BABASHKA_NEIL_DEV_GITHUB_TOKEN")))

(def curl-opts
  (merge {:throw false}
         (when (and github-user github-token)
           {:basic-auth [github-user github-token]})))

(defn curl-get-json
  ([url] (curl-get-json url nil))
  ([url opts]
   (let [response    (curl/get url (merge curl-opts opts))
         parsed-body (try (-> response :body (cheshire/parse-string true))
                          (catch Exception e
                            (binding [*out* *err*]
                              (println "Unable to parse body as JSON:")
                              (println (ex-message e))
                              (println (-> response :body)))
                            nil #_(throw e)))]
     (cond
       (and (= 403 (:status response))
            (string/includes? url "api.github")
            (string/includes? (:message parsed-body) "rate limit"))
       (binding [*out* *err*]
         (println "You've hit the github rate-limit (60 reqs/hr).
  You can set an API Token to increase the limit.
  See neil's README for details.")
         nil #_(System/exit 1))

       (contains? unexceptional-statuses (:status response))
       parsed-body
       (= 404 (:status response))
       nil
       :else
       (binding [*out* *err*]
         (println
          (or (not-empty (:body response))
              (str url " request returned status code: " (:status response))))
         nil)))))
