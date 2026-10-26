(ns babashka.neil.http-test
  (:require
   [babashka.http-client :as http]
   [babashka.neil.http :as neil-http]
   [clojure.test :refer [deftest is]]))

(deftest invalid-token-warning-test
  (with-redefs [neil-http/github-basic-auth-enabled? true
                http/get (constantly {:status 401
                                      :body "{\"message\":\"Bad credentials\"}"})]
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (nil? (neil-http/get-json "https://api.github.com/a")))
        (is (nil? (neil-http/get-json "https://api.github.com/b"))))
      (is (= 1 (count (re-seq #"invalid or expired" (str err))))))))
