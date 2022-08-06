(ns babashka.neil.dev
  (:require [babashka.process :refer [sh]]
            [clojure.core.async :refer [<!] :as async]
            [clojure.string :as str]
            [pod.babashka.fswatcher :as fw]
            [taoensso.timbre :as log]))

(def watch-paths ["prelude" "src"])

(defn- build-event? [{:keys [type path] :as _watch-event}]
  (and (not (#{:chmod} type))
       (not (str/ends-with? path "~"))))

(defn- start-builder [build-events]
  (async/go-loop [i 1]
    (let [event (<! build-events)]
      (log/info [:start-build i event])
      (sh "bb gen-script")
      (log/info [:end-build i event])
      (recur (inc i)))))

(defn- start-watchers [watch-paths build-events]
  (doseq [p watch-paths]
    (fw/watch p #(async/put! build-events %) {:recursive true})))

(defn dev []
  (let [build-xf (filter build-event?)
        build-events (async/chan (async/sliding-buffer 1) build-xf)]
    (log/info [:start-dev])
    (start-builder build-events)
    (start-watchers watch-paths build-events)
    (deref (promise))))
