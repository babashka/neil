(ns babashka.neil.dev
  (:require [babashka.process :refer [sh]]
            [clojure.core.async :refer [<!] :as async]
            [clojure.string :as str]
            [pod.babashka.fswatcher :as fw]
            [taoensso.timbre :as log]))

(def watch-paths ["bb.edn" "prelude" "src" "dev"])

(defn- build-event? [{:keys [type path] :as _watch-event}]
  (and (not (#{:chmod} type))
       (not (str/ends-with? path "~"))))

(def build-number (atom 0))

(defn build-once [event]
  (let [i (swap! build-number inc)]
    (log/info [:start-build i event])
    (sh "bb gen-script" {:err :inherit})
    (log/info [:end-build i event])))

(defn- start-builder [build-events]
  (async/go-loop []
    (let [event (<! build-events)]
      (build-once event)
      (recur))))

(defn- start-watchers [watch-paths build-events]
  (doseq [p watch-paths]
    (fw/watch p #(async/put! build-events %) {:recursive true})))

(defn dev []
  (let [build-xf (filter build-event?)
        build-events (async/chan (async/sliding-buffer 1) build-xf)]
    (log/info [:start-dev])
    (start-watchers watch-paths build-events)
    (build-once {:type ::startup-build})
    (start-builder build-events)
    (deref (promise))))
