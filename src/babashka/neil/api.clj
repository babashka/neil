(ns babashka.neil.api
  (:require [babashka.neil :as neil]))

(defn add-dep [opts]
  (neil/add-dep (neil/with-default-deps-edn opts)))

(defn add-test [opts]
  (neil/add-cognitect-test-runner (neil/with-default-deps-edn opts)))

(defn add-build [opts]
  (neil/add-build (neil/with-default-deps-edn opts)))

(defn add-kaocha [opts]
  (neil/add-kaocha (neil/with-default-deps-edn opts)))
