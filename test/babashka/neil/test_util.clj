(ns babashka.neil.test-util
  (:require [babashka.fs :as fs]
            [babashka.neil :as neil-main]
            [babashka.process :refer [sh] :as process]
            [babashka.tasks :as tasks]
            [clojure.edn :as edn]
            [taoensso.timbre :as log])
  (:import (java.io StringWriter)))

(def test-dir (str (fs/file (fs/temp-dir) "neil")))

(defn test-file [name]
  (doto (fs/file test-dir name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn ensure-git-repo []
  (let [opts {:dir test-dir :err :inherit}]
    (sh "git init -b main" opts)
    (sh "git config user.name 'Neil Tests'" opts)
    (sh "git config user.email '<>'" opts)
    (sh "git add ." opts)
    (sh "git commit -m 'First commit'" opts)))

(defn test-logging-config [log-stream]
  {:appenders
   {:log-stream {:enabled? true
                 :fn #(binding [*out* log-stream]
                        (println (force (:output_ %))))}}})

(defn neil [cli-args & {:keys [out] :or {out :edn}}]
  (let [deps-file (str (test-file "deps.edn"))
        cli-args' (concat (process/tokenize cli-args) [:deps-file deps-file])
        log-stream (StringWriter.)]
    (binding [log/*config* (merge log/*config* (test-logging-config log-stream))
              *command-line-args* cli-args']
      (let [s (with-out-str (tasks/exec `neil-main/-main))]
        (print (str log-stream))
        {:out (if (#{:edn} out) (edn/read-string s) s)}))))
