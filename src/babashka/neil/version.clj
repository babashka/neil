(ns babashka.neil.version
  (:require [babashka.fs :as fs]
            [babashka.neil.meta :as meta]
            [babashka.neil.project :as proj]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn project-version? [x]
  (or (string? x) (#{:version-not-set} x)))

(defn assert-valid-project-version [project-version deps-file]
  (when-not (project-version? project-version)
    (throw (ex-info "Project version must be a string, e.g. \"1.0.0\""
                    {:deps-file (str (fs/canonicalize deps-file))
                     :project {:version project-version}}))))

(defn run-root-command [{:keys [dir deps-file]}]
  (let [deps-file (proj/resolve-deps-file dir deps-file)
        deps-edn (some-> deps-file slurp edn/read-string)
        project-version (get-in deps-edn [:aliases :neil :project :version]
                                :version-not-set)]
    (assert-valid-project-version project-version deps-file)
    (prn {:neil meta/version :project project-version})))

(defn print-help []
  (println (str/trim "
Usage: neil version [set|major|minor|patch] [version]
       neil version tag

WARNING: The `neil version` v2 command is under development and not ready for
         end users right now.

Bump the :version key in the project config.")))

(defn run-v2-command [opts]
  (if (:help opts)
    (print-help)
    (run-root-command opts)))

(defn print-version []
  (println "neil" meta/version))

(defn neil-version [{:keys [opts]}]
  ; NOTE: The v2 logic will become the new default behavior when the new
  ;       `neil version` is ready for release. This will be removed later on.
  (if (:v2 opts)
    (run-v2-command opts)
    (print-version)))
