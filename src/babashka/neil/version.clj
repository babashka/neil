(ns babashka.neil.version
  (:require [babashka.fs :as fs]
            [babashka.neil.git :as git]
            [babashka.neil.meta :as meta]
            [babashka.neil.project :as proj]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn not-set-version-map? [{:keys [version-not-set]}]
  version-not-set)

(defn raw-string-version-map? [{:keys [raw-string]}]
  (string? raw-string))

(defn semver-version-map? [{:keys [major minor patch pre-release build]}]
  (and (nat-int? major)
       (nat-int? minor)
       (nat-int? patch)
       (if (some? pre-release) (string? pre-release) true)
       (if (some? build) (string? build) true)))

(def semver-regex
  "Source: https://semver.org/"
  (->> ["^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)"
        "(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)"
        "(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?"
        "(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$"]
       (apply str)
       re-pattern))

(defn parse-semver-string [s]
  (when s
    (let [[[_ major minor patch pre-release build]] (re-seq semver-regex s)]
      (when (and major minor patch)
        (merge {:major (Integer/parseInt major)
                :minor (Integer/parseInt minor)
                :patch (Integer/parseInt patch)}
               (when pre-release {:pre-release pre-release})
               (when build {:build build}))))))

(defn- str->version-map [s]
  (cond
    (#{:version-not-set} s) {:version-not-set true}
    (string? s) (or (parse-semver-string s) {:raw-string s})
    :else (throw (ex-info "Invalid version string" {:version-string s}))))

(defn- semver-version-map->str [version-map str-opts]
  (let [{:keys [minor major patch qualifier]} version-map]
    (str (when (:prefix str-opts) "v")
         (str/join "." [major minor patch])
         (when qualifier (str "-" qualifier)))))

(defn version-map->str [version-map & {:as str-opts}]
  (cond
    (raw-string-version-map? version-map)
    (:raw-string version-map)

    (semver-version-map? version-map)
    (semver-version-map->str version-map str-opts)

    (not-set-version-map? version-map)
    :version-not-set

    :else
    (throw (ex-info "Invalid version map" {:version-map version-map}))))

(defn git-opts [opts]
  (when (:deps-file opts)
    (let [root (str (fs/parent (:deps-file opts)))]
      (when (seq root)
        {:dir root}))))

(defn assert-no-unstaged-files [opts]
  (let [unstaged-files (git/unstaged-files (git/status (git-opts opts)))]
    (when (and (seq unstaged-files) (not (:force opts)))
      (throw (ex-info "Requires all files to be staged unless --force is provided"
                      {:unstaged-files unstaged-files})))))

(defn assert-at-least-one-staged-file [opts]
  (let [staged-files (git/staged-files (git/status (git-opts opts)))]
    (when (and (empty? staged-files) (not (:force opts)))
      (throw (ex-info "Requires at least one staged file unless --force is provided"
                      {})))))

(defn assert-git-repo [{:keys [deps-file dir]}]
  (when-not (git/repo? deps-file)
    (throw (ex-info "Requires git repo" {:deps-file (str (fs/canonicalize deps-file))
                                         :dir dir}))))

(defn deps-edn->project-version-string [deps-edn]
  (get-in deps-edn [:aliases :neil :project :version] :version-not-set))

(defn project-version-string? [x]
  (or (string? x) (#{:version-not-set} x)))

(defn assert-valid-project-version-string [project-version-string deps-file]
  (when-not (project-version-string? project-version-string)
    (throw (ex-info "Project version must be a string, e.g. \"1.0.0\""
                    {:deps-file (str (fs/canonicalize deps-file))
                     :project {:version project-version-string}}))))

(defn run-tag-command [{:keys [dir deps-file] :as opts}]
  (let [deps-file (proj/resolve-deps-file dir deps-file)
        deps-edn (some-> deps-file slurp edn/read-string)
        project-version-string (deps-edn->project-version-string deps-edn)
        _ (assert-valid-project-version-string project-version-string deps-file)
        project-version-map (str->version-map project-version-string)
        prefixed-version (version-map->str project-version-map :prefix true)]
    (assert-git-repo opts)
    (assert-no-unstaged-files opts)
    (assert-at-least-one-staged-file opts)
    (git/commit prefixed-version (git-opts opts))
    (git/tag prefixed-version (git-opts opts))
    (println prefixed-version)))

(defn run-root-command [{:keys [dir deps-file]}]
  (let [deps-file (proj/resolve-deps-file dir deps-file)
        deps-edn (some-> deps-file slurp edn/read-string)
        project-version-string (deps-edn->project-version-string deps-edn)
        _ (assert-valid-project-version-string project-version-string deps-file)
        project-version-map (str->version-map project-version-string)]
    (prn {:neil meta/version
          :project (version-map->str project-version-map :prefix false)})))

(defn print-help []
  (println (str/trim "
Usage: neil version [set|major|minor|patch] [version]
       neil version tag

Bump the :version key in the project config.")))

(defn print-version []
  (println "neil" meta/version))

(defn tag-command? [args]
  (#{:tag} (some-> args first keyword)))

(defn root-command? [args]
  (empty? args))

(defn neil-version [{:keys [args opts]}]
  (cond
    (:help opts) (print-help)
    (tag-command? args) (run-tag-command opts)
    (root-command? args) (run-root-command opts)
    :else (print-help)))
