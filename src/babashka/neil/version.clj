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

(defn semver-version-map? [{:keys [major minor patch pre-release build] :as m}]
  (and (nat-int? major)
       (nat-int? minor)
       (nat-int? patch)
       (if (contains? m :pre-release) (string? pre-release) true)
       (if (contains? m :build) (string? build) true)))

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

(defn git-tag-version-enabled? [opts]
  (and (git/repo? (:deps-file opts))
       (not (false? (:git-tag-version opts)))
       (not (false? (:tag opts)))
       (not (:no-git-tag-version opts))
       (not (:no-tag opts))))

(defn- set-version! [v {:keys [deps-file] :as _opts}]
  (proj/assoc-project-meta! {:deps-file deps-file :k :version :v v}))

(defn git-clean-working-directory? [git-status-result]
  (some->> (:statuses git-status-result)
           (remove #(re-seq #"^\?" (:status %)))
           empty?))

(defn assert-clean-working-directory [opts]
  (when (and (not (git-clean-working-directory? (git/status (git-opts opts))))
             (not (:force opts)))
    (throw (ex-info "Requires clean working directory unless --force is provided" {}))))

(defn run-set-command [{:keys [version dir deps-file] :as opts}]
  (let [deps-file' (proj/resolve-deps-file dir deps-file)
        opts' (assoc opts :deps-file deps-file')
        git-tag-version-enabled (git-tag-version-enabled? opts')]
    (when git-tag-version-enabled
      (assert-clean-working-directory opts'))
    (proj/ensure-neil-project opts')
    (let [next-version-map (str->version-map version)
          next-version-string (version-map->str next-version-map :prefix false)
          next-prefixed-version (version-map->str next-version-map :prefix true)]
      (set-version! next-version-string opts')
      (when git-tag-version-enabled
        (git/add ["deps.edn"] (git-opts opts'))
        (git/commit next-prefixed-version (git-opts opts'))
        (git/tag next-prefixed-version (git-opts opts')))
      (println next-prefixed-version))))

(defn- assert-semver-version [version-map]
  (when-not (semver-version-map? version-map)
    (let [provided-version (version-map->str version-map :prefix false)]
      (throw (ex-info "Only SemVer-style version strings can be bumped"
                      {:provided-version provided-version})))))

(defn- initial-version-map [semver-key override]
  (assoc {:major 0 :minor 0 :patch 0} semver-key (or override 1)))

(defn- bump-version [version-map semver-key override]
  (if (not-set-version-map? version-map)
    (initial-version-map semver-key override)
    (do
      (assert-semver-version version-map)
      (let [next-version (or override (inc (get version-map semver-key)))
            version-map' (assoc version-map semver-key next-version)
            {:keys [major minor patch]} version-map']
        (case semver-key
          :major {:major major :minor 0 :patch 0}
          :minor {:major major :minor minor :patch 0}
          :patch {:major major :minor minor :patch patch})))))

(defn run-bump-command [command {:keys [dir deps-file] :as opts}]
  (let [deps-file (proj/resolve-deps-file dir deps-file)
        deps-edn (some-> deps-file slurp edn/read-string)
        project-version-string (deps-edn->project-version-string deps-edn)
        _ (assert-valid-project-version-string project-version-string deps-file)
        project-version-map (str->version-map project-version-string)
        override (:version opts)
        bumped-version-map (bump-version project-version-map command override)
        bumped-version-str (version-map->str bumped-version-map :prefix false)]
    (run-set-command (assoc opts :version bumped-version-str))))

(defn print-help []
  (println (str/trim "
Usage: neil version
       neil version set [version]
       neil version [major|minor|patch] [version]
       neil version tag

Commands for managing the :version key in the deps.edn project config.

By default, these commands also create Git commits and tags in the current Git
repo to help with publishing new versions.

The project version is located in the deps.edn file at the current directory at
the path [:aliases :neil :project :version].

Any string is a valid version, but only SemVer-style versions can be bumped
using the [major|minor|patch] subcommands.

## neil version

  Print the current neil and project versions.

  Examples:
    neil version

## neil version set [version]

  Set the :version key in the project config to the provided version.

  - Creates a Git commit and tag (this can be bypassed with --no-tag)
  - Requires the working directory to be clean (this can be bypassed with --force)

  Examples:
    neil version set 0.1.0
    neil version set 9.4.146.24-node.21 --force
    neil version set 1.1.1q+quic --no-tag

## neil version [major|minor|patch] [version]

  Bump the :version key in the project config. If no specific version is provided,
  the selected version will be incremented by one.

  - Creates a Git commit and tag (this can be bypassed with --no-tag)
  - Requires the Git working directory to be clean (this can be bypassed with --force)

  Examples:
    neil version patch
    neil version major 3 --force
    neil version minor --no-tag

## neil version tag

  Create a Git commit and tag for the current version.

  - Requires that the current directory is a Git repo
  - Requires that there are no unstaged files (this can be bypassed with --force)
  - Requires that at least one file is staged (this can be bypassed with --force)

  Examples:
    neil version tag
    neil version tag --force")))

(defn print-version []
  (println "neil" meta/version))

(defn neil-version
  ([parse-args] (neil-version :root parse-args))
  ([command {:keys [opts] :as _parse-args}]
   (if (:help opts)
     (print-help)
     (case command
       :root (run-root-command opts)
       :tag (run-tag-command opts)
       :set (run-set-command opts)
       (:major :minor :patch) (run-bump-command command opts)))))
