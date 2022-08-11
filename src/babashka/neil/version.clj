(ns babashka.neil.version
  (:require [babashka.fs :as fs]
            [babashka.neil.project :as proj]
            [babashka.process :refer [sh]]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [babashka.neil.meta :as meta]
            [clojure.string :as str]))

(defn print-version-help []
  (println (str/trim "
Usage: neil version [set|major|minor|patch] [version]
       neil version tag

Bump the :version key in the project config.")))

(def semver-regex #"^v?(\d+)\.(\d+)\.(\d+)(-(\w+))?$")

(defn parse-semver-string [s]
  (when s
    (let [[[_ major minor patch _ qualifier]] (re-seq semver-regex s)]
      (when (and major minor patch)
        (merge {:major (Integer/parseInt major)
                :minor (Integer/parseInt minor)
                :patch (Integer/parseInt patch)}
               (when qualifier {:qualifier qualifier}))))))

(defn current-version [deps-map]
  (let [v (get-in deps-map [:aliases :neil :project :version])]
    (if (string? v)
      (if-let [parsed (parse-semver-string v)]
        parsed
        v)
      v)))

(defn override-version [k {:keys [major minor patch]}]
  (case k
    :major {:major major :minor 0 :patch 0}
    :minor {:major major :minor minor :patch 0}
    :patch {:major major :minor minor :patch patch}))

(defn bump-version [k opts]
  (override-version k (update opts k inc)))

(def zero-version {:major 0 :minor 0 :patch 0})

(defn version-map->str [x & {:keys [prefix] :or {prefix true}}]
  (when x
    (if (string? x)
      (if-let [parsed (parse-semver-string x)]
        (version-map->str parsed)
        x)
      (let [{:keys [minor major patch qualifier]} x]
        (str (when prefix "v")
             (str/join "." [major minor patch])
             (when qualifier (str "-" qualifier)))))))

(defn save-version-bump [deps-map sub-command override opts]
  (let [prev-v (or (current-version deps-map) zero-version)
        next-v (version-map->str
                 (if override
                   (do
                     (when (<= override (get prev-v sub-command))
                       (throw (ex-info (format "Invalid %s number provided" (name sub-command))
                                       {:current prev-v
                                        :input {sub-command override}})))
                     (merge zero-version
                            (override-version sub-command (assoc prev-v sub-command override))))
                   (bump-version sub-command (or prev-v zero-version)))
                 :prefix false)]
    (proj/assoc-project-meta! {:deps-file (:deps-file opts) :k :version :v next-v})
    next-v))

(def valid-version-keys
  #{:major :minor :patch})

(defn git-opts [opts]
  (when (:deps-file opts)
    (let [root (str (fs/parent (:deps-file opts)))]
      (when (seq root)
        {:dir root}))))

(defn parse-git-status [line]
  (let [[[_ status path]] (re-seq #"^(.{2}) (.+)$" line)]
    {:status status :path path}))

(comment (parse-git-status "X  f"))

(defn git-status [opts]
  (let [cmd "git status --porcelain=v1"
        {:keys [out err] :as x} (sh cmd (git-opts opts))]
    (if-not (str/blank? err)
      {:err err}
      {:statuses (if (str/blank? out)
                   '()
                   (map parse-git-status (str/split-lines out)))})))

(comment (git-status nil))

(defn resolve-git-repo [opts]
  (if (:deps-file opts)
    (fs/file (fs/parent (:deps-file opts)) ".git")
    (fs/file ".git")))

(defn git-repo? [opts]
  (fs/exists? (resolve-git-repo opts)))

(defn git-clean-working-directory? [git-status-result]
  (some->> (:statuses git-status-result)
           (remove #(re-seq #"^\?" (:status %)))
           empty?))

(defn git-add [opts]
  (sh "git add deps.edn"
      (assoc (git-opts opts) :err :inherit)))

(defn git-commit [version opts]
  (let [s (version-map->str version)
        cmd ["git" "commit" "-m" s]
        {:keys [out err]} (sh cmd (git-opts opts))]
    (when (seq err) (throw (ex-info err {})))))

(defn git-tag [version opts]
  (let [s (version-map->str version)
        cmd ["git" "tag" "-a" s "-m" s]
        {:keys [out err]} (sh cmd (git-opts opts))]
    (when (seq err) (throw (ex-info err {})))))

(defn assert-clean-working-directory [opts]
  (when (and (not (git-clean-working-directory? (git-status opts)))
             (not (:force opts)))
    (throw (ex-info "Requires clean working directory unless --force is provided" {}))))

(defn git-tag-version-enabled? [opts]
  (and (git-repo? opts)
       (not (false? (:git-tag-version opts)))
       (not (false? (:tag opts)))
       (not (:no-git-tag-version opts))
       (not (:no-tag opts))))

(defn bump-arg [args]
  (valid-version-keys (some-> args first keyword)))

(defn run-bump-command [opts args]
  (if-not (<= 1 (count args) 2)
    (print-version-help)
    (let [git-tag-version-enabled (git-tag-version-enabled? opts)]
      (when git-tag-version-enabled
        (assert-clean-working-directory opts))
      (proj/ensure-neil-project opts)
      (let [deps-map (edn/read-string (slurp (:deps-file opts)))
            override (when (second args) (Integer/parseInt (second args)))
            after (save-version-bump deps-map (bump-arg args) override opts)]
        (when git-tag-version-enabled
          (git-add opts)
          (git-commit after opts)
          (git-tag after opts))
        (println (version-map->str after))
        nil))))

(defn run-root-command [opts]
  (let [deps-map (edn/read-string (slurp (:deps-file opts)))
        project-version (or (version-map->str (current-version deps-map)
                                              :prefix false)
                            :version-not-set)]
    (prn {:neil meta/version :project project-version})))

(defn git-unstaged-files [git-status-result]
  (->> (:statuses git-status-result)
       (filter #(re-seq #"^ " (:status %)))
       (map :path)))

(defn git-staged-files [git-status-result]
  (->> (:statuses git-status-result)
       (filter #(re-seq #"^[ARMD]" (:status %)))
       (map :path)))

(comment
  (def gs (git-status nil))
  (clojure.pprint/pprint (git-unstaged-files gs)))

(defn assert-no-unstaged-files [opts]
  (let [unstaged-files (git-unstaged-files (git-status opts))]
    (when (and (seq unstaged-files) (not (:force opts)))
      (throw (ex-info "Requires all files to be staged unless --force is provided"
                      {:unstaged-files unstaged-files})))))

(defn assert-at-least-one-staged-file [opts]
  (let [staged-files (git-staged-files (git-status opts))]
    (when (and (empty? staged-files) (not (:force opts)))
      (throw (ex-info "Requires at least one staged file unless --force is provided"
                      {})))))

(defn run-tag-command [opts args]
  (if-not (= (count args) 1)
    (print-version-help)
    (do
      (assert-no-unstaged-files opts)
      (assert-at-least-one-staged-file opts)
      (let [deps-map (edn/read-string (slurp (:deps-file opts)))
            v (current-version deps-map)]
        (git-commit v opts)
        (git-tag v opts)
        (println (version-map->str v))
        nil))))

(defn tag-arg [args]
  (#{:tag} (some-> args first keyword)))

(defn set-version [override opts]
  (proj/assoc-project-meta! {:deps-file (:deps-file opts) :k :version :v override})
  override)

(defn strictly-increasing-version? [prev-v next-v]
  (let [prev-v' (or prev-v zero-version)]
    (or (< (:major prev-v') (:major next-v))
        (and (<= (:major prev-v') (:major next-v))
             (< (:minor prev-v') (:minor next-v)))
        (and (<= (:major prev-v') (:major next-v))
             (<= (:minor prev-v') (:minor next-v))
             (< (:patch prev-v') (:patch next-v))))))

(defn assert-strictly-increasing-version [prev-v next-v]
  (when-not (strictly-increasing-version? prev-v next-v)
    (throw (ex-info "Versions in SemVer format must be strictly increasing"
                    {:current prev-v :input next-v}))))

(defn run-set-command [opts args]
  (if-not (= (count args) 2)
    (print-version-help)
    (let [git-tag-version-enabled (git-tag-version-enabled? opts)]
      (when git-tag-version-enabled
        (assert-clean-working-directory opts))
      (proj/ensure-neil-project opts)
      (let [deps-map (edn/read-string (slurp (:deps-file opts)))
            override (second args)
            _ (when-let [semver (parse-semver-string override)]
                (assert-strictly-increasing-version (current-version deps-map) semver))
            after (set-version override opts)]
        (when git-tag-version-enabled
          (git-add opts)
          (git-commit after opts)
          (git-tag after opts))
        (println (version-map->str after))
        nil))))

(defn set-arg [args]
  (#{:set} (some-> args first keyword)))

(defn neil-version
  [{:keys [opts args]}]
  (cond
    (:help opts) (print-version-help)
    (set-arg args) (run-set-command opts args)
    (bump-arg args) (run-bump-command opts args)
    (tag-arg args) (run-tag-command opts args)
    :else (run-root-command opts)))
