(ns babashka.neil.version
  (:require [babashka.fs :as fs]
            [babashka.neil.common :as common]
            [borkdude.rewrite-edn :as r]
            [babashka.process :refer [sh]]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn print-version-help []
  (println (str/trim "
Usage: neil version [major|minor|patch] [version]

Bump the :version key in the project config.")))

(def version-path
  [:aliases common/project-alias :project :version])

(defn current-version [deps-map]
  (get-in deps-map version-path))

(defn override-version [k {:keys [major minor patch]}]
  (case k
    :major {:major major :minor 0 :patch 0}
    :minor {:major major :minor minor :patch 0}
    :patch {:major major :minor minor :patch patch}))

(defn bump-version [k opts]
  (override-version k (update opts k inc)))

(def zero-version {:major 0 :minor 0 :patch 0})

(defn save-version-bump [deps-nodes deps-map sub-command override]
  (let [prev-v (current-version deps-map)
        next-v (if override
                 (do
                   (when (<= override (get prev-v sub-command))
                     (throw (ex-info (format "Invalid %s number provided" (name sub-command))
                                     {:current prev-v
                                      :input {sub-command override}})))
                   (merge zero-version
                          (override-version sub-command (assoc prev-v sub-command override))))
                 (bump-version sub-command (or prev-v zero-version)))]
    (r/assoc-in deps-nodes version-path next-v)))

(def valid-version-keys
  #{:major :minor :patch})

(defn git-opts [opts]
  (let [root (str (fs/parent (:deps-file opts)))]
    (when (seq root)
      {:dir root})))

(defn git-repo? [opts]
  (str/blank? (:err (sh "git status --porcelain" (git-opts opts)))))

(defn git-clean-working-directory? [opts]
  (let [{:keys [out err]} (sh "git status --porcelain" (git-opts opts))]
    (and (str/blank? err) (str/blank? out))))

(defn version-map->str [{:keys [major minor patch qualifier]}]
  (str (str/join "." [major minor patch])
       (when qualifier (str "-" qualifier))))

(defn git-add [opts]
  (sh "git add deps.edn"
      (assoc (git-opts opts) :err :inherit)))

(defn git-commit [version opts]
  (sh ["git" "commit" "-m" (version-map->str version)]
      (assoc (git-opts opts) :err :inherit)))

(defn git-tag [version opts]
  (sh ["git" "tag" (str "v" (version-map->str version))]
      (assoc (git-opts opts) :err :inherit)))

(defn assert-clean-working-directory [opts]
  (when (and (not (git-clean-working-directory? opts))
             (not (:force opts)))
    (throw (ex-info "Requires clean working directory unless --force is provided" {}))))

(defn git-tag-version-enabled? [opts]
  (and (git-repo? opts)
       (not (false? (:git-tag-version opts)))
       (not (:no-git-tag-version opts))))

(defn run-sub-command [sub-command opts args]
  (let [git-tag-version-enabled (git-tag-version-enabled? opts)]
    (when git-tag-version-enabled
      (assert-clean-working-directory opts))
    (common/ensure-deps-file opts)
    (let [deps-map (edn/read-string (slurp (:deps-file opts)))
          deps-nodes (-> opts common/edn-string common/edn-nodes)
          override (when (second args) (Integer/parseInt (second args)))
          deps-nodes' (save-version-bump deps-nodes deps-map sub-command override)
          before {:project {:version (current-version deps-map)}}
          after-v (current-version (edn/read-string (str deps-nodes')))
          after {:project {:version after-v}}]
      (spit (:deps-file opts) (str deps-nodes'))
      (when git-tag-version-enabled
        (git-add opts)
        (git-commit after-v opts)
        (git-tag after-v opts))
      (pprint/pprint {:before before :after after}))))

(defn print-version [opts]
  (let [deps-map (edn/read-string (slurp (:deps-file opts)))]
    (pprint/pprint {:project {:version (current-version deps-map)}})))

(defn run-root-command [opts]
  (print-version opts))

(defn neil-version
  [{:keys [opts args]}]
  (if (:help opts)
    (print-version-help)
    (if-let [sub-command (valid-version-keys (some-> args first keyword))]
      (run-sub-command sub-command opts args)
      (run-root-command opts))))
