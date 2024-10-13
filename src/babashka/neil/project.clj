(ns babashka.neil.project
  (:require
   [babashka.fs :as fs]
   [borkdude.rewrite-edn :as r]
   [clojure.edn :as edn]))

(defn resolve-deps-file [dir deps-file]
  (if dir
    (fs/file dir deps-file)
    deps-file))

(defn ensure-neil-project [{:keys [dir deps-file]}]
  (let [deps-file (resolve-deps-file dir deps-file)
        deps-edn (slurp deps-file)
        edn (edn/read-string deps-edn)]
    (when-not (some-> edn :aliases :neil :project)
      (let [existing-aliases (:aliases edn)
            edn-nodes (r/parse-string deps-edn)
            edn-nodes (cond-> edn-nodes
                        (not (:aliases edn))
                        (r/assoc :aliases (r/parse-string "\n {}"))
                        (contains? existing-aliases :neil)
                        (r/assoc-in [:aliases :neil] (r/parse-string "\n {}"))
                        (not (some-> edn :aliases :neil :project))
                        (r/assoc-in [:aliases :neil :project]
                                    {}))]
        (spit deps-file (str edn-nodes))))))

(defn assoc-project-meta!
  "Updates deps-file's :neil :project `k` with `v`"
  [{:keys [dir deps-file k v]
    :as opts}]
  (ensure-neil-project opts)
  (let [deps-file (resolve-deps-file dir deps-file)
        deps-edn (slurp deps-file)
        nodes (r/parse-string deps-edn)
        nodes (r/assoc-in nodes [:aliases :neil :project k] v)]
    (spit deps-file (str nodes))))

(defn project-name [{:keys [deps-file]}]
  (-> (edn/read-string (slurp deps-file))
      :aliases :neil :project :name))

(defn coerce-project-name [pn]
  (let [sym (symbol pn)]
    (if (qualified-symbol? sym)
      sym
      (symbol (str pn) (str pn)))))
