(ns babashka.neil.common
  (:require
    [borkdude.rewrite-edn :as r]
    [clojure.string :as str]
    [babashka.fs :as fs]))

(def project-alias :neil)

(def deps-template
  (str/triml "
{:deps {}
 :aliases {}}
"))

(def bb-template
  (str/triml "
{:deps {}
 :tasks
 {
 }}
"))

(defn ensure-deps-file [opts]
  (let [target (:deps-file opts)]
    (when-not (fs/exists? target)
      (spit target (if (= "bb.edn" target)
                     bb-template
                     deps-template)))))

(defn edn-nodes [edn-string]
  (r/parse-string edn-string))

(defn edn-string [opts]
  (slurp (:deps-file opts)))
