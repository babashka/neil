(ns babashka.neil.test
  (:require
   #?(:bb [babashka.deps :as deps])
   [babashka.fs :as fs]
   [babashka.process :refer [shell]]
   [clojure.edn :as edn]))

(def neil-test-spec {:dirs {:coerce []}
                     :nses {:coerce []}
                     :patterns {:coerce []}
                     :vars {:coerce []}
                     :only {:coerce :symbol}
                     :includes {:coerce []}
                     :excludes {:coerce []}})

(def neil-test-aliases
  {:dir :dirs
   :d :dirs
   :namespace :nses
   :n :nses
   :r :patterns
   :namespace-regex :patterns
   :v :vars
   :var :vars
   :include :includes
   :i :includes
   :exclude :includes
   :e :includes
   :H :test-help
   :help :test-help
   :h :test-help})

(defn clojure [& args]
  #?(:bb (let [exit (:exit @(deps/clojure (vec args) {:inherit true}))]
           (when (not (zero? exit))
             (System/exit exit)))
     :clj (apply shell {} "bb clojure" args)))

(defn normalize-opts [opts]
  (if-let [only (:only opts)]
    (if (simple-symbol? only)
      (assoc opts :nses [only])
      (assoc opts :vars [only]))
    opts))

(defn neil-test [opts]
  (let [opts (normalize-opts opts)]
    (if (:test-help opts)
      (do
        (clojure "-M:test" "--test-help")
        (println)
        (println "Additional options supported by neil:")
        (println "  --only: a symbol denoting a var or namespace"))
      (let [deps-file (:deps-file opts)]
        (if (fs/exists? deps-file)
          (let [deps-edn (edn/read-string (slurp deps-file))]
            (if (some-> deps-edn :aliases :test)
              (clojure "-X:test" opts)
              (println "[neil] First execute: neil add test")))
          (println "[neil] Not inside a deps.edn project."))))))
