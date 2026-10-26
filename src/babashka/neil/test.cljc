(ns babashka.neil.test
  (:require
   #?(:bb [babashka.deps :as deps])
   [babashka.fs :as fs]
   [babashka.process :refer [shell]]
   [clojure.edn :as edn]))

(def neil-test-spec {:dirs {:coerce [] :ref "<dir>" :desc "Directory to search for tests. Defaults to test."}
                     :nses {:coerce [] :ref "<ns>" :desc "Namespace to test."}
                     :patterns {:coerce [] :ref "<regex>" :desc "Regex of namespaces to test."}
                     :vars {:coerce [] :ref "<var>" :desc "Fully qualified var to test."}
                     :only {:coerce :symbol :ref "<sym>" :desc "Var or namespace to test."}
                     :includes {:coerce [] :ref "<kw>" :desc "Runs only tests with metadata <kw>."}
                     :excludes {:coerce [] :ref "<kw>" :desc "Skips tests with metadata <kw>."}
                     :test-help {:coerce :boolean :desc "Prints the help of the test runner."}})

(def neil-test-aliases
  {:d :dirs
   :n :nses
   :r :patterns
   :v :vars
   :i :includes
   :e :excludes
   :H :test-help})

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

(defn test-alias?
  "Returns true if deps-file has a :test alias."
  [deps-file]
  (and (fs/exists? deps-file)
       (some-> (edn/read-string (slurp deps-file)) :aliases :test)))

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
