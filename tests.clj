(ns tests
  (:require
   [babashka.fs :as fs]
   [babashka.process :refer [check process tokenize]]
   [babashka.tasks :as tasks]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is]]))

(defn neil [arg & args]
  (let [tmp-file (doto (fs/file (fs/temp-dir) "neil"  "deps.edn")
                   (-> fs/parent (fs/create-dirs))
                   (fs/delete-on-exit))]
    (apply tasks/shell "./neil"
           (concat (tokenize arg) [:deps-file tmp-file] args))
    (let [s (slurp tmp-file)]
      {:raw s
       :edn (edn/read-string s)})))

(deftest add-dep-test
  (let [{:keys [edn]} (neil "add dep clj-kondo/clj-kondo")]
    (is (-> edn :deps (get 'clj-kondo/clj-kondo)))))

(defn run-dep-subcommand [subcommand & args]
  (-> (process (concat ["./neil" "dep" subcommand] args) {:out :string})
      check :out str/split-lines))

(defn run-dep-versions [lib & args]
  (apply run-dep-subcommand "versions" lib args))

(deftest dep-versions-test
  (is (seq (run-dep-versions 'org.clojure/clojure))
      "We're able to find at least one Clojure version")
  (is (= 3
         (count (run-dep-versions 'hiccup/hiccup :limit 3)))
      "We're able to find exactly 3 hiccup versions"))


(when (= *file* (System/getProperty "babashka.file"))
  (t/run-tests *ns*))
