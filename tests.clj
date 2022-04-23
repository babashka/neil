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

(deftest dep-search-test
  (is (thrown? java.lang.Exception (run-dep-subcommand "search" "someBougusLibThatDoesntExist")))
  (is (not-empty (run-dep-subcommand "search" "hiccups")))
  (is (some #(str/starts-with? % ":lib hiccups/hiccups" )
            (run-dep-subcommand "search" "hiccups")))
  (is (some #(re-matches  #":lib hiccups/hiccups :version \d+(\.\d+)+" % )
            (run-dep-subcommand "search" "hiccups")))
  (is (some #(re-matches  #":lib macchiato/hiccups :version \d+(\.\d+)+" % )
            (run-dep-subcommand "search" "hiccups")))
  ; tests for no NPEs/json parsing exceptions
  (is (any? (run-dep-subcommand "search" "org.clojure/tools.cli")))
  (is (any? (run-dep-subcommand "search" "babashka nrepl")))
  (is (thrown-with-msg? Exception #"Unable to find"
        (run-dep-subcommand "search" "%22searchTermThatIsn'tFound"))))

(when (= *file* (System/getProperty "babashka.file"))
  (t/run-tests *ns*))
