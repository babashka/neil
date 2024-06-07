(ns tests
  (:require
   [babashka.deps :as deps]
   [babashka.fs :as fs]
   [babashka.process :as process :refer [check process tokenize]]
   [babashka.tasks :as tasks]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is testing]]))

(defn test-file [name]
  (doto (fs/file (fs/temp-dir) "neil" name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn neil [arg & args]
  (let [tmp-file (test-file "deps.edn")]
    (apply process/shell "./neil"
           (concat (tokenize arg) [:deps-file tmp-file] args))
    (let [s (slurp tmp-file)]
      {:raw s
       :edn (edn/read-string s)})))

(deftest add-dep-test
  (let [{:keys [edn]} (neil "add dep clj-kondo/clj-kondo")]
    (is (-> edn :deps (get 'clj-kondo/clj-kondo))))
  (let [{:keys [edn]} (neil "add dep clj-kondo")]
    (is (-> edn :deps (get 'clj-kondo/clj-kondo))))
  (is (thrown? Exception (neil "add dep fake-dep"))))

(deftest add-dep-alias-test
  (let [{:keys [edn]} (neil "add dep clj-kondo/clj-kondo" :alias :lint)]
    (is (-> edn :aliases :lint :extra-deps (get 'clj-kondo/clj-kondo)))))

(deftest add-dep-deps-key-test
  (let [tmp-file (test-file "deps.edn")]
    (spit tmp-file "{:deps {} :aliases {:lint {:deps {}}}}")
    (tasks/shell "./neil add dep clj-kondo/clj-kondo --alias lint --deps-file" tmp-file)
    (let [edn (edn/read-string (slurp tmp-file))]
      (is (-> edn :aliases :lint :deps (get 'clj-kondo/clj-kondo))))))

(deftest add-dep-bogus-lib-symbol-test
  (let [tmp-file      (test-file "deps.edn")
        existing-deps {'clj-kondo/clj-kondo {:tag "v2022.03.08" :sha "247e538"}}]
    (spit tmp-file (str "{:deps " existing-deps "}"))
    ;; note this is not a qualified namespace - no version will be found, it should not be added
    (tasks/shell {:continue true} "./neil add dep com.rpl.specter --deps-file" tmp-file)
    (let [edn (edn/read-string (slurp tmp-file))]
      ;; make sure existing deps were not deleted
      (is (= (-> edn :deps) existing-deps)))))

(deftest add-nrepl-test
  (let [{:keys [edn]} (neil "add nrepl")
        {:keys [main-opts extra-deps]} (-> edn :aliases :nrepl)]
    (is (get extra-deps 'nrepl/nrepl))
    (is (get extra-deps 'cider/cider-nrepl))
    (is (get extra-deps 'refactor-nrepl/refactor-nrepl))
    (is (= ["-m" "nrepl.cmdline" "--interactive" "--color"
            "--middleware" "[cider.nrepl/cider-middleware,refactor-nrepl.middleware/wrap-refactor]"]
           main-opts))))

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

(deftest dep-search-help-test
  (doseq [cmd ["./neil dep search"
               "./neil dep search --help"
               "./neil dep search foo --help"]]
    (let [{:keys [out]} @(process cmd {:out :string})]
      (is (str/starts-with? out "Usage: neil dep search ")))))

(deftest dep-search-test
  (is (thrown? java.lang.Exception (run-dep-subcommand "search" "someBougusLibThatDoesntExist")))
  (is (not-empty (run-dep-subcommand "search" "hiccups")))
  (is (some #(str/starts-with? % ":lib hiccups/hiccups")
            (run-dep-subcommand "search" "hiccups")))
  (is (some #(re-matches  #":lib hiccups/hiccups :version \"\d+(\.\d+)+\" :description .*" %)
            (run-dep-subcommand "search" "hiccups")))
  (is (some #(re-matches  #":lib macchiato/hiccups :version \"\d+(\.\d+)+\" :description .*" %)
            (run-dep-subcommand "search" "hiccups")))
  ; tests for no NPEs/json parsing exceptions
  #_(is (any? (run-dep-subcommand "search" "org.clojure/tools.cli")))
  (is (any? (run-dep-subcommand "search" "babashka nrepl")))
  (is (thrown-with-msg? Exception #"Unable to find"
                        (run-dep-subcommand "search" "%22searchTermThatIsn'tFound")))
  (is (some #(str/starts-with? % "Usage: neil dep search")
            (run-dep-subcommand "search" "42"))
      "passing non-string shows help")
  (is (some #(str/starts-with? % "Usage: neil dep search")
            (run-dep-subcommand "search" "  "))
      "passing blank string shows help")
  (is (some
       (partial
        re-matches
        (re-pattern
         (str ":lib org.clojure/clojurescript "
              ":version \"\\d+(\\.\\d+)+\" "
              ":description \"org.clojure/clojurescript on Maven\"")))
       (run-dep-subcommand "search" "\"org.clojure/clojurescript\""))))

(defn run-license [filename subcommand & [args]]
  (let [args (or args "")]
    (-> (process (concat ["./neil" "license" subcommand]
                   (tokenize args) (when filename [:file filename])) {:out :string})
      check :out str/split-lines)))

(deftest license-list-test
  (testing "list/search with no args returns lines with key and name"
    (is (every? #(re-find #"^:license.*:name" %) (run-license nil "list"))))
  (testing "search with matching term prints results"
    (is (not-empty (run-license nil "search" "license"))))
  (testing "search for non-existing license prints error"
    (is (thrown-with-msg? Exception #"No licenses" (run-license nil "search" "nonExistentLicense")))))

(deftest license-add-test
  (let [out-file (test-file "LICENSE.txt")]
    (testing "license add creates license file (:license key elided)"
      (run-license out-file "add" "epl-2.0")
      (is (str/includes? (slurp out-file) "Eclipse Public License")))
    (testing "license add creates license file (with :license key)"
      (run-license out-file "add" ":license epl-2.0")
      (is (str/includes? (slurp out-file) "Eclipse Public License")))
    (testing "missing license key errors"
      (is (thrown-with-msg? Exception #"No license key" 
            (run-license out-file "add"))))
    (testing "invalid license key errors"
      (is (thrown-with-msg? Exception #"nonExistentLicense" 
            (run-license out-file "add" "nonExistentLicense"))))))

(defn run-new-command [& args]
  (-> @(process (concat ["./neil" "new"] args) {:out :string})
      :out
      edn/read-string))

(deftest new-help-test
  (doseq [cmd ["./neil new"
               "./neil new --help"
               "./neil new scratch --help"
               "./neil new scratch my-scratch --help"]]
    (let [{:keys [out]} @(process cmd {:out :string})]
      (is (str/starts-with? out "Usage: neil new")))))

(deftest new-name-only-test
  (let [target-dir (str (fs/temp-dir) "/my-scratch")]
    (spit (test-file "deps.edn") "{}")
    (let [edn (run-new-command ":name" "foo/my-scratch"
                               ":target-dir" target-dir
                               ":dry-run" "true")]
      (is (= {:create-opts {:template "scratch"
                            :scratch 'foo/my-scratch
                            :target-dir target-dir
                            :name 'foo/my-scratch}}
             edn)))))

(deftest new-scratch-test
  (let [target-dir (str (fs/temp-dir) "/my-scratch")]
    (spit (test-file "deps.edn") "{}")
    (let [edn (run-new-command "scratch" "foo/my-scratch"
                               ":target-dir" target-dir
                               ":dry-run" "true"
                               ":overwrite" "true"
                               ":scratch" "foo/my-scratch")]
      (is (= {:create-opts {:template "scratch"
                            :scratch "foo/my-scratch"
                            :overwrite true
                            :target-dir target-dir
                            :name 'foo/my-scratch}}
             edn)))))

(deftest new-remote-test
  (let [target-dir (str (fs/temp-dir) "/my-scratch")]
    (fs/delete-tree target-dir)
    (spit (test-file "deps.edn") "{}")
    (let [run #(apply run-new-command
                      "io.github.rads/neil-new-test-template" "my-scratch"
                      ":target-dir" target-dir
                      ":overwrite" "true"
                      %&)]
      (testing "dry run"
        (is (= {:template-deps {'io.github.rads/neil-new-test-template
                                {:git/url "https://github.com/rads/neil-new-test-template"
                                 :git/tag "1.0.0"
                                 :git/sha "e7954c34146fcdc4ab54fa4690bec3ceb9247d05"}}
                :create-opts {:template "io.github.rads/neil-new-test-template"
                              :target-dir target-dir
                              :overwrite true
                              :name 'my-scratch/my-scratch}}
               (run ":dry-run" "true"))))
      (testing "template output"
        (run ":dry-run" "false")
        (is (= (slurp (fs/file "test-resources/new/my-scratch/src/scratch.clj"))
               (slurp (fs/file (str target-dir "/src/scratch.clj")))))
        (is (= (edn/read-string (slurp (fs/file "test-resources/new/my-scratch/deps.edn")))
               (edn/read-string (slurp (fs/file (str target-dir "/deps.edn"))))))))))

(deftest clj-neil-new-test
  (let [{:keys [out err]} @(deps/clojure ["-M:neil" "new" "--help"]
                                         {:dir "tests-clj"
                                          :out :string
                                          :err :string})]
    (when (seq err) (throw (ex-info err {})))
    (is (str/starts-with? out "Usage: neil new "))))

(when (= *file* (System/getProperty "babashka.file"))
  (t/run-tests *ns*))
