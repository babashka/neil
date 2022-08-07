(ns babashka.neil.version-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [babashka.tasks :as tasks]
            [babashka.neil :as neil-main]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

(defn- test-file [name]
  (doto (fs/file (fs/temp-dir) "neil" name)
    (-> fs/parent (fs/create-dirs))
    (fs/delete-on-exit)))

(defn- neil [cli-args & {:keys [out] :or {out :edn}}]
  (let [deps-file (str (test-file "deps.edn"))
        cli-args' (concat (process/tokenize cli-args) [:deps-file deps-file])]
    (binding [*command-line-args* cli-args']
      (let [s (with-out-str (tasks/exec `neil-main/-main))]
        {:out (if (#{:edn} out) (edn/read-string s) s)}))))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (re-seq #"^neil \d+\.\d+\.\d+(-\w+)?\n$" out))))

(deftest root-test
  (spit (test-file "deps.edn") "{}")
  (let [{:keys [out]} (neil "version")]
    (is (= {:project {:version nil}} out))))

(deftest bump-test
  (spit (test-file "deps.edn") "{}")
  (let [{:keys [out]} (neil "version minor")]
    (is (= {:before {:project {:version nil}}
            :after {:project {:version {:major 0 :minor 1 :patch 0}}}}
           out)))
  (let [{:keys [out]} (neil "version patch")]
    (is (= {:before {:project {:version {:major 0 :minor 1 :patch 0}}}
            :after {:project {:version {:major 0 :minor 1 :patch 1}}}}
           out)))
  (let [{:keys [out]} (neil "version minor 3")]
    (is (= {:before {:project {:version {:major 0 :minor 1 :patch 1}}}
            :after {:project {:version {:major 0 :minor 3 :patch 0}}}}
           out)))
  (let [{:keys [out]} (neil "version major")]
    (is (= {:before {:project {:version {:major 0 :minor 3 :patch 0}}}
            :after {:project {:version {:major 1 :minor 0 :patch 0}}}}
           out)))
  (let [{:keys [out]} (neil "version")]
    (is (= {:project {:version {:major 1 :minor 0 :patch 0}}}
           out))))
