(ns babashka.neil.version-test
  (:require [babashka.fs :as fs]
            [babashka.neil.test-util :refer [neil test-file test-dir
                                             ensure-git-repo]]
            [clojure.test :refer [deftest is]]))

(deftest version-option-test
  (let [{:keys [out]} (neil "--version" :out :string)]
    (is (re-seq #"^neil \d+\.\d+\.\d+(-\w+)?\n$" out))))

(deftest root-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (let [{:keys [out]} (neil "version")]
    (is (= {:project {:version nil}} out))))


(deftest bump-test
  (fs/delete-tree test-dir)
  (spit (test-file "deps.edn") "{}")
  (ensure-git-repo)
  (clojure.pprint/pprint (map str (file-seq (fs/file test-dir))))
  (println "version minor")
  (let [{:keys [out]} (neil "version minor")]
    (is (= {:before {:project {:version nil}}
            :after {:project {:version {:major 0 :minor 1 :patch 0}}}}
           out)))
  (println "version patch")
  (let [{:keys [out]} (neil "version patch")]
    (is (= {:before {:project {:version {:major 0 :minor 1 :patch 0}}}
            :after {:project {:version {:major 0 :minor 1 :patch 1}}}}
           out)))
  (println "version minor 3")
  (let [{:keys [out]} (neil "version minor 3")]
    (is (= {:before {:project {:version {:major 0 :minor 1 :patch 1}}}
            :after {:project {:version {:major 0 :minor 3 :patch 0}}}}
           out)))
  (println "version major")
  (let [{:keys [out]} (neil "version major")]
    (is (= {:before {:project {:version {:major 0 :minor 3 :patch 0}}}
            :after {:project {:version {:major 1 :minor 0 :patch 0}}}}
           out)))
  (println "version")
  (let [{:keys [out]} (neil "version")]
    (is (= {:project {:version {:major 1 :minor 0 :patch 0}}}
           out))))

(comment
  (clojure.test/run-tests))
