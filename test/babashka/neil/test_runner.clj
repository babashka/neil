(ns babashka.neil.test-runner
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(load-file (str (fs/parent *file*) "/../../../prelude"))
(load-file (str (fs/parent *file*) "/../../../tests.clj"))

(defmethod clojure.test/report :begin-test-var [m]
  (println "===" (-> m :var meta :name))
  (println))

(defmethod clojure.test/report :end-test-var [_m]
  (when-let [rc t/*report-counters*]
    (let [{:keys [:fail :error]} @rc]
      (when (and (= "true" (System/getenv "BABASHKA_FAIL_FAST"))
                 (or (pos? fail) (pos? error)))
        (println "=== Failing fast")
        (System/exit 1)))))

(require '[babashka.cli] :reload) ;; workaround for bug in built-in clj - this can be removed once bvb 1.0.170 is released

(def test-namespaces
  '[tests
    babashka.neil.version-test
    babashka.neil.dep-upgrade-test])

(doseq [ns test-namespaces]
  (require ns))

(defn run-tests [& {:keys [nses]}]
  (let [selected-tests (if nses
                         (edn/read-string nses)
                         test-namespaces)
        test-results (apply t/run-tests selected-tests)
        {:keys [:fail :error]} test-results]
    (when (pos? (+ fail error))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
