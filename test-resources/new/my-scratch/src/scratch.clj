(ns scratch
  "FIXME: my new io.github.rads/neil-new-test-template project.")

(defn exec
  "Invoke me with clojure -X scratch/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m scratch"
  [& args]
  (println "-main with" args))
