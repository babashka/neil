(ns scratch
  "FIXME: my new org.corfield.new/scratch project.")

(defn exec
  "Invoke me with clojure -X scratch/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m scratch"
  [& args]
  (println "-main with" args))
