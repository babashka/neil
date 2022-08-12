(ns babashka.neil.gen-script
  (:require [clojure.string :as str]))

(defn gen-script []
  (let [prelude (slurp "prelude")
        curl (slurp "src/babashka/neil/curl.clj")
        git (slurp "src/babashka/neil/git.clj")
        new (slurp "src/babashka/neil/new.clj")
        test (slurp "src/babashka/neil/test.cljc")
        project (slurp "src/babashka/neil/project.clj")
        rewrite (slurp "src/babashka/neil/rewrite.clj")
        neil (slurp "src/babashka/neil.clj")]
    (spit "neil" (str/join "\n" [prelude
                                 curl git rewrite project new test
                                 neil]))))
