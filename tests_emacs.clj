(ns tests-emacs
  (:require [babashka.tasks :as tasks]))

(def install-buttercup-routine
  (str
   "(progn"
   " (require (quote package))"
   " (add-to-list (quote package-archives)"
   "  (quote (\\\"melpa-stable\\\". \\\"http://stable.melpa.org/packages\\\")) t)"
   " (package-install (quote buttercup)))"))

(defn install-buttercup []
  (tasks/shell (format "emacs -Q --batch --eval \"%s\"" install-buttercup-routine)))

(defn run-tests []
  (install-buttercup)
  (tasks/shell (format "emacs --batch --funcall package-initialize --directory . --funcall buttercup-run-discover")))
