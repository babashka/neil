(ns tests-emacs
  (:require [babashka.tasks :as tasks]))

(def install-buttercup-routine
  (str
   (quote
    (progn
     (require 'package)
     (add-to-list
      'package-archives '("melpa-stable" . "http://stable.melpa.org/packages") t)
     (package-install 'buttercup)))))

(defn install-buttercup []
  (tasks/shell (format "emacs -Q --batch --eval '%s'" install-buttercup-routine)))

(defn run-tests []
  (install-buttercup)
  (tasks/shell (format "emacs --batch --funcall package-initialize --directory . --funcall buttercup-run-discover")))
