(ns tests-emacs
  (:require [babashka.tasks :as tasks]
            [clojure.string :as str]))

(def install-buttercup-routine
  '(progn
    (require (quote package))
    (add-to-list (quote package-archives)
                 (quote ("melpa-stable" . "http://stable.melpa.org/packages")) t)
    (package-install (quote buttercup))))

(defn install-buttercup []
  (let [cmd (-> install-buttercup-routine str (str/escape {\" "\\\""}))]
    (tasks/shell (format "emacs -Q --batch --eval \"%s\"" cmd))))

(defn run-tests []
  (install-buttercup)
  (tasks/shell (format "emacs --batch --funcall package-initialize --directory . --funcall buttercup-run-discover")))
