(ns babashka.neil.meta)

;; This is a fallback if the var hasn't already been set earlier by a build
;; script, such as when neil is called as a library instead of a script.
(defonce version :version-not-set)
