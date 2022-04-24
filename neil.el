;;; neil.el --- Emacs companion for Babashka Neil -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Michiel Borkent
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: April 20, 2022
;; Modified: April 20, 2022
;; Version: 0.0.1
;; Keywords: convenience tools
;; Homepage: https://github.com/babashka/neil
;; Package-Requires: ((emacs "26.1"))
;;

;;; Commentary:
;; This file is part of babashka/neil project.
;;
;;  Description
;;
;; The package expects neil command-line tool installed
;;

;;; Code:

(defcustom neil-prompt-for-version-p t
  "When t, select from available versions of a lib.
Otherwise, use the latest found."
  :type 'boolean
  :group 'neil)

(defcustom neil-inject-dep-to-project-p nil
  "When t, try to add library dependency to current project.
Otherwise, simply store the dependency string in the `kill-ring'.
Works only for deps.edn projects."
  :type 'boolean
  :group 'neil)

(defun neil--identify-project-build-tool ()
  "Find build tools used in the project."
  (let* ((default-directory (or (when (boundp 'projectile-project-root)
                                  (funcall #'projectile-project-root))
                                (when (boundp 'clojure-project-dir)
                                  (funcall #'clojure-project-dir))))
         (build-files '((clojure-cli . "deps.edn")
                        (lein . "project.clj")
                        (boot . "build.boot")
                        (shadow-cljs . "shadow-cljs.edn"))))
    (delq nil
          (mapcar (lambda (candidate)
                    (when (file-exists-p (cdr candidate))
                      (car candidate)))
                  build-files))))

(defun neil-search-annotation-fn (s)
  "Annotate dependency S with its version."
  (when-let (item (assoc s minibuffer-completion-table))
    (format "  %s" (cdr item))))

(defun neil-find-clojure-package (&optional term)
  "Find Clojure dependency by suppliying TERM to neil cmd-line tool.
When `neil-prompt-for-version-p' is t - after selecting library
name lets you choose its version.
With `neil-inject-dep-to-project-p' set to t, automatically adds
the dependency to the project (deps.edn only)."
  (interactive
   (list (read-from-minibuffer "Search for Clojure libs: ")))
  (let* ((format-dep-str
          (lambda (lib-name version)
            (let ((build-tool (car (neil--identify-project-build-tool))))
              (when (and lib-name version)
                (if (or (null build-tool)
                        (eq build-tool 'clojure-cli))
                    (format "%s {:mvn/version %S}" lib-name version)
                  (format "[%s %S]" lib-name version))))))

         (perform-action
          (lambda (exe args)
            (let* ((res (shell-command-to-string
                         (format "%s %s" exe args)))
                   (res (if (or (string-match-p "Unable to find\\|Error" res))
                            (user-error res)
                          (seq-filter
                           (lambda (x) (string-match-p ":lib" x))
                           (split-string res "\n")))))
              (seq-map
               (lambda (s)
                 (when (string-match ":lib \\(.*\\) :version \\(.*\\)" s)
                   `(,(match-string 1 s) . ,(match-string 2 s))))
               res))))

         (exe (if-let ((exe (executable-find "neil")))
                  exe (user-error "Cannot find 'neil' command!")))

         (res (funcall perform-action exe (concat "dep search " (shell-quote-argument term))))
         (lib-name (let ((completion-extra-properties
                          '(:annotation-function neil-search-annotation-fn)))
                     (completing-read
                      (format "Found %s matches for '%s':" (length res) term)
                      res)))
         (version (if neil-prompt-for-version-p
                      (let ((versions (funcall perform-action exe (concat "dep versions " lib-name))))
                        (let (;; keeping the order in completion prompt is surprisingly tricky
                              (keep-order (lambda (completions)
                                            (lambda (string pred action)
                                              (if (eq action 'metadata)
                                                  `(metadata (display-sort-function . ,#'identity))
                                                (complete-with-action action completions string pred))))))
                          (completing-read
                           (format "Choose version of %s :" lib-name)
                           (funcall keep-order (seq-map 'cdr versions)))))
                    (cdr (assoc lib-name res))))
         (dep-str (funcall format-dep-str lib-name version)))

    (when (and neil-inject-dep-to-project-p
               (eq 'clojure-cli (car (neil--identify-project-build-tool))))
      (funcall
       perform-action exe
       (format "dep add :lib %s :version %s" lib-name version)))

    (kill-new dep-str)
    (message dep-str)))

(provide 'neil)

;;; neil.el ends here
