;;; neil.el --- Elisp companion for Babashka Neil -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: April 20, 2022
;; Modified: April 20, 2022
;; Version: 0.0.1
;; Keywords: convenience tools
;; Homepage: https://github.com/agzam/neil
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

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

(defun neil-search (&optional term)
  "Find Clojure dependency by suppliying TERM to neil cmd-line tool."
  (interactive
   (list (read-from-minibuffer "Search for Clojure libs: ")))
  (let* ((exe (if-let ((exe (executable-find "neil")))
                  exe (user-error "Cannot find 'neil' command!")))
         (res (shell-command-to-string
               (concat
                exe
                " dep search "
                term)))
         (res (if (or (string-match-p "Unable to find\\|Error" res))
                  (user-error res)
                (seq-filter
                 (lambda (x) (string-match-p ":lib" x))
                 (split-string res "\n"))))
         (res (seq-map
               (lambda (s)
                 (when (string-match ":lib \\(.*\\) :version \\(.*\\)" s)
                   `(,(match-string 1 s) . ,(match-string 2 s))))
               res))
         (lib-name (let ((completion-extra-properties
                        '(:annotation-function neil-search-annotation-fn)))
                   (completing-read
                    (format "Found %s matches for '%s':" (length res) term)
                    res)))
         (version (cdr (assoc lib-name res)))
         (build-tool (car (neil--identify-project-build-tool)))
         (dep-str (when (and lib-name version build-tool)
                    (if (eq build-tool 'clojure-cli)
                        (format "%s {:mvn/version %S}" lib-name version)
                      (format "[%s %S]" lib-name version)))))
    (kill-new dep-str)
    (message dep-str)))

(provide 'neil)

;;; neil.el ends here
