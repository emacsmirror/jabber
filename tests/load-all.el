;; Test that all files can be loaded  -*- lexical-binding: t; -*-

(let* ((default-directory (expand-file-name (getenv "top_builddir")))
       (elc-files (file-expand-wildcards "*.elc" t)))
  (dolist (f elc-files)
    (load f nil t)))
