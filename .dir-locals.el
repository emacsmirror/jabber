;; `nameless-current-name' cannot be detected in Org mode buffers,
;; IIRC, and thus has to be defined explicitly. To make use of this,
;; install `nameless' and enable `nameless-mode' in Org mode buffers.
((org-mode . ((org-tags-column . -60)
              (eval . (progn
                        (make-local-variable 'before-save-hook)
                        (add-hook 'before-save-hook #'org-align-all-tags nil t))))))
