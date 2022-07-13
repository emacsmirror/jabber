(defgroup jabber-mode-line nil
  "Display Jabber status in mode line"
  :group 'jabber)

(defcustom jabber-mode-line-compact t
  "Count contacts in fewer categories for compact view."
  :group 'jabber-mode-line
  :type 'boolean)

(defvar jabber-mode-line-string nil)

(defvar jabber-mode-line-presence nil)

(defvar jabber-mode-line-contacts nil)

(defadvice jabber-send-presence (after jsp-update-mode-line
				       (show status priority))
  (jabber-mode-line-presence-update))

(defun jabber-mode-line-presence-update ()
  (setq jabber-mode-line-presence (if (and jabber-connections (not *jabber-disconnecting*))
				      (cdr (assoc *jabber-current-show* jabber-presence-strings))
				    "Offline")))

(defun jabber-mode-line-count-contacts (&rest _ignore)
  (let ((count (list (cons "chat" 0)
		     (cons "" 0)
		     (cons "away" 0)
		     (cons "xa" 0)
		     (cons "dnd" 0)
		     (cons nil 0))))
    (dolist (jc jabber-connections)
      (dolist (buddy (plist-get (fsm-get-state-data jc) :roster))
	(when (assoc (get buddy 'show) count)
	  (cl-incf (cdr (assoc (get buddy 'show) count))))))
    (setq jabber-mode-line-contacts
	  (if jabber-mode-line-compact
	      (format "(%d/%d/%d)"
		      (+ (cdr (assoc "chat" count))
			 (cdr (assoc "" count)))
		      (+ (cdr (assoc "away" count))
			 (cdr (assoc "xa" count))
			 (cdr (assoc "dnd" count)))
		      (cdr (assoc nil count)))
	    (apply 'format "(%d/%d/%d/%d/%d/%d)"
		   (mapcar 'cdr count))))))

(define-minor-mode jabber-mode-line-mode
  "Toggle display of Jabber status in mode lines.
Display consists of your own status, and six numbers
meaning the number of chatty, online, away, xa, dnd
and offline contacts, respectively."
  :global t :group 'jabber-mode-line
  (setq jabber-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (if jabber-mode-line-mode
      (progn
	(add-to-list 'global-mode-string 'jabber-mode-line-string t)

	(setq jabber-mode-line-string (list " "
					    'jabber-mode-line-presence
					    " "
					    'jabber-mode-line-contacts))
        (put 'jabber-mode-line-string 'risky-local-variable t)
        (put 'jabber-mode-line-presence 'risky-local-variable t)
	(jabber-mode-line-presence-update)
	(jabber-mode-line-count-contacts)
	(ad-activate 'jabber-send-presence)
	(add-hook 'jabber-post-disconnect-hook
		  'jabber-mode-line-presence-update)
	(add-hook 'jabber-presence-hooks
		  'jabber-mode-line-count-contacts))))
