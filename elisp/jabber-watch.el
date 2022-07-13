(defcustom jabber-watch-alist nil
  "Alist of buddies for which an extra notification should be sent
when they come online, with comment strings as values."
  ;; XXX: change symbol to jid-symbol or something, and update
  ;; documentation
  :type '(alist :key-type symbol :value-type string)
  :group 'jabber-watch)

(defun jabber-presence-watch (who oldstatus newstatus
				  statustext proposed-alert)
  "Send a message if one of your extra-important buddies comes online.
The buddies are stored in `jabber-watch-alist' and are added and removed by
calling `jabber-watch-add' and `jabber-watch-remove'."
  ;; check that buddy was previously offline and now online
  (if (and (null oldstatus)
           (not (null newstatus)))
      (let ((entry (assq who jabber-watch-alist)))
	(when entry
	  ;; Give an intrusive message.  With a window system,
	  ;; that's easy.
	  (if window-system
	      (message-box "%s%s" proposed-alert
			   (if (cdr entry) (format ": %s" (cdr entry)) ""))
	    ;; Without a window system, yes-or-no-p should be
	    ;; sufficient.
	    (while (not
		    (yes-or-no-p (format "%s%s  Got that? " proposed-alert
					 (if (cdr entry) (format ": %s" (cdr entry)) ""))))))))))

(defun jabber-watch-add (buddy &optional comment)
  (interactive (list (jabber-read-jid-completing "Add buddy to watch list: ")
		     (read-string "Comment: ")))
  (unless (memq 'jabber-presence-watch jabber-presence-hooks)
    (error "The jabber-presence-watch function is not in jabber-presence-hooks"))
  (add-to-list 'jabber-watch-alist (cons
				    (jabber-jid-symbol buddy)
				    (and (not (zerop (length comment)))
					 comment))))

(defun jabber-watch-remove (buddy)
  (interactive
   (list (jabber-read-jid-completing "Remove buddy from watch list: "
				     (or (mapcar 'car jabber-watch-alist)
					 (error "Watch list is empty"))
				     t)))
  (setq jabber-watch-alist
        (delq (assq (jabber-jid-symbol buddy) jabber-watch-alist)
	      jabber-watch-alist)))
