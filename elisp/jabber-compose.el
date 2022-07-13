;;;###autoload
(defun jabber-compose (jc &optional recipient)
  "Create a buffer for composing a Jabber message.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "To whom? ")))

  (with-current-buffer (get-buffer-create
			(generate-new-buffer-name
			 (concat
			  "Jabber-Compose"
			  (when recipient
			    (format "-%s" (jabber-jid-displayname recipient))))))
    (set (make-local-variable 'jabber-widget-alist) nil)
    (setq jabber-buffer-connection jc)
    (use-local-map widget-keymap)

    (insert (jabber-propertize "Compose Jabber message\n" 'face 'jabber-title-large))

    (insert (substitute-command-keys "\\<widget-field-keymap>Completion available with \\[widget-complete].\n"))
    (push (cons :recipients
		(widget-create '(repeat :tag "Recipients" jid)
			       :value (when recipient
					(list recipient))))
	  jabber-widget-alist)

    (insert "\nSubject: ")
    (push (cons :subject
		(widget-create 'editable-field :value ""))
	  jabber-widget-alist)

    (insert "\nText:\n")
    (push (cons :text
		(widget-create 'text :value ""))
	  jabber-widget-alist)

    (insert "\n")
    (widget-create 'push-button :notify #'jabber-compose-send "Send")

    (widget-setup)

    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

(defun jabber-compose-send (&rest _ignore)
  (let ((recipients (widget-value (cdr (assq :recipients jabber-widget-alist))))
	(subject (widget-value (cdr (assq :subject jabber-widget-alist))))
	(text (widget-value (cdr (assq :text jabber-widget-alist)))))
    (when (null recipients)
      (error "No recipients specified"))

    (dolist (to recipients)
      (jabber-send-message jabber-buffer-connection to subject text nil))

    (bury-buffer)
    (message "Message sent")))
