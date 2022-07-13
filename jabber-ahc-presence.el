(defconst jabber-ahc-presence-node "http://jabber.org/protocol/rc#set-status"
  "Node used by function `jabber-ahc-presence'.")

(jabber-ahc-add jabber-ahc-presence-node "Set presence" 'jabber-ahc-presence
		'jabber-my-jid-p)

(defun jabber-ahc-presence (jc xml-data)
  "Process presence change command.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let* ((query (jabber-iq-query xml-data))
	 (sessionid (jabber-xml-get-attribute query 'sessionid))
	 (action (jabber-xml-get-attribute query 'action)))
    ;; No session state is kept; instead, lack of session-id is used
    ;; as indication of first command.
    (cond
     ;; command cancelled
     ((string= action "cancel")
      `(command ((xmlns . "http://jabber.org/protocol/commands")
		 (sessionid . ,sessionid)
		 (node . ,jabber-ahc-presence-node)
		 (status . "canceled"))))
     ;; return form
     ((null sessionid)
      `(command ((xmlns . "http://jabber.org/protocol/commands")
		 (sessionid . "jabber-ahc-presence")
		 (node . ,jabber-ahc-presence-node)
		 (status . "executing"))
		(x ((xmlns . "jabber:x:data")
		    (type . "form"))
		   (title nil ,(format "Set presence of %s" (jabber-connection-jid jc)))
		   (instructions nil "Select new presence status.")
		   (field ((var . "FORM_TYPE") (type . "hidden"))
			  (value nil "http://jabber.org/protocol/rc"))
		   (field ((var . "status")
			   (label . "Status")
			   (type . "list-single"))
			  (value nil ,(if (string= *jabber-current-show* "")
					  "online"
					*jabber-current-show*))
			  (option ((label . "Online")) (value nil "online"))
			  (option ((label . "Chatty")) (value nil "chat"))
			  (option ((label . "Away")) (value nil "away"))
			  (option ((label . "Extended away")) (value nil "xa"))
			  (option ((label . "Do not disturb")) (value nil "dnd")))
		   (field ((var . "status-message")
			   (label . "Message")
			   (type . "text-single"))
			  (value nil ,*jabber-current-status*))
		   (field ((var . "status-priority")
			   (label . "Priority")
			   (type . "text-single"))
			  (value nil ,(int-to-string *jabber-current-priority*))))))
     ;; process form
     (t
      (let* ((x (car (jabber-xml-get-children query 'x)))
	;; we assume that the first <x/> is the jabber:x:data one
	     (fields (jabber-xml-get-children x 'field))
	     (new-show *jabber-current-show*)
	     (new-status *jabber-current-status*)
	     (new-priority *jabber-current-priority*))
	(dolist (field fields)
	  (let ((var (jabber-xml-get-attribute field 'var))
		;; notice that multi-value fields won't be handled properly
		;; by this
		(value (car (jabber-xml-node-children (car (jabber-xml-get-children field 'value))))))
	    (cond
	     ((string= var "status")
	      (setq new-show (if (string= value "online")
				 ""
			       value)))
	     ((string= var "status-message")
	      (setq new-status value))
	     ((string= var "status-priority")
	      (setq new-priority (string-to-number value))))))
	(jabber-send-presence new-show new-status new-priority))
      `(command ((xmlns . "http://jabber.org/protocol/commands")
		 (sessionid . ,sessionid)
		 (node . ,jabber-ahc-presence-node)
		 (status . "completed"))
		(note ((type . "info")) "Presence has been changed."))))))
