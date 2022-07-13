(add-to-list 'jabber-jid-info-menu
	     (cons "Send browse query" 'jabber-get-browse))
(defun jabber-get-browse (jc to)
  "Send a browse infoquery request to someone.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "browse: " nil nil nil nil t)))
  (jabber-send-iq jc to
                  "get"
                  '(query ((xmlns . "jabber:iq:browse")))
                  #'jabber-process-data #'jabber-process-browse
		  #'jabber-process-data "Browse failed"))

;; called from jabber-process-data
(defun jabber-process-browse (jc xml-data)
  "Handle results from jabber:iq:browse requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (dolist (item (jabber-xml-node-children xml-data))
    (when (and (listp item)
	       (not (eq (jabber-xml-node-name item) 'ns)))
      (let ((jid (jabber-xml-get-attribute item 'jid))
	    (beginning (point)))
	(cond
	 ((or
	   (eq (jabber-xml-node-name item) 'user)
	   (string= (jabber-xml-get-attribute item 'category) "user"))
	  (insert (jabber-propertize "$ USER"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (jabber-xml-node-name item) 'service)
	   (string= (jabber-xml-get-attribute item 'category) "service"))
	  (insert (jabber-propertize "* SERVICE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (jabber-xml-node-name item) 'conference)
	   (string= (jabber-xml-get-attribute item 'category) "conference"))
	  (insert (jabber-propertize "@ CONFERENCE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 (t
	  ;; So far I've seen "server" and "directory", both in the node-name.
	  ;; Those are actually service disco categories, but jabberd 2 seems
	  ;; to use them for browse results as well.  It's not right (as in
	  ;; XEP-0011), but it's reasonable.
	  (let ((category (jabber-xml-get-attribute item 'category)))
	    (if (= (length category) 0)
		(setq category (jabber-xml-node-name item)))
	    (insert (jabber-propertize (format "! OTHER: %s" category)
				'face 'jabber-title-medium)
		    "\n\n"))))
	(dolist (attr '((type . "Type:\t\t")
			(jid . "JID:\t\t")
			(name . "Name:\t\t")
			(version . "Version:\t")))
	  (let ((data (jabber-xml-get-attribute item (car attr))))
	    (if (> (length data) 0)
		(insert (cdr attr) data "\n"))))

	(dolist (ns (jabber-xml-get-children item 'ns))
	  (if (stringp (car (jabber-xml-node-children ns)))
	      (insert "Namespace:\t" (car (jabber-xml-node-children ns)) "\n")))

	(insert "\n")
	(put-text-property beginning (point) 'jabber-jid jid)
	(put-text-property beginning (point) 'jabber-account jc)

	;; XXX: Is this kind of recursion really needed?
	(if (listp (car (jabber-xml-node-children item)))
	    (jabber-process-browse jc item))))))
