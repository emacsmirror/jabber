(require 'jabber-ourversion)

(defcustom jabber-version-show t
  "Show our client version to others.  Acts on loading."
  :type 'boolean
  :group 'jabber)

(add-to-list 'jabber-jid-info-menu
	     (cons "Request software version" 'jabber-get-version))
(defun jabber-get-version (jc to)
  "Request software version.

JC is the Jabber connection."
  (interactive (list
		(jabber-read-account)
		(jabber-read-jid-completing "Request version of: " nil nil nil 'full t)))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:version")))
		  #'jabber-process-data #'jabber-process-version
		  #'jabber-process-data "Version request failed"))

;; called by jabber-process-data
(defun jabber-process-version (jc xml-data)
  "Handle results from jabber:iq:version requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let ((query (jabber-iq-query xml-data)))
    (dolist (x '((name . "Name:\t\t") (version . "Version:\t") (os . "OS:\t\t")))
      (let ((data (car (jabber-xml-node-children (car (jabber-xml-get-children query (car x)))))))
	(when data
	  (insert (cdr x) data "\n"))))))

(if jabber-version-show
    (and
     (add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:version" 'jabber-return-version))
     (jabber-disco-advertise-feature "jabber:iq:version")))

(defun jabber-return-version (jc xml-data)
  "Return client version as defined in XEP-0092.
Sender and ID are determined from the incoming packet passed in XML-DATA.

JC is the Jabber connection."
  ;; Things we might check: does this iq message really have type='get' and
  ;; exactly one child, namely query with xmlns='jabber:iq:version'?
  ;; Then again, jabber-process-iq should take care of that.
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id))
	(os (format "%s %d.%d (%s)"
	     (cond ((featurep 'xemacs) "XEmacs")
		   (t "Emacs"))
	     emacs-major-version emacs-minor-version
	     system-type)))
    (jabber-send-iq jc to "result"
		    `(query ((xmlns . "jabber:iq:version"))
			    (name () "jabber.el")
			    (version () ,jabber-version)
			    ;; Booting... /vmemacs.el
			    ;; Shamelessly stolen from someone's sig.
			    (os () ,os))
		    nil nil nil nil
		    id)))
