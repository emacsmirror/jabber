(unless (fboundp 'sha1)
  (require 'sha1))

(defun jabber-get-auth (jc to session-id)
  "Send IQ get request in namespace \"jabber:iq:auth\".
JC is the Jabber connection."
  (jabber-send-iq jc to
		  "get"
		  `(query ((xmlns . "jabber:iq:auth"))
			  (username () ,(plist-get (fsm-get-state-data jc) :username)))
		  #'jabber-do-logon session-id
		  #'jabber-report-success "Impossible error - auth field request"))

(defun jabber-do-logon (jc xml-data session-id)
  "Send username and password in logon attempt.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((digest-allowed (jabber-xml-get-children (jabber-iq-query xml-data) 'digest))
         (passwd (when
		     (or digest-allowed
			 (plist-get (fsm-get-state-data jc) :encrypted)
			 (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? "))
		   (or (plist-get (fsm-get-state-data jc) :password)
		       (jabber-read-password (jabber-connection-bare-jid jc)))))
         auth)
    (if (null passwd)
        (fsm-send jc :authentication-failure)
      (if digest-allowed
          (setq auth `(digest () ,(sha1 (concat session-id passwd))))
        (setq auth `(password () ,passwd)))
      ;; For legacy authentication we must specify a resource.
      (unless (plist-get (fsm-get-state-data jc) :resource)
	;; Yes, this is ugly.  Where is my encapsulation?
	(plist-put (fsm-get-state-data jc) :resource "emacs-jabber"))
      (jabber-send-iq jc (plist-get (fsm-get-state-data jc) :server)
		"set"
		`(query ((xmlns . "jabber:iq:auth"))
			(username () ,(plist-get (fsm-get-state-data jc) :username))
			,auth
			(resource () ,(plist-get (fsm-get-state-data jc) :resource)))
		#'jabber-process-logon passwd
		#'jabber-process-logon nil))))

(defun jabber-process-logon (jc xml-data closure-data)
  "Receive login success or failure, and request roster.
CLOSURE-DATA should be the password on success and nil on failure.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (if closure-data
      ;; Logon success
      (fsm-send jc (cons :authentication-success closure-data))

    ;; Logon failure
    (jabber-report-success jc xml-data "Logon")
    (fsm-send jc :authentication-failure)))
