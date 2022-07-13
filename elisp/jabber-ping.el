(add-to-list 'jabber-jid-info-menu (cons "Ping" 'jabber-ping))

(defun jabber-ping-send (jc to process-func on-success on-error)
  "Send XEP-0199 ping IQ stanza.
JC is connection to use, TO is full JID, PROCESS-FUNC is fucntion to call to
process result, ON-SUCCESS and ON-ERROR is arg for this function depending on
result."
  (jabber-send-iq jc to "get"
                  '(ping ((xmlns . "urn:xmpp:ping")))
                  process-func on-success
                  process-func on-error))

(defun jabber-ping (to)
  "Ping XMPP entity.
TO is full JID.  All connected JIDs is used."
  (interactive (list (jabber-read-jid-completing "Send ping to: " nil nil nil 'full)))
  (dolist (jc jabber-connections)
    (jabber-ping-send jc to 'jabber-silent-process-data 'jabber-process-ping "Ping is unsupported")))

;; called by jabber-process-data
(defun jabber-process-ping (jc xml-data)
  "Handle results from ping requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((to (jabber-xml-get-attribute xml-data 'from)))
    (format "%s is alive" to)))

(add-to-list 'jabber-iq-get-xmlns-alist (cons "urn:xmpp:ping" 'jabber-pong))
(jabber-disco-advertise-feature "urn:xmpp:ping")

(defun jabber-pong (jc xml-data)
  "Return pong as defined in XEP-0199.
Sender and Id are determined from the incoming packet passed in XML-DATA.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result" nil nil nil nil nil id)))
