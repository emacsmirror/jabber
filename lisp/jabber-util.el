;;; jabber-util.el --- various utility functions    -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2008, 2010 - Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'cl-lib)
(require 'jabber-xml)
(require 'fsm)
(require 'password-cache)

(condition-case nil
    (require 'auth-source)
  (error nil))

(defvar jabber-jid-history nil
  "History of entered JIDs.")

;; Global reference declarations

(declare-function jabber-chat-with "jabber-chat.el"
                  (jc jid &optional other-window))
(declare-function jabber-ahc-execute-command "jabber-ahc.el" (jc to node))
(declare-function jabber-get-register "jabber-register.el" (jc to))
(declare-function jabber-muc-read-my-nickname "jabber-muc.el"
                  (jc group &optional default))
(declare-function jabber-muc-join "jabber-muc.el"
                  (jc group nickname &optional popup))

;;

(defalias 'jabber-propertize 'propertize)

(defsubst jabber-read-with-input-method (prompt &optional initial-contents history default-value)
  "Like `read-string', but always inheriting the current input method."
  ;; Preserve input method when entering a minibuffer.
  (read-string prompt initial-contents history default-value t))

(unless (fboundp 'delete-and-extract-region)
  (defsubst delete-and-extract-region (start end)
    (prog1
	(buffer-substring start end)
      (delete-region start end))))

(unless (fboundp 'access-file)
  (defsubst access-file (filename error-message)
    (unless (file-readable-p filename)
      (error error-message))))

  (defalias 'jabber-float-time 'float-time)

(defalias 'jabber-cancel-timer 'cancel-timer)

(defvar jabber-connections)
(defun jabber-concat-rosters ()
  "Concatenate the rosters of all connected accounts."
  (apply #'append
	 (mapcar
	  (lambda (jc)
	    (plist-get (fsm-get-state-data jc) :roster))
	  jabber-connections)))

(defun jabber-concat-rosters-full ()
  "Concatenate the rosters of all connected accounts.
Show full JIDs, with resources."
  (let ((jids (apply #'append
                     (mapcar
                      (lambda (jc)
                        (plist-get (fsm-get-state-data jc) :roster))
                      jabber-connections))))
    (apply #'append
           (mapcar (lambda (jid)
                     (mapcar (lambda (res) (intern (format "%s/%s" jid (car res))))
                             (get (jabber-jid-symbol jid) 'resources)))
                   jids))))

(defun jabber-connection-jid (jc)
  "Return the full JID of connection JC."
  (let ((sd (fsm-get-state-data jc)))
    (concat (plist-get sd :username) "@"
	    (plist-get sd :server) "/"
	    (plist-get sd :resource))))

(defun jabber-connection-bare-jid (jc)
  "Return the bare JID of connection JC."
  (let ((sd (fsm-get-state-data jc)))
    (concat (plist-get sd :username) "@"
	    (plist-get sd :server))))

(defun jabber-connection-original-jid (jc)
  "Return the original JID of connection JC.
The \"original JID\" is the JID we authenticated with.  The
server might subsequently assign us a different JID at resource
binding."
  (plist-get (fsm-get-state-data jc) :original-jid))

(defun jabber-find-connection (bare-jid)
  "Find the connection to the account named by BARE-JID.
Return nil if none found."
  (cl-dolist (jc jabber-connections)
    (when (string= bare-jid (jabber-connection-bare-jid jc))
      (cl-return jc))))

(defun jabber-find-active-connection (dead-jc)
  "Find an active connection for dead connection DEAD-JC.
Return nil if none found."
  (let ((jid (jabber-connection-bare-jid dead-jc)))
    (jabber-find-connection jid)))

(defun jabber-jid-username (jid)
  "Return the username portion of JID, or nil if none found.
JID must be a string."
  (when (string-match "\\(.*\\)@.*\\(/.*\\)?" jid)
    (match-string 1 jid)))

(defun jabber-jid-user (jid)
  "Return the user portion (username@server) of JID.
JID must be a string."
  ;;transports don't have @, so don't require it
  ;;(string-match ".*@[^/]*" jid)
  (string-match "[^/]*" jid)
  (match-string 0 jid))

(defun jabber-jid-server (jid)
  "Return the server portion of JID."
  (string-match "^\\(.*@\\)?\\([^@/]+\\)\\(/.*\\)?$" jid)
  (match-string 2 jid))

(defun jabber-jid-rostername (user)
  "Return the name of USER if present in roster, or nil."
  (let ((user (jabber-jid-symbol user)))
    (if (> (length (get user 'name)) 0)
	(get user 'name))))

(defun jabber-jid-displayname (string)
  "Return the name of the user from STRING as in roster, else username@server."
  (or (jabber-jid-rostername string)
      (jabber-jid-user (if (symbolp string)
			   (symbol-name string)
			 string))))

(defvar jabber-bookmarks)
(defun jabber-jid-bookmarkname (string)
  "Return from STRING the conference name from boomarks or displayname.
Use the name according to roster or else the JID if none set."
  (require 'jabber-bookmarks)
  (or (cl-loop for conference
               in (car (cl-loop for value being the hash-values of jabber-bookmarks
                                collect value))
            do (let ((ls (cadr conference)))
                 (if (string= (cdr (assoc 'jid ls)) string)
                     (cl-return (cdr (assoc 'name ls))))))
      (jabber-jid-displayname string)))

(defun jabber-jid-resource (jid)
  "Return the resource portion of a JID, or nil if there is none.
JID must be a string."
  (when (string-match "^\\(\\([^/]*@\\)?[^/]*\\)/\\(.*\\)" jid)
    (match-string 3 jid)))

(defvar jabber-jid-obarray)
(defun jabber-jid-symbol (jid)
  "Return the symbol for JID, which must be a symbol or a string."
  ;; If it's already a symbol, just return it.
  (if (symbolp jid)
      jid
    ;; XXX: "downcase" is a poor man's nodeprep.  See XMPP CORE.
    (intern (downcase (jabber-jid-user jid)) jabber-jid-obarray)))

(defvar jabber-account-list)
(defun jabber-my-jid-p (jc jid)
  "Return non-nil if the specified JID is in the `jabber-account-list'.
Comment: (modulo resource).
Also return non-nil if JID matches JC, modulo resource."
  (or
   (equal (jabber-jid-user jid)
	  (jabber-connection-bare-jid jc))
   (member (jabber-jid-user jid) (mapcar (lambda (x) (jabber-jid-user (car x))) jabber-account-list))))

(defvar *jabber-active-groupchats*)
(defun jabber-read-jid-completing (prompt &optional subset require-match default resource fulljids)
  "Read a jid out of the current roster from the minibuffer.
If SUBSET is non-nil, it should be a list of symbols from which
the JID is to be selected, instead of using the entire roster.
If REQUIRE-MATCH is non-nil, the JID must be in the list used.
If DEFAULT is non-nil, it's used as the default value, otherwise
the default is inferred from context.
RESOURCE is one of the following:

nil         Accept full or bare JID, as entered
full        Turn bare JIDs to full ones with highest-priority resource
bare-or-muc Turn full JIDs to bare ones, except for in MUC

If FULLJIDS is non-nil, complete jids with resources."
  (let ((jid-at-point (or
		       (and default
			    ;; default can be either a symbol or a string
			    (if (symbolp default)
				(symbol-name default)
			      default))
                       (let* ((jid (get-text-property (point) 'jabber-jid))
                              (res (get (jabber-jid-symbol jid) 'resource)))
                         (when jid
                           (if (and fulljids res (not (jabber-jid-resource jid)))
                               (format "%s/%s" jid res)
                             jid)))
		       (bound-and-true-p jabber-chatting-with)
		       (bound-and-true-p jabber-group)))
	(completion-ignore-case t)
	(jid-completion-table (mapcar #'(lambda (item)
					  (cons (symbol-name item) item))
				      (or subset (funcall (if fulljids
                                                              'jabber-concat-rosters-full
                                                            'jabber-concat-rosters)))))
	chosen)
    (dolist (item (or subset (jabber-concat-rosters)))
      (if (get item 'name)
	  (push (cons (get item 'name) item) jid-completion-table)))
    ;; if the default is not in the allowed subset, it's not a good default
    (if (and subset (not (assoc jid-at-point jid-completion-table)))
	(setq jid-at-point nil))
    (let ((input
	   (completing-read (concat prompt
				    (if jid-at-point
					(format "(default %s) " jid-at-point)))
			    jid-completion-table
			    nil require-match nil 'jabber-jid-history jid-at-point)))
      (setq chosen
	    (if (and input (assoc-string input jid-completion-table t))
		(symbol-name (cdr (assoc-string input jid-completion-table t)))
	      (and (not (zerop (length input)))
		   input))))

    (when chosen
      (pcase resource
	('full
	 ;; If JID is bare, add the highest-priority resource.
	 (if (jabber-jid-resource chosen)
	     chosen
	   (let ((highest-resource (get (jabber-jid-symbol chosen) 'resource)))
	     (if highest-resource
		 (concat chosen "/" highest-resource)
	       chosen))))
	('bare-or-muc
	 ;; If JID is full and non-MUC, remove resource.
	 (if (null (jabber-jid-resource chosen))
	     chosen
	   (let ((bare (jabber-jid-user chosen)))
	     (if (assoc bare *jabber-active-groupchats*)
		 chosen
	       bare))))
	(_
	 chosen)))))

(defun jabber-read-node (prompt)
  "Read node name, taking default from disco item at point."
  (let ((node-at-point (get-text-property (point) 'jabber-node)))
    (read-string (concat prompt
			 (if node-at-point
			     (format "(default %s) " node-at-point)))
		 node-at-point)))

(defun jabber-password-key (bare-jid)
  "Construct key for `password' library from BARE-JID."
  (concat "xmpp:" bare-jid))

(defun jabber-read-password (bare-jid)
  "Read Jabber password from minibuffer."
  (let ((found
	 (and (fboundp 'auth-source-search)
	      (nth 0 (auth-source-search
		      :user (jabber-jid-username bare-jid)
		      :host (jabber-jid-server bare-jid)
		      :port "xmpp"
		      :max 1
		      :require '(:secret))))))
    (if found
	(let ((secret (plist-get found :secret)))
	  (copy-sequence
	   (if (functionp secret)
	       (funcall secret)
	     secret)))
      (let ((prompt (format "Jabber password for %s: " bare-jid)))
	;; Need to copy the password, as sasl.el wants to erase it.
	(copy-sequence
	 (password-read prompt (jabber-password-key bare-jid)))))))

(defun jabber-cache-password (bare-jid password)
  "Cache PASSWORD for BARE-JID."
  (password-cache-add (jabber-password-key bare-jid) password))

(defun jabber-uncache-password (bare-jid)
  "Uncache cached password for BARE-JID.
Useful if the password proved to be wrong."
  (interactive (list (jabber-jid-user
		      (completing-read "Forget password of account: " jabber-account-list nil nil nil 'jabber-account-history))))
  (password-cache-remove (jabber-password-key bare-jid)))

(defvar jabber-buffer-connection)
(defun jabber-read-account (&optional always-ask contact-hint)
  "Ask for which connected account to use.
If ALWAYS-ASK is nil and there is only one account, return that
account.
If CONTACT-HINT is a string or a JID symbol, default to an account
that has that contact in its roster."
  (let ((completions
         (mapcar (lambda (c)
                   (cons
                    (jabber-connection-bare-jid c)
                    c))
                 jabber-connections)))
    (cond
     ((null jabber-connections)
      (error "Not connected to Jabber"))
     ((and (null (cdr jabber-connections)) (not always-ask))
      ;; only one account
      (car jabber-connections))
     (t
      (or
       ;; if there is a jabber-account property at point,
       ;; present it as default value
       (cdr (assoc (let ((at-point (get-text-property (point) 'jabber-account)))
                     (when (and at-point
                                (memq at-point jabber-connections))
                       (jabber-connection-bare-jid at-point)))
                   completions))
       (let* ((default
                (or
		 (and contact-hint
		      (setq contact-hint (jabber-jid-symbol contact-hint))
		      (let ((matching
			     (cl-find-if
			      (lambda (jc)
				(memq contact-hint (plist-get (fsm-get-state-data jc) :roster)))
			      jabber-connections)))
			(when matching
			  (jabber-connection-bare-jid matching))))
                 ;; if the buffer is associated with a connection, use it
                 (when (and jabber-buffer-connection
			    (jabber-find-active-connection jabber-buffer-connection))
                   (jabber-connection-bare-jid jabber-buffer-connection))
                 ;; else, use the first connection in the list
                 (caar completions)))
              (input (completing-read
                      (concat "Select Jabber account (default "
                              default
                              "): ")
                      completions nil t nil 'jabber-account-history
                      default)))
         (cdr (assoc input completions))))))))

(defun jabber-iq-query (xml-data)
  "Return the query part of an IQ stanza.
An IQ stanza may have zero or one query child, and zero or one <error/> child.
The query child is often but not always <query/>.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let (query)
    (dolist (x (jabber-xml-node-children xml-data))
      (if (and
	   (listp x)
	   (not (eq (jabber-xml-node-name x) 'error)))
	  (setq query x)))
    query))

(defun jabber-iq-error (xml-data)
  "Return the <error/> part of an IQ stanza, if any.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (car (jabber-xml-get-children xml-data 'error)))

(defun jabber-iq-xmlns (xml-data)
  "Return the namespace of an IQ stanza, i.e. the namespace of its query part.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (jabber-xml-get-attribute (jabber-iq-query xml-data) 'xmlns))

(defun jabber-message-timestamp (xml-data)
  "Given a <message/> element, return its timestamp, or nil if none.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (jabber-x-delay
   (or
    (jabber-xml-path xml-data '(("urn:xmpp:delay" . "delay")))
    (jabber-xml-path xml-data '(("jabber:x:delay" . "x"))))))

(defun jabber-x-delay (xml-data)
  "Return timestamp given a delayed delivery element.
This can be either a <delay/> tag in namespace urn:xmpp:delay (XEP-0203), or
a <x/> tag in namespace jabber:x:delay (XEP-0091).
Return nil if no such data available.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (cond
   ((and (eq (jabber-xml-node-name xml-data) 'x)
	 (string= (jabber-xml-get-attribute xml-data 'xmlns) "jabber:x:delay"))
    (let ((stamp (jabber-xml-get-attribute xml-data 'stamp)))
      (if (and (stringp stamp)
	       (= (length stamp) 17))
	  (jabber-parse-legacy-time stamp))))
   ((and (eq (jabber-xml-node-name xml-data) 'delay)
	 (string= (jabber-xml-get-attribute xml-data 'xmlns) "urn:xmpp:delay"))
    (let ((stamp (jabber-xml-get-attribute xml-data 'stamp)))
      (when (stringp stamp)
	(jabber-parse-time stamp))))))

(defun jabber-parse-legacy-time (timestamp)
  "Parse timestamp in ccyymmddThh:mm:ss format (UTC) and return as internal
time value."
  (let ((year (string-to-number (substring timestamp 0 4)))
	(month (string-to-number (substring timestamp 4 6)))
	(day (string-to-number (substring timestamp 6 8)))
	(hour (string-to-number (substring timestamp 9 11)))
	(minute (string-to-number (substring timestamp 12 14)))
	(second (string-to-number (substring timestamp 15 17))))
    (encode-time second minute hour day month year 0)))

(defun jabber-encode-legacy-time (timestamp)
  "Parse TIMESTAMP as internal time value and encode as ccyymmddThh:mm:ss (UTC)."
  (if (featurep 'xemacs)
      ;; XEmacs doesn't have `universal' argument to format-time-string,
      ;; so we have to do it ourselves.
      (format-time-string "%Y%m%dT%H:%M:%S"
			  (time-subtract timestamp
					 (list 0 (car (current-time-zone)))))
    (format-time-string "%Y%m%dT%H:%M:%S" timestamp t)))

(defun jabber-encode-time (time)
  "Convert TIME to a string by XEP-0082.
TIME is in a format accepted by `format-time-string'."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun jabber-encode-timezone ()
  (let ((time-zone-offset (nth 0 (current-time-zone))))
    (if (null time-zone-offset)
        "Z"
      (let* ((positivep (>= time-zone-offset 0))
             (hours (/ (abs time-zone-offset) 3600))
             (minutes (/ (% (abs time-zone-offset) 3600) 60)))
        (format "%s%02d:%02d"(if positivep "+" "-") hours minutes)))))

(defun jabber-parse-time (raw-time)
  "Parse the DateTime encoded in TIME according to XEP-0082."
  (let* ((time (if (string= (substring raw-time 4 5) "-")
                   raw-time
                 (concat
                  (substring raw-time 0 4) "-"
                  (substring raw-time 4 6) "-"
                  (substring raw-time 6 (length raw-time)))))
         (year (string-to-number (substring time 0 4)))
	 (month (string-to-number (substring time 5 7)))
	 (day (string-to-number (substring time 8 10)))
	 (hour (string-to-number (substring time 11 13)))
	 (minute (string-to-number (substring time 14 16)))
	 (second (string-to-number (substring time 17 19)))
         (timezone (if (eq (aref time 19) ?.)
                       ;; fractions are optional
                       (let ((timezone (cadr
                                        (split-string (substring time 20)
                                                      "[-+Z]"))))
                         (if (string= "" timezone)
                             "Z"
                           timezone))
                     (substring time 19))))
    ;; timezone is either Z (UTC) or [+-]HH:MM
    (let ((timezone-seconds
	   (if (string= timezone "Z")
	       0
	     (* (if (eq (aref timezone 0) ?+) 1 -1)
		(* 60 (+ (* 60 (string-to-number (substring timezone 1 3)))
			 (string-to-number (substring timezone 4 6))))))))
      (encode-time second minute hour day month year timezone-seconds))))

(defun jabber-report-success (_jc xml-data context)
  "IQ callback reporting success or failure of the operation.
CONTEXT is a string describing the action.
\"CONTEXT succeeded\" or \"CONTEXT failed: REASON\" is displayed in
the echo area.
JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((type (jabber-xml-get-attribute xml-data 'type)))
    (message (concat context
		     (if (string= type "result")
			 " succeeded"
		       (concat
			" failed: "
			(let ((the-error (jabber-iq-error xml-data)))
			  (if the-error
			      (jabber-parse-error the-error)
			    "No error message given"))))))))

(defconst jabber-error-messages
  (list
   (cons 'bad-request "Bad request")
   (cons 'conflict "Conflict")
   (cons 'feature-not-implemented "Feature not implemented")
   (cons 'forbidden "Forbidden")
   (cons 'gone "Gone")
   (cons 'internal-server-error "Internal server error")
   (cons 'item-not-found "Item not found")
   (cons 'jid-malformed "JID malformed")
   (cons 'not-acceptable "Not acceptable")
   (cons 'not-allowed "Not allowed")
   (cons 'not-authorized "Not authorized")
   (cons 'payment-required "Payment required")
   (cons 'recipient-unavailable "Recipient unavailable")
   (cons 'redirect "Redirect")
   (cons 'registration-required "Registration required")
   (cons 'remote-server-not-found "Remote server not found")
   (cons 'remote-server-timeout "Remote server timeout")
   (cons 'resource-constraint "Resource constraint")
   (cons 'service-unavailable "Service unavailable")
   (cons 'subscription-required "Subscription required")
   (cons 'undefined-condition "Undefined condition")
   (cons 'unexpected-request "Unexpected request"))
  "String descriptions of XMPP stanza errors.")

(defconst jabber-legacy-error-messages
  (list
   (cons 302 "Redirect")
   (cons 400 "Bad request")
   (cons 401 "Unauthorized")
   (cons 402 "Payment required")
   (cons 403 "Forbidden")
   (cons 404 "Not found")
   (cons 405 "Not allowed")
   (cons 406 "Not acceptable")
   (cons 407 "Registration required")
   (cons 408 "Request timeout")
   (cons 409 "Conflict")
   (cons 500 "Internal server error")
   (cons 501 "Not implemented")
   (cons 502 "Remote server error")
   (cons 503 "Service unavailable")
   (cons 504 "Remote server timeout")
   (cons 510 "Disconnected"))
  "String descriptions of legacy errors (XEP-0086).")

(defun jabber-parse-error (error-xml)
  "Parse the given <error/> tag and return a string fit for human consumption.
See secton 9.3, Stanza Errors, of XMPP Core, and XEP-0086, Legacy Errors."
  (let ((error-type (jabber-xml-get-attribute error-xml 'type))
	(error-code (jabber-xml-get-attribute error-xml 'code))
	condition text)
    (if error-type
	;; If the <error/> tag has a type element, it is new-school.
	(dolist (child (jabber-xml-node-children error-xml))
	  (when (string=
		 (jabber-xml-get-attribute child 'xmlns)
		 "urn:ietf:params:xml:ns:xmpp-stanzas")
	    (if (eq (jabber-xml-node-name child) 'text)
		(setq text (car (jabber-xml-node-children child)))
	      (setq condition
		    (or (cdr (assq (jabber-xml-node-name child) jabber-error-messages))
			(symbol-name (jabber-xml-node-name child)))))))
      (setq condition (or (cdr (assq (string-to-number error-code) jabber-legacy-error-messages))
			  error-code))
      (setq text (car (jabber-xml-node-children error-xml))))
    (concat condition
	    (if text (format ": %s" text)))))

(defun jabber-error-condition (error-xml)
  "Parse the given <error/> tag and return the condition symbol."
  (catch 'condition
    (dolist (child (jabber-xml-node-children error-xml))
      (when (string=
		 (jabber-xml-get-attribute child 'xmlns)
		 "urn:ietf:params:xml:ns:xmpp-stanzas")
	(throw 'condition (jabber-xml-node-name child))))))

(defvar jabber-stream-error-messages
  (list
   (cons 'bad-format "Bad XML format")
   (cons 'bad-namespace-prefix "Bad namespace prefix")
   (cons 'conflict "Conflict")
   (cons 'connection-timeout "Connection timeout")
   (cons 'host-gone "Host gone")
   (cons 'host-unknown "Host unknown")
   (cons 'improper-addressing "Improper addressing") ; actually only s2s
   (cons 'internal-server-error "Internal server error")
   (cons 'invalid-from "Invalid from")
   (cons 'invalid-id "Invalid id")
   (cons 'invalid-namespace "Invalid namespace")
   (cons 'invalid-xml "Invalid XML")
   (cons 'not-authorized "Not authorized")
   (cons 'policy-violation "Policy violation")
   (cons 'remote-connection-failed "Remote connection failed")
   (cons 'resource-constraint "Resource constraint")
   (cons 'restricted-xml "Restricted XML")
   (cons 'see-other-host "See other host")
   (cons 'system-shutdown "System shutdown")
   (cons 'undefined-condition "Undefined condition")
   (cons 'unsupported-encoding "Unsupported encoding")
   (cons 'unsupported-stanza-type "Unsupported stanza type")
   (cons 'unsupported-version "Unsupported version")
   (cons 'xml-not-well-formed "XML not well formed"))
  "String descriptions of XMPP stream errors.")

(defun jabber-stream-error-condition (error-xml)
  "Return the condition of a <stream:error/> tag."
  ;; as we don't know the node name of the condition, we have to
  ;; search for it.
  (cl-dolist (node (jabber-xml-node-children error-xml))
    (when (and (string= (jabber-xml-get-attribute node 'xmlns)
			"urn:ietf:params:xml:ns:xmpp-streams")
	       (assq (jabber-xml-node-name node)
		     jabber-stream-error-messages))
      (cl-return (jabber-xml-node-name node)))))

(defun jabber-parse-stream-error (error-xml)
  "Parse the given error tag and return a string fit for human consumption.
ERROR-XML is a <stream:error/> tag parsed with `xml-parse-region'."
  (let ((text-node (car (jabber-xml-get-children error-xml 'text)))
	(condition (jabber-stream-error-condition error-xml)))
    (concat (if condition (cdr (assq condition jabber-stream-error-messages))
	      "Unknown stream error")
	    (if (and text-node (stringp (car (jabber-xml-node-children text-node))))
		(concat ": " (car (jabber-xml-node-children text-node)))))))

(put 'jabber-error
     'error-conditions
     '(error jabber-error))
(put 'jabber-error
     'error-message
     "Jabber error")


;; https://www.rfc-editor.org/rfc/rfc6120.html#section-8.3 explains
;; that there are stanza errors, which are recoverable and do not
;; terminate the stream.

;; Each stanza has a type which are the one explained at the
;; ERROR-TYPE parameter. checkdoc throws warnings stating that errors
;; messages should start with capital letters, thus the `downcase'
;; function is used as a workaround.
(defun jabber-signal-error (error-type condition &optional text app-specific)
  "Signal an error to be sent by Jabber.
ERROR-TYPE is one of \"Cancel\", \"Continue\", \"Mmodify\", \"Auth\"
and \"Wait\" (lowercase versions make `checkdoc' to throw errors).
CONDITION is a symbol denoting a defined XMPP condition.
TEXT is a string to be sent in the error message, or nil for no text.
APP-SPECIFIC is a list of extra XML tags.

See section 9.3 of XMPP Core (RFC 3920).
See section 8.3 of XMPP Core (RFC 6120)."
  (signal 'jabber-error
	  (list (downcase error-type) condition text app-specific)))

(defun jabber-unhex (string)
  "Convert a hex-encoded UTF-8 string to Emacs representation.
For example, \"ji%C5%99i@%C4%8Dechy.example/v%20Praze\" becomes
\"jiři@čechy.example/v Praze\"."
  (decode-coding-string (url-unhex-string string) 'utf-8))

(defun jabber-handle-uri (uri &rest _ignored-args)
  "Handle XMPP links according to draft-saintandre-xmpp-iri-04.
See Info node `(jabber)XMPP URIs'.
URI is a string with the \"xmpp://\" link to handle.
IGNORED-ARGS are ignored arguments the handler may pass. "
  (interactive "sEnter XMPP URI: ")

  (when (string-match "//" uri)
    (error "URIs with authority part are not supported"))

  ;; This regexp handles three cases:
  ;; xmpp:romeo@montague.net
  ;; xmpp:romeo@montague.net?roster
  ;; xmpp:romeo@montague.net?roster;name=Romeo%20Montague;group=Lovers
  (unless (string-match "^xmpp:\\([^?]+\\)\\(\\?\\([a-z]+\\)\\(;\\(.*\\)\\)?\\)?" uri)
    (error "Invalid XMPP URI '%s'" uri))

  ;; We start by raising the Emacs frame.
  (raise-frame)

  (let ((jid (jabber-unhex (match-string 1 uri)))
	(method (match-string 3 uri))
	(args (let ((text (match-string 5 uri)))
		;; If there are arguments...
		(when text
		  ;; ...split the pairs by ';'...
		  (let ((pairs (split-string text ";")))
		    (mapcar (lambda (pair)
			      ;; ...and split keys from values by '='.
			      (pcase-let ((`(,key ,value)
				           (split-string pair "=")))
				;; Values can be hex-coded.
				(cons key (jabber-unhex value))))
			    pairs))))))
    ;; The full list of methods is at
    ;; <URL:http://www.jabber.org/registrar/querytypes.html>.
    (cond
     ;; Join an MUC.
     ((string= method "join")
      (let ((account (jabber-read-account)))
	(jabber-muc-join
	 account jid (jabber-muc-read-my-nickname account jid) t)))
     ;; Register with a service.
     ((string= method "register")
      (jabber-get-register (jabber-read-account) jid))
     ;; Run an ad-hoc command
     ((string= method "command")
      ;; XXX: does the 'action' attribute make sense?
      (jabber-ahc-execute-command
       (jabber-read-account) jid (cdr (assoc "node" args))))
     ;; Everything else: open a chat buffer.
     (t
      (jabber-chat-with (jabber-read-account) jid)))))

(defun url-xmpp (url)
  "Handle XMPP URLs from internal Emacs functions."
  ;; XXX: This parsing roundtrip is redundant, and the parser of the
  ;; url package might lose information.
  (jabber-handle-uri (url-recreate-url url)))

(defun string>-numerical (s1 s2)
  "Return t if first arg string is more than second in numerical order."
  (cond ((string= s1 s2) nil)
	((> (length s1) (length s2)) t)
	((< (length s1) (length s2)) nil)
	((< (string-to-number (substring s1 0 1)) (string-to-number (substring s2 0 1))) nil)
	((> (string-to-number (substring s1 0 1)) (string-to-number (substring s2 0 1))) t)
	(t (string>-numerical (substring s1 1) (substring s2 1)))))

(defun jabber-append-string-to-file (string file &optional func &rest args)
  "Append STRING (may be nil) to FILE.  Create FILE if needed.
If FUNC is non-nil, then call FUNC with ARGS at beginning of
temporaly buffer _before_ inserting STRING."
  (when (or (stringp string) (functionp func))
    (with-temp-buffer
      (when (functionp func) (apply func args))
      (when (stringp string) (insert string))
      (write-region (point-min) (point-max) file t (list t)))))

(defun jabber-tree-map (fn tree)
  "Apply FN to all nodes in the TREE starting with root.
FN is applied to the node and not to the data itself."
  (let ((result (cons nil nil)))
    (cl-do ((tail tree (cdr tail))
	    (prev result end)
	    (end result (let* ((x (car tail))
			       (val (if (atom x)
				        (funcall fn x)
                                      (jabber-tree-map fn x))))
		          (setf (car end) val (cdr end) (cons nil
                                                              nil)))))
	((atom tail)
	 (progn
	   (setf (cdr prev) (if tail (funcall fn tail) nil))
	   result)))))

(provide 'jabber-util)

;;; jabber-util.el ends here