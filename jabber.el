;;; jabber.el --- a minimal jabber client  -*- lexical-binding: t; -*-

;; Author: Magnus Henoch <mange@freemail.hu>
;; Maintainer: wgreenhouse <wgreenhouse@tilde.club>
;; Keywords: comm
;; Homepage: https://tildegit.org/wgreenhouse/emacs-jabber
;; Package-Requires: ((hexrgb "0") (emacs "27.1"))
;; Version: 0.8.92

;; Copyright (C) 2003-2010, 2013 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002-2004 - Tom Berger - object@intelectronica.net
;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni
;; Copyright (C) 2008-2010, 2012-2013 - Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2006-2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2004-2005 - Carl Henrik Lunde - chlunde+jabber+@ping.uio.no
;; Copyright (C) 2009-2010 - Demyan Rogozhin <demyan.rogozhin@gmail.com>
;; Copyright (C) 2004 - Mathias Dahl
;; Copyright (C) 2007 - Serguei Jidkov - jsv@e-mail.ru

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

;;; Commentary:
;;

;;; History:
;;

;;; Code:

;; [[file:jabber.org::#dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'goto-addr)
;; Dependencies:1 ends here

;; [[file:jabber.org::#lexical-binding-test][lexical binding test:1]]
(defmacro jabber-lexical-p ()
  "Return non-nil in buffers with lexical binding."
  '(let* ((ret t)
          (code (lambda ()
                  ret)))
     (let ((_ret nil))
       (funcall code))))

(unless (jabber-lexical-p)
  (message "jabber.org: Lexical binding is off, trying to turn it on.")
  (setq lexical-binding t))

(eval-when-compile
  (unless (jabber-lexical-p)
    (message "jabber.org: Lexical binding is off, trying to turn it on.")
    (setq lexical-binding t)))

(unless (jabber-lexical-p)
  (message "jabber.org: It seems that lexical binding is still off...
Consider adding the file-local variable prop-line to the tangled jabber.el file
or try to byte-compile the code."))
;; lexical binding test:1 ends here

;; [[file:jabber.org::#custom-variables][custom variables:1]]
(defvar jabber-enable-legacy-features-p nil)
;; custom variables:1 ends here

;; [[file:jabber.org::#xml-functions][XML functions:1]]
(require 'xml)
;; XML functions:1 ends here

;; [[file:jabber.org::#escape-xml][jabber-escape-xml:1]]
(defun jabber-escape-xml (string)
  "Escape STRING for XML."
  (if (stringp string)
      (let ((newstr (concat string)))
	;; Form feeds might appear in code you copy, etc.  Nevertheless,
	;; it's invalid XML.
	(setq newstr (jabber-replace-in-string newstr "\f" "\n"))
	;; Other control characters are also illegal, except for
	;; tab, CR, and LF.
	(setq newstr (jabber-replace-in-string newstr "[\000-\010\013\014\016-\037]" " "))
	(setq newstr (jabber-replace-in-string newstr "&" "&amp;"))
	(setq newstr (jabber-replace-in-string newstr "<" "&lt;"))
	(setq newstr (jabber-replace-in-string newstr ">" "&gt;"))
	(setq newstr (jabber-replace-in-string newstr "'" "&apos;"))
	(setq newstr (jabber-replace-in-string newstr "\"" "&quot;"))
	newstr)
    string))
;; jabber-escape-xml:1 ends here

;; [[file:jabber.org::#unescape-xml][jabber-unescape-xml:1]]
(defun jabber-unescape-xml (string)
  "Unescape STRING for XML."
  ;; Eventually this can be done with `xml-substitute-special', but the
  ;; version in xml.el of GNU Emacs 21.3 is buggy.
  (if (stringp string)
      (let ((newstr string))
	(setq newstr (jabber-replace-in-string newstr "&quot;" "\""))
	(setq newstr (jabber-replace-in-string newstr "&apos;" "'"))
	(setq newstr (jabber-replace-in-string newstr "&gt;" ">"))
	(setq newstr (jabber-replace-in-string newstr "&lt;" "<"))
	(setq newstr (jabber-replace-in-string newstr "&amp;" "&"))
	newstr)
    string))
;; jabber-unescape-xml:1 ends here

;; [[file:jabber.org::#sexp2xml][jabber-sexp2xml:1]]
(defun jabber-sexp2xml (sexp)
  "Return SEXP as well-formatted XML.
SEXP should be in the form:
 (tagname ((attribute-name . attribute-value)...) children...)"
  (cond
   ((stringp sexp)
    (jabber-escape-xml sexp))
   ((listp (car sexp))
    (let ((xml ""))
      (dolist (tag sexp)
	(setq xml (concat xml (jabber-sexp2xml tag))))
      xml))
   ;; work around bug in old versions of xml.el, where ("") can appear
   ;; as children of a node
   ((and (consp sexp)
	 (stringp (car sexp))
	 (zerop (length (car sexp))))
    "")
   (t
    (let ((xml ""))
      (setq xml (concat "<"
			(symbol-name (car sexp))))
      (dolist (attr (cadr sexp))
	(if (consp attr)
	    (setq xml (concat xml
			      (format " %s='%s'"
				      (symbol-name (car attr))
				      (jabber-escape-xml (cdr attr)))))))
      (if (cddr sexp)
	  (progn
	    (setq xml (concat xml ">"))
	    (dolist (child (cddr sexp))
	      (setq xml (concat xml
				(jabber-sexp2xml child))))
	    (setq xml (concat xml
			      "</"
			      (symbol-name (car sexp))
			      ">")))
	(setq xml (concat xml
			  "/>")))
      xml))))
;; jabber-sexp2xml:1 ends here

;; [[file:jabber.org::#xml-skip-tag-forward][jabber-xml-skip-tag-forward:1]]
(defun jabber-xml-skip-tag-forward (&optional dont-recurse-into-stream)
  "Skip to end of tag or matching closing tag if present.
Return t iff after a closing tag, otherwise throws an 'unfinished
tag with value nil.
If DONT-RECURSE-INTO-STREAM is non-nil, stop after an opening
<stream:stream> tag.

The version of `sgml-skip-tag-forward' in Emacs 21 isn't good
enough for us."
  (skip-chars-forward "^<")
  (cond
   ((looking-at "<!\\[CDATA\\[")
    (if (search-forward "]]>" nil t)
	(goto-char (match-end 0))
      (throw 'unfinished nil)))
   ((looking-at "<\\([^[:space:]/>]+\\)\\([[:space:]]+[^=>]+=[[:space:]]*'[^']*'\\|[[:space:]]+[^=>]+=[[:space:]]*\"[^\"]*\"\\)*")
    (let ((node-name (match-string 1)))
      (goto-char (match-end 0))
      (skip-syntax-forward "\s-") ; Skip over trailing white space.
      (cond
       ((looking-at "/>")
	(goto-char (match-end 0))
	t)
       ((looking-at ">")
	(goto-char (match-end 0))
	(unless (and dont-recurse-into-stream (equal node-name "stream:stream"))
	  (cl-loop
	   do (skip-chars-forward "^<")
	   until (looking-at (regexp-quote (concat "</" node-name ">")))
	   do (jabber-xml-skip-tag-forward))
	  (goto-char (match-end 0)))
	t)
       (t
	(throw 'unfinished nil)))))
   (t
    (throw 'unfinished nil))))
;; jabber-xml-skip-tag-forward:1 ends here

;; [[file:jabber.org::#xml-parse-next-stanza][jabber-xml-parse-next-stanza:1]]
(defun jabber-xml-parse-next-stanza ()
  "Parse the first XML stanza in the current buffer.
Parse and return the first complete XML element in the buffer,
leaving point at the end of it.  If there is no complete XML
element, return nil."
  (and (catch 'unfinished
	 (goto-char (point-min))
	 (jabber-xml-skip-tag-forward)
	 (> (point) (point-min)))
       (xml-parse-region (point-min) (point))))
;; jabber-xml-parse-next-stanza:1 ends here

;; [[file:jabber.org::#xml-node-name][jabber-xml-node-name:1]]
(defsubst jabber-xml-node-name (node)
  "Return the tag associated with NODE.
The tag is a lower-case symbol."
  (if (listp node) (car node)))
;; jabber-xml-node-name:1 ends here

;; [[file:jabber.org::#xml-node-attributes][jabber-xml-node-attributes:1]]
(defsubst jabber-xml-node-attributes (node)
  "Return the list of attributes of NODE.
The list can be nil."
  (if (listp node) (nth 1 node)))
;; jabber-xml-node-attributes:1 ends here

;; [[file:jabber.org::#xml-node-children][jabber-xml-node-children:1]]
(defsubst jabber-xml-node-children (node)
  "Return the list of children of NODE.
This is a list of nodes, and it can be nil."
  (let ((children (cddr node)))
    ;; Work around a bug in early versions of xml.el
    (if (equal children '(("")))
	nil
      children)))
;; jabber-xml-node-children:1 ends here

;; [[file:jabber.org::#xml-get-children][jabber-xml-get-children:1]]
(defun jabber-xml-get-children (node child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should be a lower case symbol."
  (let ((match ()))
    (dolist (child (jabber-xml-node-children node))
      (if child
	  (if (equal (jabber-xml-node-name child) child-name)
	      (push child match))))
    (nreverse match)))
;; jabber-xml-get-children:1 ends here

;; [[file:jabber.org::#xml-get-attribute][jabber-xml-get-attribute:1]]
(defsubst jabber-xml-get-attribute (node attribute)
  "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found."
  (when (consp node)
    (xml-get-attribute-or-nil node attribute)))
;; jabber-xml-get-attribute:1 ends here

;; [[file:jabber.org::#xml-get-xmlns][jabber-xml-get-xmlns:1]]
(defsubst jabber-xml-get-xmlns (node)
  "Get \"xmlns\" attribute of NODE, or nil if not present."
  (jabber-xml-get-attribute node 'xmlns))
;; jabber-xml-get-xmlns:1 ends here

;; [[file:jabber.org::#xml-path][jabber-xml-path:1]]
(defun jabber-xml-path (xml-data path)
  "Find sub-node of XML-DATA according to PATH.
PATH is a vaguely XPath-inspired list.  Each element can be:
a symbol     go to first child node with this node name
cons cell    car is string containing namespace URI,
             cdr is string containing node name.  Find
             first matching child node.
any string   character data of this node."
  (let ((node xml-data))
    (while (and path node)
      (let ((step (car path)))
        (cond
         ((symbolp step)
          (setq node (car (jabber-xml-get-children node step))))
         ((consp step)
          ;; This will be easier with namespace-aware use
          ;; of xml.el.  It will also be more correct.
          ;; Now, it only matches explicit namespace declarations.
          (setq node
                (cl-block dolist-loop
                (dolist (x (jabber-xml-get-children node (intern (cdr step))))
                  (when (string= (jabber-xml-get-attribute x 'xmlns)
                                 (car step))
                    (cl-return-from dolist-loop  x))))))
         ((stringp step)
          (setq node (car (jabber-xml-node-children node)))
          (unless (stringp node)
            (setq node nil)))
         (t
          (error "Unknown path step: %s" step))))
      (setq path (cdr path)))
    node))
;; jabber-xml-path:1 ends here

;; [[file:jabber.org::#xml-let-attributes][jabber-xml-let-attributes:1]]
(defmacro jabber-xml-let-attributes (attributes xml-data &rest body)
  "Evaluate BODY with ATTRIBUTES bound to their values in XML-DATA.
ATTRIBUTES must be a list of symbols, as present in XML-DATA."
  `(let ,(mapcar #'(lambda (attr)
		     (list attr `(jabber-xml-get-attribute ,xml-data ',attr)))
		 attributes)
     ,@body))
(put 'jabber-xml-let-attributes 'lisp-indent-function 2)
;; jabber-xml-let-attributes:1 ends here

;; [[file:jabber.org::#xml-resolve-namespace-prefixes][jabber-xml-resolve-namespace-prefixes:1]]
(defun jabber-xml-resolve-namespace-prefixes (xml-data &optional default-ns prefixes)
  (let ((node-name (jabber-xml-node-name xml-data))
	(attrs (jabber-xml-node-attributes xml-data)))
    (setq prefixes (jabber-xml-merge-namespace-declarations attrs prefixes))

    ;; If there is an xmlns attribute, it is the new default
    ;; namespace.
    (let ((xmlns (jabber-xml-get-xmlns xml-data)))
      (when xmlns
	(setq default-ns xmlns)))
    ;; Now, if the node name has a prefix, replace it and add an
    ;; "xmlns" attribute.  Slightly ugly, but avoids the need to
    ;; change all the rest of jabber.el at once.
    (let ((node-name-string (symbol-name node-name)))
      (when (string-match "\\(.*\\):\\(.*\\)" node-name-string)
	(let* ((prefix (match-string 1 node-name-string))
	       (unprefixed (match-string 2 node-name-string))
	       (ns (assoc prefix prefixes)))
	  (if (null ns)
	      ;; This is not supposed to happen...
	      (message "jabber-xml-resolve-namespace-prefixes: Unknown prefix in %s" node-name-string)
	    (setf (car xml-data) (intern unprefixed))
	    (setf (cadr xml-data) (cons (cons 'xmlns (cdr ns)) (delq 'xmlns attrs)))))))
    ;; And iterate through all child elements.
    (mapc (lambda (x)
	    (when (listp x)
	      (jabber-xml-resolve-namespace-prefixes x default-ns prefixes)))
	  (jabber-xml-node-children xml-data))
    xml-data))
;; jabber-xml-resolve-namespace-prefixes:1 ends here

;; [[file:jabber.org::#xml-merge-namespace-declarations][jabber-xml-merge-namespace-declarations:1]]
(defun jabber-xml-merge-namespace-declarations (attrs prefixes)
  ;; First find any xmlns:foo attributes..
  (dolist (attr attrs)
    (let ((attr-name (symbol-name (car attr))))
      (when (string-match "xmlns:" attr-name)
	(let ((prefix (substring attr-name (match-end 0)))
	      (ns-uri (cdr attr)))
	  ;; A slightly complicated dance to never change the
	  ;; original value of prefixes (since the caller depends on
	  ;; it), but also to avoid excessive copying (which remove
	  ;; always does).  Might need to profile and tweak this for
	  ;; performance.
	  (setq prefixes
		(cons (cons prefix ns-uri)
			(if (assoc prefix prefixes)
			    (remove (assoc prefix prefixes) prefixes)
			  prefixes)))))))
  prefixes)
;; jabber-xml-merge-namespace-declarations:1 ends here

;; [[file:jabber.org::#various-utility-functions][various utility functions:1]]
(require 'password-cache)
(condition-case nil
    (require 'auth-source)
  (error nil))
;; various utility functions:1 ends here

;; [[file:jabber.org::#jid-history][jabber-jid-history:1]]
(defvar jabber-jid-history nil
  "History of entered JIDs.")
;; jabber-jid-history:1 ends here

;; [[file:jabber.org::#replace-string][jabber-replace-in-string:1]]
(defsubst jabber-replace-in-string (string regexp newtext)
  "Return STRING with all matches for REGEXP replaced with NEWTEXT.
NEWTEXT is inserted literally, without changing its case or treating \"\\\"
specially."
  (replace-regexp-in-string regexp newtext string t t))
;; jabber-replace-in-string:1 ends here

;; [[file:jabber.org::#propertize][jabber-propertize:1]]
(defalias 'jabber-propertize 'propertize)
;; jabber-propertize:1 ends here

;; [[file:jabber.org::#bound-true-p][bound-and-true-p:1]]
(unless (fboundp 'bound-and-true-p)
  (defmacro bound-and-true-p (var)
    "Return the value of symbol VAR if it is bound, else nil."
    `(and (boundp (quote ,var)) ,var)))
;; bound-and-true-p:1 ends here

;; [[file:jabber.org::#read-input-method][jabber-read-with-input-method:1]]
(defsubst jabber-read-with-input-method (prompt &optional initial-contents history default-value)
  "Like `read-string', but always inheriting the current input method."
  (read-string prompt initial-contents history default-value t))
;; jabber-read-with-input-method:1 ends here

;; [[file:jabber.org::#delete-extract-region][delete-and-extract-region:1]]
(unless (fboundp 'delete-and-extract-region)
  (defsubst delete-and-extract-region (start end)
    (prog1
	(buffer-substring start end)
      (delete-region start end))))
;; delete-and-extract-region:1 ends here

;; [[file:jabber.org::#access-file][access-file:1]]
(unless (fboundp 'access-file)
  (defsubst access-file (filename error-message)
    (unless (file-readable-p filename)
      (error error-message))))
;; access-file:1 ends here

;; [[file:jabber.org::#float-time][jabber-float-time:1]]
(defalias 'jabber-float-time 'float-time)
;; jabber-float-time:1 ends here

;; [[file:jabber.org::#cancel-timer][jabber-cancel-timer:1]]
(defalias 'jabber-cancel-timer 'cancel-timer)
;; jabber-cancel-timer:1 ends here

;; [[file:jabber.org::#concat-rosters][jabber-concat-rosters:1]]
(defvar jabber-connections)
(defun jabber-concat-rosters ()
  "Concatenate the rosters of all connected accounts."
  (apply #'append
	 (mapcar
	  (lambda (jc)
	    (plist-get (fsm-get-state-data jc) :roster))
	  jabber-connections)))
;; jabber-concat-rosters:1 ends here

;; [[file:jabber.org::#concat-rosters-full][jabber-concat-rosters-full:1]]
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
;; jabber-concat-rosters-full:1 ends here

;; [[file:jabber.org::#connection-jid][jabber-connection-jid:1]]
(defun jabber-connection-jid (jc)
  "Return the full JID of connection JC."
  (let ((sd (fsm-get-state-data jc)))
    (concat (plist-get sd :username) "@"
	    (plist-get sd :server) "/"
	    (plist-get sd :resource))))
;; jabber-connection-jid:1 ends here

;; [[file:jabber.org::#connection-bare-jid][jabber-connection-bare-jid:1]]
(defun jabber-connection-bare-jid (jc)
  "Return the bare JID of connection JC."
  (let ((sd (fsm-get-state-data jc)))
    (concat (plist-get sd :username) "@"
	    (plist-get sd :server))))
;; jabber-connection-bare-jid:1 ends here

;; [[file:jabber.org::#connection-original-jid][jabber-connection-original-jid:1]]
(defun jabber-connection-original-jid (jc)
  "Return the original JID of connection JC.
The \"original JID\" is the JID we authenticated with.  The
server might subsequently assign us a different JID at resource
binding."
  (plist-get (fsm-get-state-data jc) :original-jid))
;; jabber-connection-original-jid:1 ends here

;; [[file:jabber.org::#find-connection][jabber-find-connection:1]]
(defun jabber-find-connection (bare-jid)
  "Find the connection to the account named by BARE-JID.
Return nil if none found."
  (dolist (jc jabber-connections)
    (when (string= bare-jid (jabber-connection-bare-jid jc))
      (cl-return jc))))
;; jabber-find-connection:1 ends here

;; [[file:jabber.org::#find-active-connection][jabber-find-active-connection:1]]
(defun jabber-find-active-connection (dead-jc)
  "Find an active connection for dead connection DEAD-JC.
Return nil if none found."
  (let ((jid (jabber-connection-bare-jid dead-jc)))
    (jabber-find-connection jid)))
;; jabber-find-active-connection:1 ends here

;; [[file:jabber.org::#jid-username][jabber-jid-username:1]]
(defun jabber-jid-username (jid)
  "Return the username portion of JID, or nil if none found.
JID must be a string."
  (when (string-match "\\(.*\\)@.*\\(/.*\\)?" jid)
    (match-string 1 jid)))
;; jabber-jid-username:1 ends here

;; [[file:jabber.org::#jid-user][jabber-jid-user:1]]
(defun jabber-jid-user (jid)
  "Return the user portion (username@server) of JID.
JID must be a string."
  ;;transports don't have @, so don't require it
  ;;(string-match ".*@[^/]*" jid)
  (string-match "[^/]*" jid)
  (match-string 0 jid))
;; jabber-jid-user:1 ends here

;; [[file:jabber.org::#jid-server][jabber-jid-server:1]]
(defun jabber-jid-server (jid)
  "Return the server portion of JID."
  (string-match "^\\(.*@\\)?\\([^@/]+\\)\\(/.*\\)?$" jid)
  (match-string 2 jid))
;; jabber-jid-server:1 ends here

;; [[file:jabber.org::#jid-rostername][jabber-jid-rostername:1]]
(defun jabber-jid-rostername (user)
  "Return the name of USER if present in roster, or nil."
  (let ((user (jabber-jid-symbol user)))
    (if (> (length (get user 'name)) 0)
	(get user 'name))))
;; jabber-jid-rostername:1 ends here

;; [[file:jabber.org::#jid-displayname][jabber-jid-displayname:1]]
(defun jabber-jid-displayname (string)
  "Return the name of the user from STRING as in roster, else username@server."
  (or (jabber-jid-rostername string)
      (jabber-jid-user (if (symbolp string)
			   (symbol-name string)
			 string))))
;; jabber-jid-displayname:1 ends here

;; [[file:jabber.org::#jid-bookmarkname][jabber-jid-bookmarkname:1]]
(defvar jabber-bookmarks)
(defun jabber-jid-bookmarkname (string)
  "Return from STRING the conference name from boomarks or displayname.
Use the name according to roster or else the JID if none set."
  (or (cl-loop for conference in (cl-first (cl-loop for value being the hash-values of jabber-bookmarks
                                           collect value))
            do (let ((ls (cadr conference)))
                 (if (string= (cdr (assoc 'jid ls)) string)
                     (cl-return (cdr (assoc 'name ls))))))
      (jabber-jid-displayname string)))
;; jabber-jid-bookmarkname:1 ends here

;; [[file:jabber.org::#jid-resource][jabber-jid-resource:1]]
(defun jabber-jid-resource (jid)
  "Return the resource portion of a JID, or nil if there is none.
JID must be a string."
  (when (string-match "^\\(\\([^/]*@\\)?[^/]*\\)/\\(.*\\)" jid)
    (match-string 3 jid)))
;; jabber-jid-resource:1 ends here

;; [[file:jabber.org::#jid-symbol][jabber-jid-symbol:1]]
(defvar jabber-jid-obarray)
(defun jabber-jid-symbol (jid)
  "Return the symbol for JID, which must be a symbol or a string."
  ;; If it's already a symbol, just return it.
  (if (symbolp jid)
      jid
    ;; XXX: "downcase" is a poor man's nodeprep.  See XMPP CORE.
    (intern (downcase (jabber-jid-user jid)) jabber-jid-obarray)))
;; jabber-jid-symbol:1 ends here

;; [[file:jabber.org::#my-jid-p][jabber-my-jid-p:1]]
(defvar jabber-account-list)
(defun jabber-my-jid-p (jc jid)
  "Return non-nil if the specified JID is in the `jabber-account-list'.
Comment: (modulo resource).
Also return non-nil if JID matches JC, modulo resource."
  (or
   (equal (jabber-jid-user jid)
	  (jabber-connection-bare-jid jc))
   (member (jabber-jid-user jid) (mapcar (lambda (x) (jabber-jid-user (car x))) jabber-account-list))))
;; jabber-my-jid-p:1 ends here

;; [[file:jabber.org::#read-jid-completing][jabber-read-jid-completing:1]]
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
      (cl-case resource
	(full
	 ;; If JID is bare, add the highest-priority resource.
	 (if (jabber-jid-resource chosen)
	     chosen
	   (let ((highest-resource (get (jabber-jid-symbol chosen) 'resource)))
	     (if highest-resource
		 (concat chosen "/" highest-resource)
	       chosen))))
	(bare-or-muc
	 ;; If JID is full and non-MUC, remove resource.
	 (if (null (jabber-jid-resource chosen))
	     chosen
	   (let ((bare (jabber-jid-user chosen)))
	     (if (assoc bare *jabber-active-groupchats*)
		 chosen
	       bare))))
	(t
	 chosen)))))
;; jabber-read-jid-completing:1 ends here

;; [[file:jabber.org::#read-node][jabber-read-node:1]]
(defun jabber-read-node (prompt)
  "Read node name, taking default from disco item at point."
  (let ((node-at-point (get-text-property (point) 'jabber-node)))
    (read-string (concat prompt
			 (if node-at-point
			     (format "(default %s) " node-at-point)))
		 node-at-point)))
;; jabber-read-node:1 ends here

;; [[file:jabber.org::#password-key][jabber-password-key:1]]
(defun jabber-password-key (bare-jid)
  "Construct key for `password' library from BARE-JID."
  (concat "xmpp:" bare-jid))
;; jabber-password-key:1 ends here

;; [[file:jabber.org::#read-password][jabber-read-password:1]]
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
;; jabber-read-password:1 ends here

;; [[file:jabber.org::#cache-password][jabber-cache-password:1]]
(defun jabber-cache-password (bare-jid password)
  "Cache PASSWORD for BARE-JID."
  (password-cache-add (jabber-password-key bare-jid) password))
;; jabber-cache-password:1 ends here

;; [[file:jabber.org::#uncache-password][jabber-uncache-password:1]]
(defun jabber-uncache-password (bare-jid)
  "Uncache cached password for BARE-JID.
Useful if the password proved to be wrong."
  (interactive (list (jabber-jid-user
		      (completing-read "Forget password of account: " jabber-account-list nil nil nil 'jabber-account-history))))
  (password-cache-remove (jabber-password-key bare-jid)))
;; jabber-uncache-password:1 ends here

;; [[file:jabber.org::#read-account][jabber-read-account:1]]
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
                       (jabber-connection-bare-jid at-point))) completions))
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
;; jabber-read-account:1 ends here

;; [[file:jabber.org::#iq-query][jabber-iq-query:1]]
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
;; jabber-iq-query:1 ends here

;; [[file:jabber.org::#iq-error][jabber-iq-error:1]]
(defun jabber-iq-error (xml-data)
  "Return the <error/> part of an IQ stanza, if any.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (car (jabber-xml-get-children xml-data 'error)))
;; jabber-iq-error:1 ends here

;; [[file:jabber.org::#iq-xmlns][jabber-iq-xmlns:1]]
(defun jabber-iq-xmlns (xml-data)
  "Return the namespace of an IQ stanza, i.e. the namespace of its query part.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (jabber-xml-get-attribute (jabber-iq-query xml-data) 'xmlns))
;; jabber-iq-xmlns:1 ends here

;; [[file:jabber.org::#message-timestamp][jabber-message-timestamp:1]]
(defun jabber-message-timestamp (xml-data)
  "Given a <message/> element, return its timestamp, or nil if none.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (jabber-x-delay
   (or
    (jabber-xml-path xml-data '(("urn:xmpp:delay" . "delay")))
    (jabber-xml-path xml-data '(("jabber:x:delay" . "x"))))))
;; jabber-message-timestamp:1 ends here

;; [[file:jabber.org::#x-delay][jabber-x-delay:1]]
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
;; jabber-x-delay:1 ends here

;; [[file:jabber.org::#parse-legacy-time][jabber-parse-legacy-time:1]]
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
;; jabber-parse-legacy-time:1 ends here

;; [[file:jabber.org::#encode-legacy-time][jabber-encode-legacy-time:1]]
(defun jabber-encode-legacy-time (timestamp)
  "Parse TIMESTAMP as internal time value and encode as ccyymmddThh:mm:ss (UTC)."
  (if (featurep 'xemacs)
      ;; XEmacs doesn't have `universal' argument to format-time-string,
      ;; so we have to do it ourselves.
      (format-time-string "%Y%m%dT%H:%M:%S"
			  (time-subtract timestamp
					 (list 0 (car (current-time-zone)))))
    (format-time-string "%Y%m%dT%H:%M:%S" timestamp t)))
;; jabber-encode-legacy-time:1 ends here

;; [[file:jabber.org::#encode-time][jabber-encode-time:1]]
(defun jabber-encode-time (time)
  "Convert TIME to a string by XEP-0082.
TIME is in a format accepted by `format-time-string'."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
;; jabber-encode-time:1 ends here

;; [[file:jabber.org::#encode-timezone][jabber-encode-timezone:1]]
(defun jabber-encode-timezone ()
  (let ((time-zone-offset (nth 0 (current-time-zone))))
    (if (null time-zone-offset)
        "Z"
      (let* ((positivep (>= time-zone-offset 0))
             (hours (/ (abs time-zone-offset) 3600))
             (minutes (/ (% (abs time-zone-offset) 3600) 60)))
        (format "%s%02d:%02d"(if positivep "+" "-") hours minutes)))))
;; jabber-encode-timezone:1 ends here

;; [[file:jabber.org::#parse-time][jabber-parse-time:1]]
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
;; jabber-parse-time:1 ends here

;; [[file:jabber.org::#report-success][jabber-report-success:1]]
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
;; jabber-report-success:1 ends here

;; [[file:jabber.org::#error-messages][jabber-error-messages:1]]
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
;; jabber-error-messages:1 ends here

;; [[file:jabber.org::#legacy-error-messages][jabber-legacy-error-messages:1]]
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
;; jabber-legacy-error-messages:1 ends here

;; [[file:jabber.org::#parse-error][jabber-parse-error:1]]
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
;; jabber-parse-error:1 ends here

;; [[file:jabber.org::#error-condition][jabber-error-condition:1]]
(defun jabber-error-condition (error-xml)
  "Parse the given <error/> tag and return the condition symbol."
  (catch 'condition
    (dolist (child (jabber-xml-node-children error-xml))
      (when (string=
		 (jabber-xml-get-attribute child 'xmlns)
		 "urn:ietf:params:xml:ns:xmpp-stanzas")
	(throw 'condition (jabber-xml-node-name child))))))
;; jabber-error-condition:1 ends here

;; [[file:jabber.org::#stream-error-messages][jabber-stream-error-messages:1]]
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
;; jabber-stream-error-messages:1 ends here

;; [[file:jabber.org::#stream-error-condition][jabber-stream-error-condition:1]]
(defun jabber-stream-error-condition (error-xml)
  "Return the condition of a <stream:error/> tag."
  ;; as we don't know the node name of the condition, we have to
  ;; search for it.
  (dolist (node (jabber-xml-node-children error-xml))
    (when (and (string= (jabber-xml-get-attribute node 'xmlns)
			"urn:ietf:params:xml:ns:xmpp-streams")
	       (assq (jabber-xml-node-name node)
		     jabber-stream-error-messages))
      (cl-return (jabber-xml-node-name node)))))
;; jabber-stream-error-condition:1 ends here

;; [[file:jabber.org::#parse-stream-error][jabber-parse-stream-error:1]]
(defun jabber-parse-stream-error (error-xml)
  "Parse the given error tag and return a string fit for human consumption.
ERROR-XML is a <stream:error/> tag parsed with `xml-parse-region'."
  (let ((text-node (car (jabber-xml-get-children error-xml 'text)))
	(condition (jabber-stream-error-condition error-xml)))
    (concat (if condition (cdr (assq condition jabber-stream-error-messages))
	      "Unknown stream error")
	    (if (and text-node (stringp (car (jabber-xml-node-children text-node))))
		(concat ": " (car (jabber-xml-node-children text-node)))))))
;; jabber-parse-stream-error:1 ends here

;; [[file:jabber.org::#parse-stream-error][jabber-parse-stream-error:2]]
(put 'jabber-error
     'error-conditions
     '(error jabber-error))
(put 'jabber-error
     'error-message
     "Jabber error")
;; jabber-parse-stream-error:2 ends here

;; [[file:jabber.org::#signal-error][jabber-signal-error:1]]
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
;; jabber-signal-error:1 ends here

;; [[file:jabber.org::#unhex][jabber-unhex:1]]
(defun jabber-unhex (string)
  "Convert a hex-encoded UTF-8 string to Emacs representation.
For example, \"ji%C5%99i@%C4%8Dechy.example/v%20Praze\" becomes
\"jiři@čechy.example/v Praze\"."
  (decode-coding-string (url-unhex-string string) 'utf-8))
;; jabber-unhex:1 ends here

;; [[file:jabber.org::#handle-uri][jabber-handle-uri:1]]
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
			      (cl-destructuring-bind (key value)
				  (split-string pair "=")
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
;; jabber-handle-uri:1 ends here

;; [[file:jabber.org::#url-xmpp][url-xmpp:1]]
(defun url-xmpp (url)
  "Handle XMPP URLs from internal Emacs functions."
  ;; XXX: This parsing roundtrip is redundant, and the parser of the
  ;; url package might lose information.
  (jabber-handle-uri (url-recreate-url url)))
;; url-xmpp:1 ends here

;; [[file:jabber.org::#string>-numerical][string>-numerical:1]]
(defun string>-numerical (s1 s2)
  "Return t if first arg string is more than second in numerical order."
  (cond ((string= s1 s2) nil)
	((> (length s1) (length s2)) t)
	((< (length s1) (length s2)) nil)
	((< (string-to-number (substring s1 0 1)) (string-to-number (substring s2 0 1))) nil)
	((> (string-to-number (substring s1 0 1)) (string-to-number (substring s2 0 1))) t)
	(t (string>-numerical (substring s1 1) (substring s2 1)))))
;; string>-numerical:1 ends here

;; [[file:jabber.org::#append-string-to-file][jabber-append-string-to-file:1]]
(defun jabber-append-string-to-file (string file &optional func &rest args)
  "Append STRING (may be nil) to FILE.  Create FILE if needed.
If FUNC is non-nil, then call FUNC with ARGS at beginning of
temporaly buffer _before_ inserting STRING."
  (when (or (stringp string) (functionp func))
    (with-temp-buffer
      (when (functionp func) (apply func args))
      (when (stringp string) (insert string))
      (write-region (point-min) (point-max) file t (list t)))))
;; jabber-append-string-to-file:1 ends here

;; [[file:jabber.org::#tree-map][jabber-tree-map:1]]
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
;; jabber-tree-map:1 ends here

;; [[file:jabber.org::#menu][jabber-menu:1]]
;;;###autoload
(defvar jabber-menu
  (let ((map (make-sparse-keymap "jabber-menu")))
    (define-key-after map
      [jabber-menu-connect]
      '("Connect" . jabber-connect-all))

    (define-key-after map
      [jabber-menu-disconnect]
      '(menu-item "Disconnect" jabber-disconnect
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-status]
      `(menu-item "Set Status" ,(make-sparse-keymap "set-status")
		  :enable (bound-and-true-p jabber-connections)))

    (define-key map
      [jabber-menu-status jabber-menu-status-chat]
      '(menu-item
	"Chatty"
	(lambda ()
	  (interactive)
	  (jabber-send-presence "chat"
				(jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
				*jabber-current-priority*))
	:button (:radio . (and (boundp '*jabber-current-show*)
			       (equal *jabber-current-show* "chat")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-dnd]
      '(menu-item
	"Do not Disturb"
	(lambda ()
	  (interactive)
	  (jabber-send-presence "dnd"
				(jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
				*jabber-current-priority*))
	:button (:radio . (and (boundp '*jabber-current-show*)
			       (equal *jabber-current-show* "dnd")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-xa]
      '(menu-item "Extended Away" jabber-send-xa-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "xa")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-away]
      '(menu-item "Away" jabber-send-away-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "away")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-online]
      '(menu-item "Online" jabber-send-default-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "")))))

    (define-key-after map
      [separator]
      '(menu-item "--"))

    (define-key-after map
      [jabber-menu-chat-with]
      '(menu-item "Chat with..." jabber-chat-with
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-nextmsg]
      '(menu-item "Next unread message" jabber-activity-switch-to
		  :enable (bound-and-true-p jabber-activity-jids)))

    (define-key-after map
      [jabber-menu-send-subscription-request]
      '(menu-item "Send subscription request" jabber-send-subscription-request
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-roster]
      '("Switch to roster" . jabber-switch-to-roster-buffer))

    (define-key-after map
      [separator2]
      '(menu-item "--"))

    (define-key-after map
      [jabber-menu-customize]
      '("Customize" . jabber-customize))

    (define-key-after map
      [jabber-menu-info]
      '("Help" . jabber-info))

    map))
;; jabber-menu:1 ends here

;; [[file:jabber.org::#display-menu][jabber-display-menu:1]]
;;;###autoload
(defcustom jabber-display-menu 'maybe
  "Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if jabber.el is installed under `package-user-dir', or
if any of `jabber-account-list' or `jabber-connections' is non-nil."
  :group 'jabber
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When installed by user, or when any accounts have been configured or connected" maybe)))
;; jabber-display-menu:1 ends here

;; [[file:jabber.org::#menu-1][jabber-menu:1]]
(defun jabber-menu (&optional remove)
  "Put \"Jabber\" menu on menubar.
With prefix argument, remove it."
  (interactive "P")
  (setq jabber-display-menu (if remove nil t))
  (force-mode-line-update))
(make-obsolete 'jabber-menu "set the variable `jabber-display-menu' instead." "27.2")
;; jabber-menu:1 ends here

;; [[file:jabber.org::#menu-1][jabber-menu:2]]
;;;###autoload
(define-key-after (lookup-key global-map [menu-bar])
  [jabber-menu]
  (list 'menu-item "Jabber" jabber-menu
	:visible
        '(or (eq jabber-display-menu t)
             (and (eq jabber-display-menu 'maybe)
                  (or (bound-and-true-p jabber-account-list)
                      (bound-and-true-p jabber-connections))))))
;; jabber-menu:2 ends here

;; [[file:jabber.org::#jid-chat-menu][jabber-jid-chat-menu:1]]
(defvar jabber-jid-chat-menu nil
  "Menu items for chat menu.")
;; jabber-jid-chat-menu:1 ends here

;; [[file:jabber.org::#jid-info-menu][jabber-jid-info-menu:1]]
(defvar jabber-jid-info-menu nil
  "Menu item for info menu.")
;; jabber-jid-info-menu:1 ends here

;; [[file:jabber.org::#jid-roster-menu][jabber-jid-roster-menu:1]]
(defvar jabber-jid-roster-menu nil
  "Menu items for roster menu.")
;; jabber-jid-roster-menu:1 ends here

;; [[file:jabber.org::#jid-muc-menu][jabber-jid-muc-menu:1]]
(defvar jabber-jid-muc-menu nil
  "Menu items for MUC menu.")
;; jabber-jid-muc-menu:1 ends here

;; [[file:jabber.org::#jid-service-menu][jabber-jid-service-menu:1]]
(defvar jabber-jid-service-menu nil
  "Menu items for service menu.")
;; jabber-jid-service-menu:1 ends here

;; [[file:jabber.org::#popup-menu][jabber-popup-menu:1]]
(defun jabber-popup-menu (which-menu)
  "Popup specified menu."
  (let* ((mouse-event (and (listp last-input-event) last-input-event))
	 (choice (widget-choose "Actions" which-menu mouse-event)))
    (if mouse-event
	(mouse-set-point mouse-event))
    (if choice
	(call-interactively choice))))
;; jabber-popup-menu:1 ends here

;; [[file:jabber.org::#popup-chat-menu][jabber-popup-chat-menu:1]]
(defun jabber-popup-chat-menu ()
  "Popup chat menu."
  (interactive)
  (jabber-popup-menu jabber-jid-chat-menu))
;; jabber-popup-chat-menu:1 ends here

;; [[file:jabber.org::#popup-info-menu][jabber-popup-info-menu:1]]
(defun jabber-popup-info-menu ()
  "Popup info menu."
  (interactive)
  (jabber-popup-menu jabber-jid-info-menu))
;; jabber-popup-info-menu:1 ends here

;; [[file:jabber.org::#popup-roster-menu][jabber-popup-roster-menu:1]]
(defun jabber-popup-roster-menu ()
  "Popup roster menu."
  (interactive)
  (jabber-popup-menu jabber-jid-roster-menu))
;; jabber-popup-roster-menu:1 ends here

;; [[file:jabber.org::#popup-muc-menu][jabber-popup-muc-menu:1]]
(defun jabber-popup-muc-menu ()
  "Popup MUC menu."
  (interactive)
  (jabber-popup-menu jabber-jid-muc-menu))
;; jabber-popup-muc-menu:1 ends here

;; [[file:jabber.org::#popup-service-menu][jabber-popup-service-menu:1]]
(defun jabber-popup-service-menu ()
  "Popup service menu."
  (interactive)
  (jabber-popup-menu jabber-jid-service-menu))
;; jabber-popup-service-menu:1 ends here

;; [[file:jabber.org::#popup-combined-menu][jabber-popup-combined-menu:1]]
(defun jabber-popup-combined-menu ()
  "Popup combined menu."
  (interactive)
  (jabber-popup-menu (append jabber-jid-chat-menu jabber-jid-info-menu jabber-jid-roster-menu jabber-jid-muc-menu)))
;; jabber-popup-combined-menu:1 ends here

;; [[file:jabber.org::#network-transport-functions][Network transport functions:1]]
;; Emacs 24 can be linked with GnuTLS
(ignore-errors (require 'gnutls))

;; Try two different TLS/SSL libraries, but don't fail if none available.
(or (ignore-errors (require 'tls))
    (ignore-errors (require 'ssl)))

(ignore-errors (require 'starttls))

(eval-and-compile
  (or (ignore-errors (require 'srv))
      (ignore-errors
        (let ((load-path (cons (expand-file-name
                                "jabber-fallback-lib"
                                (file-name-directory (locate-library "jabber")))
                               load-path)))
          (require 'srv)))
      (error
       "The srv library was not found in `load-path' or jabber-fallback-lib/ directory")))
;; Network transport functions:1 ends here

;; [[file:jabber.org::#conn][jabber-conn:1]]
(defgroup jabber-conn nil "Jabber Connection Settings."
  :group 'jabber)
;; jabber-conn:1 ends here

;; [[file:jabber.org::#have-starttls][jabber-have-starttls:1]]
(defun jabber-have-starttls ()
  "Return non-nil if we can use STARTTLS."
  (or (and (fboundp 'gnutls-available-p)
	   (gnutls-available-p))
      (and (featurep 'starttls)
	   (or (and (bound-and-true-p starttls-gnutls-program)
		    (executable-find starttls-gnutls-program))
	       (and (bound-and-true-p starttls-program)
		    (executable-find starttls-program))))))
;; jabber-have-starttls:1 ends here

;; [[file:jabber.org::#default-connection-type][jabber-default-connection-type:1]]
(defconst jabber-default-connection-type
  (cond
   ;; Use STARTTLS if we can...
   ((jabber-have-starttls)
    'starttls)
   ;; ...else default to unencrypted connection.
   (t
    'network))
  "Default connection type.
See `jabber-connect-methods'.")
;; jabber-default-connection-type:1 ends here

;; [[file:jabber.org::#connection-ssl-program][jabber-connection-ssl-program:1]]
(defcustom jabber-connection-ssl-program nil
  "Program used for SSL/TLS connections.
nil means prefer gnutls but fall back to openssl.
'gnutls' means use gnutls (through `open-tls-stream').
'openssl means use openssl (through `open-ssl-stream')."
  :type '(choice (const :tag "Prefer gnutls, fall back to openssl" nil)
		 (const :tag "Use gnutls" gnutls)
		 (const :tag "Use openssl" openssl))
  :group 'jabber-conn)
;; jabber-connection-ssl-program:1 ends here

;; [[file:jabber.org::#invalid-certificate-servers][jabber-invalid-certificate-servers:1]]
(defcustom jabber-invalid-certificate-servers ()
  "Jabber servers for which we accept invalid TLS certificates.
This is a list of server names, each matching the hostname part
of your JID.

This option has effect only when using native GnuTLS in Emacs 24
or later."
  :type '(repeat string)
  :group 'jabber-conn)
;; jabber-invalid-certificate-servers:1 ends here

;; [[file:jabber.org::#connect-methods][jabber-connect-methods:1]]
(defvar jabber-connect-methods
  `((network jabber-network-connect jabber-network-send)
    (starttls
     ,(if (and (fboundp 'gnutls-available-p)
	       (gnutls-available-p))
	  ;; With "native" TLS, we can use a normal connection.
	  'jabber-network-connect
	'jabber-starttls-connect)
     jabber-network-send)
    (ssl jabber-ssl-connect jabber-ssl-send)
    (virtual jabber-virtual-connect jabber-virtual-send))
  "Alist of connection methods and functions.
First item is the symbol naming the method.
Second item is the connect function.
Third item is the send function.")
;; jabber-connect-methods:1 ends here

;; [[file:jabber.org::#get-connect-function][jabber-get-connect-function:1]]
(defun jabber-get-connect-function (type)
  "Get the connect function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq type jabber-connect-methods)))
    (nth 1 entry)))
;; jabber-get-connect-function:1 ends here

;; [[file:jabber.org::#get-send-function][jabber-get-send-function:1]]
(defun jabber-get-send-function (type)
  "Get the send function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq type jabber-connect-methods)))
    (nth 2 entry)))
;; jabber-get-send-function:1 ends here

;; [[file:jabber.org::#srv-targets][jabber-srv-targets:1]]
(defun jabber-srv-targets (server network-server port)
  "Find host and port to connect to.
If NETWORK-SERVER and/or PORT are specified, use them.
If we can't find SRV records, use standard defaults."
  ;; If the user has specified a host or a port, obey that.
  (if (or network-server port)
      (list (cons (or network-server server)
		  (or port 5222)))
    (or (condition-case nil
	    (srv-lookup (concat "_xmpp-client._tcp." server))
	  (error nil))
	(list (cons server 5222)))))
;; jabber-srv-targets:1 ends here

;; [[file:jabber.org::#network-connect][jabber-network-connect:1]]
;; Plain TCP/IP connection
(defun jabber-network-connect (fsm server network-server port)
  "Connect to a Jabber server with a plain network connection.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (cond
   ((featurep 'make-network-process '(:nowait t))
    ;; We can connect asynchronously!
    (jabber-network-connect-async fsm server network-server port))
   (t
    ;; Connecting to the server will block Emacs.
    (jabber-network-connect-sync fsm server network-server port))))
;; jabber-network-connect:1 ends here

;; [[file:jabber.org::#network-connect-async][jabber-network-connect-async:1]]
(defun jabber-network-connect-async (fsm server network-server port)
  ;; Get all potential targets...
  (let ((targets (jabber-srv-targets server network-server port))
		errors
		(fsm fsm))
    ;; ...and connect to them one after another, asynchronously, until
    ;; connection succeeds.
    (cl-labels
	((connect
	  (target remaining-targets)
	  (let ((target target) (remaining-targets remaining-targets))
	    (cl-labels ((connection-successful
		      (c)
		      ;; This mustn't be `fsm-send-sync', because the FSM
		      ;; needs to change the sentinel, which cannot be done
		      ;; from inside the sentinel.
		      (fsm-send fsm (list :connected c)))
		     (connection-failed
		      (c status)
		      (when (and (> (length status) 0)
				 (eq (aref status (1- (length status))) ?\n))
			(setq status (substring status 0 -1)))
		      (let ((err
			     (format "Couldn't connect to %s:%s: %s"
				     (car target) (cdr target) status)))
			(message "%s" err)
			(push err errors))
		      (when c (delete-process c))
		      (if remaining-targets
			  (progn
			    (message
			     "Connecting to %s:%s..."
			     (caar remaining-targets) (cdar remaining-targets))
			    (connect (car remaining-targets) (cdr remaining-targets)))
			(fsm-send fsm (list :connection-failed (nreverse errors))))))
	      (condition-case e
		  (make-network-process
		   :name "jabber"
		   :buffer (generate-new-buffer jabber-process-buffer)
		   :host (car target) :service (cdr target)
		   :coding 'utf-8
		   :nowait t
		   :sentinel
		   (let ((_target target) (_remaining-targets remaining-targets))
		     (lambda (connection status)
		       (cond
			((string-match "^open" status)
			 (connection-successful connection))
			((string-match "^failed" status)
			 (connection-failed connection status))
			((string-match "^deleted" status)
			 ;; This happens when we delete a process in the
			 ;; "failed" case above.
			 nil)
			(t
			 (message "Unknown sentinel status `%s'" status))))))
		(file-error
		 ;; A file-error has the error message in the third list
		 ;; element.
		 (connection-failed nil (car (cddr e))))
		(error
		 ;; Not sure if we ever get anything but file-errors,
		 ;; but let's make sure we report them:
		 (connection-failed nil (error-message-string e))))))))
      (message "Connecting to %s:%s..." (caar targets) (cdar targets))
      (connect (car targets) (cdr targets)))))
;; jabber-network-connect-async:1 ends here

;; [[file:jabber.org::#network-connect-sync][jabber-network-connect-sync:1]]
(defun jabber-network-connect-sync (fsm server network-server port)
  ;; This code will AFAIK only be used on Windows.  Apologies in
  ;; advance for any bit rot...
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(targets (jabber-srv-targets server network-server port))
	errors)
    (catch 'connected
      (dolist (target targets)
	(condition-case e
	    (let ((process-buffer (generate-new-buffer jabber-process-buffer))
		  connection)
	      (unwind-protect
		  (setq connection (open-network-stream
				    "jabber"
				    process-buffer
				    (car target)
				    (cdr target)))

		(unless (or connection jabber-debug-keep-process-buffers)
		  (kill-buffer process-buffer)))

	      (when connection
		(fsm-send fsm (list :connected connection))
		(throw 'connected connection)))
	  (file-error
	   ;; A file-error has the error message in the third list
	   ;; element.
	   (let ((err (format "Couldn't connect to %s:%s: %s"
			      (car target) (cdr target)
			      (car (cddr e)))))
	     (message "%s" err)
	     (push err errors)))
	  (error
	   ;; Not sure if we ever get anything but file-errors,
	   ;; but let's make sure we report them:
	   (let ((err (format "Couldn't connect to %s:%s: %s"
			      (car target) (cdr target)
			      (error-message-string e))))
	     (message "%s" err)
	     (push err errors)))))
      (fsm-send fsm (list :connection-failed (nreverse errors))))))
;; jabber-network-connect-sync:1 ends here

;; [[file:jabber.org::#network-send][jabber-network-send:1]]
(defun jabber-network-send (connection string)
  "Send a string via a plain TCP/IP connection to the Jabber Server."
  (process-send-string connection string))
;; jabber-network-send:1 ends here

;; [[file:jabber.org::#ssl-connect][jabber-ssl-connect:1]]
;; SSL connection, we use openssl's s_client function for encryption
;; of the link
;; TODO: make this configurable
(defun jabber-ssl-connect (fsm server network-server port)
  "Connect via OpenSSL or GnuTLS to a Jabber Server.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(connect-function
	 (cond
	  ((and (memq jabber-connection-ssl-program '(nil gnutls))
		(fboundp 'open-tls-stream))
	   'open-tls-stream)
	  ((and (memq jabber-connection-ssl-program '(nil openssl))
		(fboundp 'open-ssl-stream))
	   'open-ssl-stream)
	  (t
	   (error "Neither TLS nor SSL connect functions available"))))
	error-msg)
    (let ((process-buffer (generate-new-buffer jabber-process-buffer))
	  connection)
      (setq network-server (or network-server server))
      (setq port (or port 5223))
      (condition-case e
	  (setq connection (funcall connect-function
				    "jabber"
				    process-buffer
				    network-server
				    port))
	(error
	 (setq error-msg
	       (format "Couldn't connect to %s:%d: %s" network-server port
		       (error-message-string e)))
	 (message "%s" error-msg)))
      (unless (or connection jabber-debug-keep-process-buffers)
	(kill-buffer process-buffer))
      (if connection
	  (fsm-send fsm (list :connected connection))
	(fsm-send fsm (list :connection-failed
			    (when error-msg (list error-msg))))))))
;; jabber-ssl-connect:1 ends here

;; [[file:jabber.org::#ssl-send][jabber-ssl-send:1]]
(defun jabber-ssl-send (connection string)
  "Send a string via an SSL-encrypted connection to the Jabber Server."
  ;; It seems we need to send a linefeed afterwards.
  (process-send-string connection string)
  (process-send-string connection "\n"))
;; jabber-ssl-send:1 ends here

;; [[file:jabber.org::#starttls-connect][jabber-starttls-connect:1]]
(defun jabber-starttls-connect (fsm server network-server port)
  "Connect via an external GnuTLS process to a Jabber Server.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(targets (jabber-srv-targets server network-server port))
	errors)
    (unless (fboundp 'starttls-open-stream)
      (error "The starttls.el library is not available"))
    (catch 'connected
      (dolist (target targets)
	(condition-case e
	    (let ((process-buffer (generate-new-buffer jabber-process-buffer))
		  connection)
	      (unwind-protect
		  (setq connection
			(starttls-open-stream
			 "jabber"
			 process-buffer
			 (car target)
			 (cdr target)))
		(unless (or connection jabber-debug-keep-process-buffers)
		  (kill-buffer process-buffer)))
	      (if (null connection)
		  ;; It seems we don't actually get an error if we
		  ;; can't connect.  Let's try to convey some useful
		  ;; information to the user at least.
		  (let ((err (format "Couldn't connect to %s:%s"
				     (car target) (cdr target))))
		    (message "%s" err)
		    (push err errors))
		(fsm-send fsm (list :connected connection))
		(throw 'connected connection)))
	  (error
	   (let ((err (format "Couldn't connect to %s: %s" target
			      (error-message-string e))))
	     (message "%s" err)
	     (push err errors)))))
	(fsm-send fsm (list :connection-failed (nreverse errors))))))
;; jabber-starttls-connect:1 ends here

;; [[file:jabber.org::#starttls-initiate][jabber-starttls-initiate:1]]
(defun jabber-starttls-initiate (fsm)
  "Initiate a starttls connection."
  (jabber-send-sexp fsm
   '(starttls ((xmlns . "urn:ietf:params:xml:ns:xmpp-tls")))))
;; jabber-starttls-initiate:1 ends here

;; [[file:jabber.org::#starttls-process-input][jabber-starttls-process-input:1]]
(defun jabber-starttls-process-input (fsm xml-data)
  "Process result of starttls request.
On failure, signal error.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (cond
   ((eq (car xml-data) 'proceed)
    (let* ((state-data (fsm-get-state-data fsm))
	   (connection (plist-get state-data :connection)))
      ;; Did we use open-network-stream or starttls-open-stream?  We
      ;; can tell by process-type.
      (cl-case (process-type connection)
	(network
	 (let* ((hostname (plist-get state-data :server))
		(verifyp (not (member hostname jabber-invalid-certificate-servers))))
	   ;; gnutls-negotiate might signal an error, which is caught
	   ;; by our caller
	   (gnutls-negotiate
	    :process connection
	    ;; This is the hostname that the certificate should be valid for:
	    :hostname hostname
	    :verify-hostname-error verifyp
	    :verify-error verifyp)))
	(real
	 (or
	  (starttls-negotiate connection)
	  (error "Negotiation failure"))))))
   ((eq (car xml-data) 'failure)
    (error "Command rejected by server"))))
;; jabber-starttls-process-input:1 ends here

;; [[file:jabber.org::#*jabber-virtual-server-function*][*jabber-virtual-server-function*:1]]
(defvar *jabber-virtual-server-function* nil
  "Function to use for sending stanzas on a virtual connection.
The function should accept two arguments, the connection object
and a string that the connection wants to send.")
;; *jabber-virtual-server-function*:1 ends here

;; [[file:jabber.org::#virtual-connect][jabber-virtual-connect:1]]
(defun jabber-virtual-connect (fsm _server _network-server _port)
  "Connect to a virtual \"server\".
Use `*jabber-virtual-server-function*' as send function.
FSM is the finite state machine created in jabber.el library."
  (unless (functionp *jabber-virtual-server-function*)
    (error "No virtual server function specified"))
  ;; We pass the fsm itself as "connection object", as that is what a
  ;; virtual server needs to send stanzas.
  (fsm-send fsm (list :connected fsm)))
;; jabber-virtual-connect:1 ends here

;; [[file:jabber.org::#virtual-send][jabber-virtual-send:1]]
(defun jabber-virtual-send (connection string)
  (funcall *jabber-virtual-server-function* connection string))
;; jabber-virtual-send:1 ends here

;; [[file:jabber.org::#sasl-authentication][SASL authentication:1]]
;;; This file uses sasl.el from FLIM or Gnus.  If it can't be found,
;;; jabber-core.el won't use the SASL functions.
(eval-and-compile
  (condition-case nil
      (require 'sasl)
    (error nil)))

;;; Alternatives to FLIM would be the command line utility of GNU SASL,
;;; or anything the Gnus people decide to use.

;;; See XMPP-CORE and XMPP-IM for details about the protocol.
;; SASL authentication:1 ends here

;; [[file:jabber.org::#sasl-start-auth][jabber-sasl-start-auth:1]]
(defun jabber-sasl-start-auth (jc stream-features)
"Start the SASL authentication mechanism.
JC is The Jabber Connection.
STREAM-FEATURES the XML parsed \"stream features\" answer (it is used
with `jabber-xml-get-chidlren')."
  ;; Find a suitable common mechanism.
  (let* ((mechanism-elements (car (jabber-xml-get-children stream-features 'mechanisms)))
	 (mechanisms (mapcar
		      (lambda (tag)
			(car (jabber-xml-node-children tag)))
		      (jabber-xml-get-children mechanism-elements 'mechanism)))
	 (mechanism
	  (if (and (member "ANONYMOUS" mechanisms)
		   (or jabber-silent-mode (yes-or-no-p "Use anonymous authentication? ")))
	      (sasl-find-mechanism '("ANONYMOUS"))
	    (sasl-find-mechanism mechanisms))))

    ;; No suitable mechanism?
    (if (null mechanism)
	;; Maybe we can use legacy authentication
	(let ((iq-auth (cl-find "http://jabber.org/features/iq-auth"
			  (jabber-xml-get-children stream-features 'auth)
			  :key #'jabber-xml-get-xmlns
			  :test #'string=))
	      ;; Or maybe we have to use STARTTLS, but can't
	      (starttls (cl-find "urn:ietf:params:xml:ns:xmpp-tls"
			      (jabber-xml-get-children stream-features 'starttls)
			      :key #'jabber-xml-get-xmlns
			      :test #'string=)))
	  (cond
	   (iq-auth
	    (fsm-send jc :use-legacy-auth-instead))
	   (starttls
	    (message "STARTTLS encryption required, but disabled/non-functional at our end")
	    (fsm-send jc :authentication-failure))
	   (t
	    (message "Authentication failure: no suitable SASL mechanism found")
	    (fsm-send jc :authentication-failure))))

      ;; Watch for plaintext logins over unencrypted connections
      (if (and (not (plist-get (fsm-get-state-data jc) :encrypted))
	       (member (sasl-mechanism-name mechanism)
		       '("PLAIN" "LOGIN"))
	       (not (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? ")))
	  (fsm-send jc :authentication-failure)

	;; Start authentication.
	(let* (passphrase
	       (client (sasl-make-client mechanism
					 (plist-get (fsm-get-state-data jc) :username)
					 "xmpp"
					 (plist-get (fsm-get-state-data jc) :server)))
	       (sasl-read-passphrase (jabber-sasl-read-passphrase-closure
				      jc
				      (lambda (p) (setq passphrase (copy-sequence p)) p)))
	       (step (sasl-next-step client nil)))
	  (jabber-send-sexp
	   jc
	   `(auth ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl")
		   (mechanism . ,(sasl-mechanism-name mechanism)))
		  ,(when (sasl-step-data step)
		     (base64-encode-string (sasl-step-data step) t))))
	  (list client step passphrase))))))
;; jabber-sasl-start-auth:1 ends here

;; [[file:jabber.org::#sasl-read-passphrase-closure][jabber-sasl-read-passphrase-closure:1]]
(defun jabber-sasl-read-passphrase-closure (jc remember)
  "Return a lambda function suitable for `sasl-read-passphrase' for JC.
Call REMEMBER with the password.  REMEMBER is expected to return it as well."
  (let ((password (plist-get (fsm-get-state-data jc) :password))
		(bare-jid (jabber-connection-bare-jid jc))
		(remember remember))
    (if password
	(lambda (prompt) (funcall remember (copy-sequence password)))
      (lambda (prompt) (funcall remember (jabber-read-password bare-jid))))))
;; jabber-sasl-read-passphrase-closure:1 ends here

;; [[file:jabber.org::#sasl-process-input][jabber-sasl-process-input:1]]
(defun jabber-sasl-process-input (jc xml-data sasl-data)
"SASL protocol input processing.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((client (cl-first sasl-data))
	 (step (cl-second sasl-data))
	 (passphrase (cl-third sasl-data))
	 (sasl-read-passphrase (jabber-sasl-read-passphrase-closure
				jc
				(lambda (p) (setq passphrase (copy-sequence p)) p))))
    (cond
     ((eq (car xml-data) 'challenge)
      (sasl-step-set-data step (base64-decode-string (car (jabber-xml-node-children xml-data))))
      (setq step (sasl-next-step client step))
      (jabber-send-sexp
       jc
       `(response ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))
		  ,(when (sasl-step-data step)
		     (base64-encode-string (sasl-step-data step) t)))))

     ((eq (car xml-data) 'failure)
      (message "%s: authentication failure: %s"
	       (jabber-connection-bare-jid jc)
	       (jabber-xml-node-name (car (jabber-xml-node-children xml-data))))
      (fsm-send jc :authentication-failure))

     ((eq (car xml-data) 'success)
      ;; The server might, depending on the mechanism, send
      ;; "additional data" (see RFC 4422) with the <success/> element.
      ;; Since some SASL mechanisms perform mutual authentication, we
      ;; need to pass this data to sasl.el - we're not necessarily
      ;; done just because the server says we're done.
      (let* ((data (car (jabber-xml-node-children xml-data)))
	     (decoded (if data
			  (base64-decode-string data)
			"")))
	(sasl-step-set-data step decoded)
	(condition-case e
	    (progn
	      ;; Check that sasl-next-step doesn't signal an error.
	      ;; TODO: once sasl.el allows it, check that all steps have
	      ;; been completed.
	      (sasl-next-step client step)
	      (message "Authentication succeeded for %s" (jabber-connection-bare-jid jc))
	      (fsm-send jc (cons :authentication-success passphrase)))
	  (sasl-error
	   (message "%s: authentication failure: %s"
		    (jabber-connection-bare-jid jc)
		    (error-message-string e))
	   (fsm-send jc :authentication-failure))))))
    (list client step passphrase)))
;; jabber-sasl-process-input:1 ends here

;; [[file:jabber.org::#common-keymap-many-modes][common keymap for many modes:1]]
;; button.el was introduced in Emacs 22
(condition-case e
    (require 'button)
  (error nil))
;; common keymap for many modes:1 ends here

;; [[file:jabber.org::#common-keymap][jabber-common-keymap:1]]
(defvar jabber-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'jabber-popup-chat-menu)
    (define-key map "\C-c\C-r" 'jabber-popup-roster-menu)
    (define-key map "\C-c\C-i" 'jabber-popup-info-menu)
    (define-key map "\C-c\C-m" 'jabber-popup-muc-menu)
    (define-key map "\C-c\C-s" 'jabber-popup-service-menu)
    ;; note that {forward,backward}-button are not autoloaded.
    ;; thus the `require' above.
    (when (fboundp 'forward-button)
      (define-key map [?\t] 'forward-button)
      (define-key map [backtab] 'backward-button))
    map))
;; jabber-common-keymap:1 ends here

;; [[file:jabber.org::#global-keymap][jabber-global-keymap:1]]
;;;###autoload
(defvar jabber-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" 'jabber-connect-all)
    (define-key map "\C-d" 'jabber-disconnect)
    (define-key map "\C-r" 'jabber-switch-to-roster-buffer)
    (define-key map "\C-j" 'jabber-chat-with)
    (define-key map "\C-l" 'jabber-activity-switch-to)
    (define-key map "\C-a" 'jabber-send-away-presence)
    (define-key map "\C-o" 'jabber-send-default-presence)
    (define-key map "\C-x" 'jabber-send-xa-presence)
    (define-key map "\C-p" 'jabber-send-presence)
    map)
  "Global Jabber keymap (usually under C-x C-j).")
;; jabber-global-keymap:1 ends here

;; [[file:jabber.org::#global-keymap][jabber-global-keymap:2]]
;;;###autoload
(define-key ctl-x-map "\C-j" jabber-global-keymap)
;; jabber-global-keymap:2 ends here

;; [[file:jabber.org::#xml-console-mode][XML Console mode:1]]
(require 'ewoc)
(require 'sgml-mode) ;we base on this mode to hightlight XML
;; XML Console mode:1 ends here

;; [[file:jabber.org::#console-name-format][jabber-console-name-format:1]]
(defcustom jabber-console-name-format "*-jabber-console-%s-*"
  "Format for console buffer name.  %s mean connection jid."
  :type 'string
  :group 'jabber-debug)
;; jabber-console-name-format:1 ends here

;; [[file:jabber.org::#console-truncate-lines][jabber-console-truncate-lines:1]]
(defcustom jabber-console-truncate-lines 3000
  "Maximum number of lines in console buffer.
Not truncate if set to 0."
  :type 'integer
  :group 'jabber-debug)
;; jabber-console-truncate-lines:1 ends here

;; [[file:jabber.org::#point-insert][jabber-point-insert:1]]
(defvar jabber-point-insert nil
  "Position where the message being composed starts.")
;; jabber-point-insert:1 ends here

;; [[file:jabber.org::#send-function][jabber-send-function:1]]
(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")
;; jabber-send-function:1 ends here

;; [[file:jabber.org::#console-mode-hook][jabber-console-mode-hook:1]]
(defvar jabber-console-mode-hook nil
  "Hook called at the end of `jabber-console-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")
;; jabber-console-mode-hook:1 ends here

;; [[file:jabber.org::#console-ewoc][jabber-console-ewoc:1]]
(defvar jabber-console-ewoc nil
  "The ewoc showing the XML elements of this stream buffer.")
;; jabber-console-ewoc:1 ends here

;; [[file:jabber.org::#console-mode-map][jabber-console-mode-map:1]]
(defvar jabber-console-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map "\r" 'jabber-chat-buffer-send)
    map))
;; jabber-console-mode-map:1 ends here

;; [[file:jabber.org::#console-create-buffer][jabber-console-create-buffer:1]]
(defun jabber-console-create-buffer (jc)
  (with-current-buffer
	  (get-buffer-create (format jabber-console-name-format (jabber-connection-bare-jid jc)))
    (unless (eq major-mode 'jabber-console-mode)
      (jabber-console-mode))
    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)
    (current-buffer)))
;; jabber-console-create-buffer:1 ends here

;; [[file:jabber.org::#console-send][jabber-console-send:1]]
(defun jabber-console-send (jc data)
  ;; Put manual string into buffers ewoc
  (jabber-process-console jc "raw" data)
  ;; ...than sent it to server
  (jabber-send-string jc data))
;; jabber-console-send:1 ends here

;; [[file:jabber.org::#console-comment][jabber-console-comment:1]]
(defun jabber-console-comment (str)
  "Insert comment into console buffer."
  (let ((string (concat
                 comment-start str "@" (jabber-encode-time (current-time)) ":"
                 comment-end "\n")))
    (when (stringp jabber-debug-log-xml)
      (jabber-append-string-to-file string jabber-debug-log-xml))
    (insert string)))
;; jabber-console-comment:1 ends here

;; [[file:jabber.org::#console-pp][jabber-console-pp:1]]
(defun jabber-console-pp (data)
  "Pretty Printer for XML-sexp and raw data."
  (let ((direction (car data))
        (xml-list (cdr data))
        (raw (cadr data)))
    (jabber-console-comment direction)
    (if (stringp raw)
        ;; raw code input
        (progn
          (insert raw)
          (when (stringp jabber-debug-log-xml)
            (jabber-append-string-to-file raw jabber-debug-log-xml)))
      ;; receive/sending
      (progn
        (xml-print xml-list)
        (when (stringp jabber-debug-log-xml)
          (jabber-append-string-to-file
           "\n" jabber-debug-log-xml 'xml-print xml-list))))))
;; jabber-console-pp:1 ends here

;; [[file:jabber.org::#console-mode][jabber-console-mode:1]]
(define-derived-mode jabber-console-mode sgml-mode "Jabber Console"
  "Major mode for debug XMPP protocol."
  ;; Make sure to set this variable somewhere
  (make-local-variable 'jabber-send-function)
  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-console-ewoc)

  (setq jabber-send-function 'jabber-console-send)

  (unless jabber-console-ewoc
    (setq jabber-console-ewoc
	  (ewoc-create #'jabber-console-pp nil "<!-- + -->"))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker))))
;; jabber-console-mode:1 ends here

;; [[file:jabber.org::#console-mode][jabber-console-mode:2]]
(put 'jabber-console-mode 'mode-class 'special)
;; jabber-console-mode:2 ends here

;; [[file:jabber.org::#console-sanitize][jabber-console-sanitize:1]]
(defun jabber-console-sanitize (xml-data)
  "Sanitize XML-DATA for `jabber-process-console'."
  (if (listp xml-data)
      (jabber-tree-map (lambda (x) (if (numberp x) (format "%s" x) x)) xml-data)
    xml-data))
;; jabber-console-sanitize:1 ends here

;; [[file:jabber.org::#process-console][jabber-process-console:1]]
;;;###autoload
(defun jabber-process-console (jc direction xml-data)
  "Log XML-DATA i/o as XML in \"*-jabber-console-JID-*\" buffer."
  (let ((buffer (get-buffer-create (jabber-console-create-buffer jc))))
    (with-current-buffer buffer
      (progn
        (ewoc-enter-last jabber-console-ewoc (list direction (jabber-console-sanitize xml-data)))
		(when (< 1  jabber-console-truncate-lines)
		  (let ((jabber-log-lines-to-keep jabber-console-truncate-lines))
			(jabber-truncate-top buffer jabber-console-ewoc)))))))
;; jabber-process-console:1 ends here

;; [[file:jabber.org::#core][core:1]]
(eval-and-compile
  (or (ignore-errors (require 'fsm))
      (ignore-errors
        (let ((load-path (cons (expand-file-name
                                "jabber-fallback-lib"
                                (file-name-directory (locate-library "jabber")))
                               load-path)))
          (require 'fsm)))
      (error
       "The fsm library was not found in `load-path' or jabber-fallback-lib/ directory")))
;; core:1 ends here

;; [[file:jabber.org::#connections][jabber-connections:1]]
(defvar jabber-connections nil
  "List of jabber-connection FSMs.")
;; jabber-connections:1 ends here

;; [[file:jabber.org::#*jabber-roster*][*jabber-roster*:1]]
(defvar *jabber-roster* nil
  "The roster list.")
;; *jabber-roster*:1 ends here

;; [[file:jabber.org::#jid-obarray][jabber-jid-obarray:1]]
(defvar jabber-jid-obarray (make-vector 127 0)
  "Obarray for keeping JIDs.")
;; jabber-jid-obarray:1 ends here

;; [[file:jabber.org::#*jabber-disconnecting*][*jabber-disconnecting*:1]]
(defvar *jabber-disconnecting* nil
  "Boolean - are we in the process of disconnecting by free will.")
;; *jabber-disconnecting*:1 ends here

;; [[file:jabber.org::#message-chain][jabber-message-chain:1]]
(defvar jabber-message-chain nil
  "Incoming messages are sent to these functions, in order.")
;; jabber-message-chain:1 ends here

;; [[file:jabber.org::#iq-chain][jabber-iq-chain:1]]
(defvar jabber-iq-chain nil
  "Incoming infoqueries are sent to these functions, in order.")
;; jabber-iq-chain:1 ends here

;; [[file:jabber.org::#presence-chain][jabber-presence-chain:1]]
(defvar jabber-presence-chain nil
  "Incoming presence notifications are sent to these functions, in order.")
;; jabber-presence-chain:1 ends here

;; [[file:jabber.org::#namespace-prefixes][jabber-namespace-prefixes:1]]
(defvar jabber-namespace-prefixes nil
  "XML namespace prefixes used for the current connection.")
(make-variable-buffer-local 'jabber-namespace-prefixes)
;; jabber-namespace-prefixes:1 ends here

;; [[file:jabber.org::#core][jabber-core:1]]
(defgroup jabber-core nil "customize core functionality."
  :group 'jabber)
;; jabber-core:1 ends here

;; [[file:jabber.org::#post-connect-hooks][jabber-post-connect-hooks:1]]
(defcustom jabber-post-connect-hooks '(jabber-send-current-presence
				       jabber-muc-autojoin
				       jabber-whitespace-ping-start
				       jabber-vcard-avatars-find-current)
  "*Hooks run after successful connection and authentication.
The functions should accept one argument, the connection object."
  :type 'hook
  :options '(jabber-send-current-presence
	     jabber-muc-autojoin
	     jabber-whitespace-ping-start
	     jabber-keepalive-start
	     jabber-vcard-avatars-find-current
	     jabber-autoaway-start)
  :group 'jabber-core)
;; jabber-post-connect-hooks:1 ends here

;; [[file:jabber.org::#pre-disconnect-hook][jabber-pre-disconnect-hook:1]]
(defcustom jabber-pre-disconnect-hook nil
  "*Hooks run just before voluntary disconnection.
This might be due to failed authentication."
  :type 'hook
  :group 'jabber-core)
;; jabber-pre-disconnect-hook:1 ends here

;; [[file:jabber.org::#lost-connection-hooks][jabber-lost-connection-hooks:1]]
(defcustom jabber-lost-connection-hooks nil
  "*Hooks run after involuntary disconnection.
The functions are called with one argument: the connection object."
  :type 'hook
  :group 'jabber-core)
;; jabber-lost-connection-hooks:1 ends here

;; [[file:jabber.org::#post-disconnect-hook][jabber-post-disconnect-hook:1]]
(defcustom jabber-post-disconnect-hook nil
  "*Hooks run after disconnection."
  :type 'hook
  :group 'jabber-core)
;; jabber-post-disconnect-hook:1 ends here

;; [[file:jabber.org::#auto-reconnect][jabber-auto-reconnect:1]]
(defcustom jabber-auto-reconnect nil
  "Reconnect automatically after losing connection?
This will be of limited use unless you have the password library
installed, and have configured it to cache your password
indefinitely.  See `password-cache' and `password-cache-expiry'."
  :type 'boolean
  :group 'jabber-core)
;; jabber-auto-reconnect:1 ends here

;; [[file:jabber.org::#reconnect-delay][jabber-reconnect-delay:1]]
(defcustom jabber-reconnect-delay 5
  "Seconds to wait before reconnecting."
  :type 'integer
  :group 'jabber-core)
;; jabber-reconnect-delay:1 ends here

;; [[file:jabber.org::#roster-buffer][jabber-roster-buffer:1]]
(defcustom jabber-roster-buffer "*-jabber-roster-*"
  "The name of the roster buffer."
  :type 'string
  :group 'jabber-core)
;; jabber-roster-buffer:1 ends here

;; [[file:jabber.org::#process-buffer][jabber-process-buffer:1]]
(defvar jabber-process-buffer " *-jabber-process-*"
  "The name of the process buffer.")
;; jabber-process-buffer:1 ends here

;; [[file:jabber.org::#use-sasl][jabber-use-sasl:1]]
(defcustom jabber-use-sasl t
  "If non-nil, use SASL if possible.
SASL will still not be used if the library for it is missing or
if the server doesn't support it.

Disabling this shouldn't be necessary, but it may solve certain
problems."
  :type 'boolean
  :group 'jabber-core)
;; jabber-use-sasl:1 ends here

;; [[file:jabber.org::#have-sasl-p][jabber-have-sasl-p:1]]
(defsubst jabber-have-sasl-p ()
  "Return non-nil if SASL functions are available."
  (featurep 'sasl))
;; jabber-have-sasl-p:1 ends here

;; [[file:jabber.org::#account-history][jabber-account-history:1]]
(defvar jabber-account-history ()
  "Keeps track of previously used jabber accounts.")
;; jabber-account-history:1 ends here

;; [[file:jabber.org::#connection-type-history][jabber-connection-type-history:1]]
(defvar jabber-connection-type-history ()
  "Keeps track of previously used connection types.")
;; jabber-connection-type-history:1 ends here

;; [[file:jabber.org::#connect-all][jabber-connect-all:1]]
;;;###autoload (autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
(defun jabber-connect-all (&optional arg)
  "Connect to all configured Jabber accounts.
See `jabber-account-list'.
If no accounts are configured (or with prefix argument), call `jabber-connect'
interactively.
With many prefix arguments, one less is passed to `jabber-connect'."
  (interactive "P")
  (let ((accounts
	 (cl-remove-if (lambda (account)
		      (cdr (assq :disabled (cdr account))))
		    jabber-account-list)))
    (if (or (null accounts) arg)
	(let ((current-prefix-arg
	       (cond
		;; A number of C-u's; remove one, so to speak.
		((consp arg)
		 (if (> (car arg) 4)
		     (list (/ (car arg) 4))
		   nil))
		;; Otherwise, we just don't care.
		(t
		 arg))))
	  (call-interactively 'jabber-connect))
      ;; Only connect those accounts that are not yet connected.
      (let ((already-connected (mapcar #'jabber-connection-original-jid jabber-connections))
	    (connected-one nil))
	(dolist (account accounts)
	  (unless (member (jabber-jid-user (car account)) already-connected)
	    (let* ((jid (car account))
		   (alist (cdr account))
		   (password (cdr (assq :password alist)))
		   (network-server (cdr (assq :network-server alist)))
		   (port (cdr (assq :port alist)))
		   (connection-type (cdr (assq :connection-type alist))))
	      (jabber-connect
	       (jabber-jid-username jid)
	       (jabber-jid-server jid)
	       (jabber-jid-resource jid)
	       nil password network-server
	       port connection-type)
	      (setq connected-one t))))
	(unless connected-one
	  (message "All configured Jabber accounts are already connected"))))))
;; jabber-connect-all:1 ends here

;; [[file:jabber.org::#connect][jabber-connect:1]]
;;;###autoload (autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)
(defun jabber-connect (username server resource &optional
				registerp password network-server
				port connection-type)
  "Connect to the Jabber server and start a Jabber XML stream.
With prefix argument, register a new account.
With double prefix argument, specify more connection details."
  (interactive
   (let* ((jid (completing-read "Enter your JID: " jabber-account-list nil nil nil 'jabber-account-history))
	  (entry (assoc jid jabber-account-list))
	  (alist (cdr entry))
	  password network-server port connection-type registerp)
     (when (zerop (length jid))
       (error "No JID specified"))
     (unless (jabber-jid-username jid)
       (error "Missing username part in JID"))
     (when entry
       ;; If the user entered the JID of one of the preconfigured
       ;; accounts, use that data.
       (setq password (cdr (assq :password alist)))
       (setq network-server (cdr (assq :network-server alist)))
       (setq port (cdr (assq :port alist)))
       (setq connection-type (cdr (assq :connection-type alist))))
     (when (equal current-prefix-arg '(16))
       ;; Double prefix arg: ask about everything.
       ;; (except password, which is asked about later anyway)
       (setq password nil)
       (setq network-server
	     (read-string (format "Network server: (default `%s') " network-server)
			  nil nil network-server))
       (when (zerop (length network-server))
	 (setq network-server nil))
       (setq port
	     (car
	      (read-from-string
	       (read-string (format "Port: (default `%s') " port)
			    nil nil (if port (number-to-string port) "nil")))))
       (setq connection-type
	     (car
	      (read-from-string
	       (let ((default (symbol-name (or connection-type jabber-default-connection-type))))
		 (completing-read
		  (format "Connection type: (default `%s') " default)
		  (mapcar (lambda (type)
			    (cons (symbol-name (car type)) nil))
			  jabber-connect-methods)
		  nil t nil 'jabber-connection-type-history default)))))
       (setq registerp (or jabber-silent-mode (yes-or-no-p "Register new account? "))))
     (when (equal current-prefix-arg '(4))
       (setq registerp t))

     (list (jabber-jid-username jid)
	   (jabber-jid-server jid)
	   (jabber-jid-resource jid)
	   registerp password network-server port connection-type)))

  (if (member (list username
		    server)
	      (mapcar
	       (lambda (c)
		 (let ((data (fsm-get-state-data c)))
		   (list (plist-get data :username)
			 (plist-get data :server))))
	       jabber-connections))
      (message "Already connected to %s@%s"
	       username server)
    ;;(jabber-clear-roster)

    (push (start-jabber-connection username server resource
				   registerp password
				   network-server port connection-type)
	  jabber-connections)))
;; jabber-connect:1 ends here

;; [[file:jabber.org::#connection][jabber-connection:1]]
(define-state-machine jabber-connection
  :start ((username server resource registerp password network-server port connection-type)
	  "Start a Jabber connection."
	  (let* ((connection-type
		  (or connection-type jabber-default-connection-type))
		 (send-function
		  (jabber-get-send-function connection-type)))

	    (list :connecting
		  (list :send-function send-function
			;; Save the JID we originally connected with.
			:original-jid (concat username "@" server)
			:username username
			:server server
			:resource resource
			:password password
			:registerp registerp
			:connection-type connection-type
			:encrypted (eq connection-type 'ssl)
			:network-server network-server
			:port port)))))
;; jabber-connection:1 ends here

;; [[file:jabber.org::#connection][jabber-connection:2]]
(define-enter-state jabber-connection nil
  (fsm state-data)
  ;; `nil' is the error state.

  ;; Close the network connection.
  (let ((connection (plist-get state-data :connection)))
    (when (processp connection)
      (let ((process-buffer (process-buffer connection)))
	(delete-process connection)
	(when (and (bufferp process-buffer)
		   (not jabber-debug-keep-process-buffers))
	  (kill-buffer process-buffer)))))
  (setq state-data (plist-put state-data :connection nil))
  ;; Clear MUC data
  (jabber-muc-connection-closed (jabber-connection-bare-jid fsm))
  ;; Remove lost connections from the roster buffer.
  (jabber-display-roster)
  (let ((expected (plist-get state-data :disconnection-expected))
	(reason (plist-get state-data :disconnection-reason))
	(ever-session-established (plist-get state-data :ever-session-established)))
    (unless expected
      (run-hook-with-args 'jabber-lost-connection-hooks fsm)
      (message "%s@%s%s: connection lost: `%s'"
	       (plist-get state-data :username)
	       (plist-get state-data :server)
	       (if (plist-get state-data :resource)
		   (concat "/" (plist-get state-data :resource))
		 "")
	       reason))

    (if (and jabber-auto-reconnect (not expected) ever-session-established)
	;; Reconnect after a short delay?
	(list state-data jabber-reconnect-delay)
      ;; Else the connection is really dead.  Remove it from the list
      ;; of connections.
      (setq jabber-connections
	    (delq fsm jabber-connections))
      (when jabber-mode-line-mode
        (jabber-mode-line-presence-update))
      (jabber-display-roster)
      ;; And let the FSM sleep...
      (list state-data nil))))
;; jabber-connection:2 ends here

;; [[file:jabber.org::#connection][jabber-connection:3]]
(define-state jabber-connection nil
  (fsm state-data event callback)
  ;; In the `nil' state, the connection is dead.  We wait for a
  ;; :timeout message, meaning to reconnect, or :do-disconnect,
  ;; meaning to cancel reconnection.
  (cl-case event
    (:timeout
     (list :connecting state-data))
    (:do-disconnect
     (setq jabber-connections
	    (delq fsm jabber-connections))
     (list nil state-data nil))))
;; jabber-connection:3 ends here

;; [[file:jabber.org::#connection][jabber-connection:4]]
(define-enter-state jabber-connection :connecting
  (fsm state-data)
  (let* ((connection-type (plist-get state-data :connection-type))
	 (connect-function (jabber-get-connect-function connection-type))
	 (server (plist-get state-data :server))
	 (network-server (plist-get state-data :network-server))
	 (port (plist-get state-data :port)))
    (funcall connect-function fsm server network-server port))
  (list state-data nil))
;; jabber-connection:4 ends here

;; [[file:jabber.org::#connection][jabber-connection:5]]
(define-state jabber-connection :connecting
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:connected
     (let ((connection (cadr event))
	   (registerp (plist-get state-data :registerp)))

       (setq state-data (plist-put state-data :connection connection))

       (when (processp connection)
	 ;; TLS connections leave data in the process buffer, which
	 ;; the XML parser will choke on.
	 (with-current-buffer (process-buffer connection)
	   (erase-buffer))

	 (set-process-filter connection (fsm-make-filter fsm))
	 (set-process-sentinel connection (fsm-make-sentinel fsm)))

       (list :connected state-data)))

    (:connection-failed
     (message "Jabber connection failed")
     (plist-put state-data :disconnection-reason
		(mapconcat #'identity (cadr event) "; "))
     (list nil state-data))

    (:do-disconnect
     ;; We don't have the connection object, so defer the disconnection.
     :defer)))
;; jabber-connection:5 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:1]]
(defsubst jabber-fsm-handle-sentinel (state-data event)
  "Handle sentinel event for jabber fsm."
  ;; We do the same thing for every state, so avoid code duplication.
  (let* ((string (car (cddr event)))
	 ;; The event string sometimes (always?) has a trailing
	 ;; newline, that we don't care for.
	 (trimmed-string
	  (if (eq ?\n (aref string (1- (length string))))
	      (substring string 0 -1)
	    string))
	 (new-state-data
	  ;; If we already know the reason (e.g. a stream error), don't
	  ;; overwrite it.
	  (if (plist-get state-data :disconnection-reason)
	      state-data
	    (plist-put state-data :disconnection-reason trimmed-string))))
    (list nil new-state-data)))
;; jabber-fsm-handle-sentinel:1 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:2]]
(define-enter-state jabber-connection :connected
  (fsm state-data)

  (jabber-send-stream-header fsm)

  ;; Next thing happening is the server sending its own <stream:stream> start tag.

  (list state-data nil))
;; jabber-fsm-handle-sentinel:2 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:3]]
(define-state jabber-connection :connected
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :connected state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stream-start
     (let ((session-id (cadr event))
	   (stream-version (car (cddr event))))
       (setq state-data
	     (plist-put state-data :session-id session-id))
       ;; the stream feature is only sent if the initiating entity has
       ;; sent 1.0 in the stream header. if sasl is not supported then
       ;; we don't send 1.0 in the header and therefore we shouldn't wait
       ;; even if 1.0 is present in the receiving stream.
       (cond
	;; Wait for stream features?
	((and stream-version
	      (>= (string-to-number stream-version) 1.0)
	      jabber-use-sasl
	      (jabber-have-sasl-p))
	 ;; Stay in same state...
	 (list :connected state-data))
	;; Register account?
	((plist-get state-data :registerp)
	 ;; XXX: require encryption for registration?
	 (list :register-account state-data))
	;; Legacy authentication?
	(t
	 (list :legacy-auth state-data)))))

    (:stanza
     (let ((stanza (cadr event)))
       (cond
	;; At this stage, we only expect a stream:features stanza.
	((not (eq (jabber-xml-node-name stanza) 'features))
	 (list nil (plist-put state-data
			      :disconnection-reason
			      (format "Unexpected stanza %s" stanza))))
	((and (jabber-xml-get-children stanza 'starttls)
	      (eq (plist-get state-data :connection-type) 'starttls))
	 (list :starttls state-data))
	;; XXX: require encryption for registration?
	((plist-get state-data :registerp)
	 ;; We could check for the <register/> element in stream
	 ;; features, but as a client we would only lose by doing
	 ;; that.
	 (list :register-account state-data))
	(t
	 (list :sasl-auth (plist-put state-data :stream-features stanza))))))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-fsm-handle-sentinel:3 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:4]]
(define-enter-state jabber-connection :starttls
  (fsm state-data)
  (jabber-starttls-initiate fsm)
  (list state-data nil))
;; jabber-fsm-handle-sentinel:4 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:5]]
(define-state jabber-connection :starttls
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :starttls state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (condition-case e
	 (progn
	   (jabber-starttls-process-input fsm (cadr event))
	   ;; Connection is encrypted.  Send a stream tag again.
	   (list :connected (plist-put state-data :encrypted t)))
       (error
	(let* ((msg (concat "STARTTLS negotiation failed: "
			    (error-message-string e)))
	       (new-state-data (plist-put state-data :disconnection-reason msg)))
	  (list nil new-state-data)))))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-fsm-handle-sentinel:5 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:6]]
(define-enter-state jabber-connection :register-account
  (fsm state-data)
  (jabber-get-register fsm nil)
  (list state-data nil))
;; jabber-fsm-handle-sentinel:6 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:7]]
(define-state jabber-connection :register-account
  (fsm state-data event callback)
  ;; The connection will be closed in jabber-register
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :register-account state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (or
      (jabber-process-stream-error (cadr event) state-data)
      (progn
	(jabber-process-input fsm (cadr event))
	(list :register-account state-data))))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-fsm-handle-sentinel:7 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:8]]
(define-enter-state jabber-connection :legacy-auth
  (fsm state-data)
  (jabber-get-auth fsm (plist-get state-data :server)
		   (plist-get state-data :session-id))
  (list state-data nil))
;; jabber-fsm-handle-sentinel:8 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:9]]
(define-state jabber-connection :legacy-auth
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :legacy-auth state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (or
      (jabber-process-stream-error (cadr event) state-data)
      (progn
	(jabber-process-input fsm (cadr event))
	(list :legacy-auth state-data))))

    (:authentication-success
     (jabber-cache-password (jabber-connection-bare-jid fsm) (cdr event))
     (list :session-established state-data))

    (:authentication-failure
     (jabber-uncache-password (jabber-connection-bare-jid fsm))
     ;; jabber-logon has already displayed a message
     (list nil (plist-put state-data
			  :disconnection-expected t)))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-fsm-handle-sentinel:9 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:10]]
(define-enter-state jabber-connection :sasl-auth
  (fsm state-data)
  (let ((new-state-data
	 (plist-put state-data
		    :sasl-data
		    (jabber-sasl-start-auth
		     fsm
		     (plist-get state-data
				:stream-features)))))
    (list new-state-data nil)))
;; jabber-fsm-handle-sentinel:10 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:11]]
(define-state jabber-connection :sasl-auth
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :sasl-auth state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (let ((new-sasl-data
	    (jabber-sasl-process-input
	     fsm (cadr event)
	     (plist-get state-data :sasl-data))))
       (list :sasl-auth (plist-put state-data :sasl-data new-sasl-data))))

    (:use-legacy-auth-instead
     (list :legacy-auth (plist-put state-data :sasl-data nil)))

    (:authentication-success
     (jabber-cache-password (jabber-connection-bare-jid fsm) (cdr event))
     (list :bind (plist-put state-data :sasl-data nil)))

    (:authentication-failure
     (jabber-uncache-password (jabber-connection-bare-jid fsm))
     ;; jabber-sasl has already displayed a message
     (list nil (plist-put state-data
			  :disconnection-expected t)))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-fsm-handle-sentinel:11 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:12]]
(define-enter-state jabber-connection :bind
  (fsm state-data)
  (jabber-send-stream-header fsm)
  (list state-data nil))
;; jabber-fsm-handle-sentinel:12 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:13]]
(define-state jabber-connection :bind
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :bind state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stream-start
     ;; we wait for stream features...
     (list :bind state-data))

    (:stanza
     (let ((stanza (cadr event)))
       (cond
	((eq (jabber-xml-node-name stanza) 'features)
	 ;; Record stream features, discarding earlier data:
	 (setq state-data (plist-put state-data :stream-features stanza))
	 (if (jabber-xml-get-children stanza 'bind)
	     (let ((handle-bind
		    (lambda (jc xml-data success)
		      (fsm-send jc (list
				    (if success :bind-success :bind-failure)
				    xml-data))))
		   ;; So let's bind a resource.  We can either pick a resource ourselves,
		   ;; or have the server pick one for us.
		   (resource (plist-get state-data :resource)))
	       (jabber-send-iq fsm nil "set"
			       `(bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind"))
				      ,@(when resource
					  `((resource () ,resource))))
			       handle-bind t
			       handle-bind nil)
	       (list :bind state-data))
	   (message "Server doesn't permit resource binding")
	   (list nil state-data)))
	(t
	 (or
	  (jabber-process-stream-error (cadr event) state-data)
	  (progn
	    (jabber-process-input fsm (cadr event))
	    (list :bind state-data)))))))

    (:bind-success
     (let ((jid (jabber-xml-path (cadr event) '(bind jid ""))))
       ;; Maybe this isn't the JID we asked for.
       (plist-put state-data :username (jabber-jid-username jid))
       (plist-put state-data :server (jabber-jid-server jid))
       (plist-put state-data :resource (jabber-jid-resource jid)))

     ;; If the server follows the older RFCs 3920 and 3921, it may
     ;; offer session initiation here.  If it follows RFCs 6120 and
     ;; 6121, it might not offer it, and we should just skip it.
     (if (jabber-xml-get-children (plist-get state-data :stream-features) 'session)
	 (let ((handle-session
		(lambda (jc xml-data success)
		  (fsm-send jc (list
				(if success :session-success :session-failure)
				xml-data)))))
	   (jabber-send-iq fsm nil "set"
			   '(session ((xmlns . "urn:ietf:params:xml:ns:xmpp-session")))
			   handle-session t
			   handle-session nil)
	   (list :bind state-data))
       ;; Session establishment not offered - assume not necessary.
       (list :session-established state-data)))

    (:session-success
     ;; We have a session
     (list :session-established state-data))

    (:bind-failure
     (message "Resource binding failed: %s"
	      (jabber-parse-error
	       (jabber-iq-error (cadr event))))
     (list nil state-data))

    (:session-failure
     (message "Session establishing failed: %s"
	      (jabber-parse-error
	       (jabber-iq-error (cadr event))))
     (list nil state-data))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-fsm-handle-sentinel:13 ends here

;; [[file:jabber.org::#fsm-handle-sentinel][jabber-fsm-handle-sentinel:14]]
(define-enter-state jabber-connection :session-established
  (fsm state-data)
  (jabber-send-iq fsm nil
		  "get"
		  '(query ((xmlns . "jabber:iq:roster")))
		  #'jabber-process-roster 'initial
		  #'jabber-initial-roster-failure nil)
  (list (plist-put state-data :ever-session-established t) nil))
;; jabber-fsm-handle-sentinel:14 ends here

;; [[file:jabber.org::#pending-presence-timeout][jabber-pending-presence-timeout:1]]
(defvar jabber-pending-presence-timeout 0.5
  "Wait this long before doing presence packet batch processing.")
;; jabber-pending-presence-timeout:1 ends here

;; [[file:jabber.org::#pending-presence-timeout][jabber-pending-presence-timeout:2]]
(define-state jabber-connection :session-established
  (fsm state-data event callback)
  (cl-case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :session-established state-data :keep)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (or
      (jabber-process-stream-error (cadr event) state-data)
      (progn
	(jabber-process-input fsm (cadr event))
	(list :session-established state-data :keep))))

    (:roster-update
     ;; Batch up roster updates
     (let* ((jid-symbol-to-update (cdr event))
	    (pending-updates (plist-get state-data :roster-pending-updates)))
       ;; If there are pending updates, there is a timer running
       ;; already; just add the new symbol and wait.
       (if pending-updates
	   (progn
	     (unless (memq jid-symbol-to-update pending-updates)
	       (nconc pending-updates (list jid-symbol-to-update)))
	     (list :session-established state-data :keep))
	 ;; Otherwise, we need to create the list and start the timer.
	 (setq state-data
	       (plist-put state-data
			  :roster-pending-updates
			  (list jid-symbol-to-update)))
	 (list :session-established state-data jabber-pending-presence-timeout))))

    (:timeout
     ;; Update roster
     (let ((pending-updates (plist-get state-data :roster-pending-updates)))
       (setq state-data (plist-put state-data :roster-pending-updates nil))
       (jabber-roster-update fsm nil pending-updates nil)
       (list :session-established state-data)))

    (:send-if-connected
     ;; This is the only state in which we respond to such messages.
     ;; This is to make sure we don't send anything inappropriate
     ;; during authentication etc.
     (jabber-send-sexp fsm (cdr event))
     (list :session-established state-data :keep))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))
;; jabber-pending-presence-timeout:2 ends here

;; [[file:jabber.org::#disconnect][jabber-disconnect:1]]
(defun jabber-disconnect (&optional arg)
  "Disconnect from all Jabber servers.  If ARG supplied, disconnect one account."
  (interactive "P")
  (if arg
      (jabber-disconnect-one (jabber-read-account))
    (unless *jabber-disconnecting*	; avoid reentry
      (let ((*jabber-disconnecting* t))
	(if (null jabber-connections)
	    (message "Already disconnected")
	  (run-hooks 'jabber-pre-disconnect-hook)
	  (dolist (c jabber-connections)
	    (jabber-disconnect-one c t))
	  (setq jabber-connections nil)

	  (jabber-disconnected)
	  (when (called-interactively-p 'interactive)
	    (message "Disconnected from Jabber server(s)")))))))
;; jabber-disconnect:1 ends here

;; [[file:jabber.org::#disconnect-one][jabber-disconnect-one:1]]
(defun jabber-disconnect-one (jc &optional dont-redisplay)
  "Disconnect from one Jabber server.
If DONT-REDISPLAY is non-nil, don't update roster buffer.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (fsm-send-sync jc :do-disconnect)
  (when (called-interactively-p 'interactive)
    (message "Disconnected from %s"
	     (jabber-connection-jid jc)))
  (unless dont-redisplay
    (jabber-display-roster)))
;; jabber-disconnect-one:1 ends here

;; [[file:jabber.org::#disconnected][jabber-disconnected:1]]
(defun jabber-disconnected ()
  "Re-initialise jabber package variables.
Call this function after disconnection."
  (when (get-buffer jabber-roster-buffer)
    (with-current-buffer (get-buffer jabber-roster-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer))))

  (jabber-clear-roster)
  (run-hooks 'jabber-post-disconnect-hook))
;; jabber-disconnected:1 ends here

;; [[file:jabber.org::#log-xml][jabber-log-xml:1]]
(defun jabber-log-xml (fsm direction data)
  "Print DATA to XML console (and, optionally, in file).
If `jabber-debug-log-xml' is nil, do nothing.
FSM is the connection that is sending/receiving.
DIRECTION is a string, either \"sending\" or \"receive\".
DATA is any sexp."
  (when jabber-debug-log-xml
      (jabber-process-console fsm direction data)))
;; jabber-log-xml:1 ends here

;; [[file:jabber.org::#pre-filter][jabber-pre-filter:1]]
(defun jabber-pre-filter (process string fsm)
  (with-current-buffer (process-buffer process)
    ;; Append new data
    (goto-char (point-max))
    (insert string)

    (unless (boundp 'jabber-filtering)
      (let (jabber-filtering)
	(jabber-filter process fsm)))))
;; jabber-pre-filter:1 ends here

;; [[file:jabber.org::#filter][jabber-filter:1]]
(defun jabber-filter (process fsm)
  "The filter function for the jabber process."
  (with-current-buffer (process-buffer process)
    ;; Start from the beginning
    (goto-char (point-min))
    (let (xml-data)
      (cl-loop
       do
       ;; Skip whitespace
       (unless (zerop (skip-chars-forward " \t\r\n"))
	 (delete-region (point-min) (point)))
       ;; Skip processing directive
       (when (looking-at "<\\?xml[^?]*\\?>")
	 (delete-region (match-beginning 0) (match-end 0)))

       ;; Stream end?
       (when (looking-at "</stream:stream>")
	 (cl-return (fsm-send fsm :stream-end)))

       ;; Stream header?
       (when (looking-at "<stream:stream[^>]*\\(>\\)")
	 ;; Let's pretend that the stream header is a closed tag,
	 ;; and parse it as such.
	 (replace-match "/>" t t nil 1)
	 (let* ((ending-at (point))
		(stream-header (car (xml-parse-region (point-min) ending-at)))
		(session-id (jabber-xml-get-attribute stream-header 'id))
		(stream-version (jabber-xml-get-attribute stream-header 'version)))

	   ;; Need to keep any namespace attributes on the stream
	   ;; header, as they can affect any stanza in the
	   ;; stream...
	   (setq jabber-namespace-prefixes
		 (jabber-xml-merge-namespace-declarations
		  (jabber-xml-node-attributes stream-header)
		  nil))
	   (jabber-log-xml fsm "receive" stream-header)
	   (fsm-send fsm (list :stream-start session-id stream-version))
	   (delete-region (point-min) ending-at)))

       ;; Normal tag

       ;; XXX: do these checks make sense?  If so, reinstate them.
       ;;(if (active-minibuffer-window)
       ;;    (run-with-idle-timer 0.01 nil #'jabber-filter process string)

       ;; This check is needed for xml.el of Emacs 21, as it chokes on
       ;; empty attribute values.
       (save-excursion
	 (while (search-forward-regexp " \\w+=''" nil t)
           (replace-match "")))

       (setq xml-data (jabber-xml-parse-next-stanza))

       while xml-data
       do
       ;; If there's a problem with writing the XML log,
       ;; make sure the stanza is delivered, at least.
       (condition-case e
	   (jabber-log-xml fsm "receive" (car xml-data))
	 (error
	  (ding)
	  (message "Couldn't write XML log: %s" (error-message-string e))
	  (sit-for 2)))
       (delete-region (point-min) (point))

       (fsm-send fsm (list :stanza
			   (jabber-xml-resolve-namespace-prefixes
			    (car xml-data) nil jabber-namespace-prefixes)))
       ;; XXX: move this logic elsewhere
       ;; We explicitly don't catch errors in jabber-process-input,
       ;; to facilitate debugging.
       ;; (jabber-process-input (car xml-data))
       ))))
;; jabber-filter:1 ends here

;; [[file:jabber.org::#process-input][jabber-process-input:1]]
(defun jabber-process-input (jc xml-data)
  "Process an incoming parsed tag.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((tag (jabber-xml-node-name xml-data))
	 (functions (eval (cdr (assq tag '((iq . jabber-iq-chain)
					   (presence . jabber-presence-chain)
					   (message . jabber-message-chain)))))))
    (dolist (f functions)
      (condition-case e
	  (funcall f jc xml-data)
	((debug error)
	 (fsm-debug-output "Error %S while processing %S with function %s" e xml-data f))))))
;; jabber-process-input:1 ends here

;; [[file:jabber.org::#process-stream-error][jabber-process-stream-error:1]]
(defun jabber-process-stream-error (xml-data state-data)
  "Process an incoming stream error.
Return nil if XML-DATA is not a stream:error stanza.
Return an fsm result list if it is."
  (when (and (eq (jabber-xml-node-name xml-data) 'error)
	     (equal (jabber-xml-get-xmlns xml-data) "http://etherx.jabber.org/streams"))
    (let ((condition (jabber-stream-error-condition xml-data))
	  (text (jabber-parse-stream-error xml-data)))
      (setq state-data (plist-put state-data :disconnection-reason
				  (format "Stream error: %s" text)))
      ;; Special case: when the error is `conflict', we have been
      ;; forcibly disconnected by the same user.  Don't reconnect
      ;; automatically.
      (when (eq condition 'conflict)
	(setq state-data (plist-put state-data :disconnection-expected t)))
      (list nil state-data))))
;; jabber-process-stream-error:1 ends here

;; [[file:jabber.org::#clear-roster][jabber-clear-roster:1]]
;; XXX: This function should probably die.  The roster is stored
;; inside the connection plists, and the obarray shouldn't be so big
;; that we need to clean it.
(defun jabber-clear-roster ()
  "Clean up the roster."
  ;; This is made complicated by the fact that the JIDs are symbols with properties.
  (mapatoms #'(lambda (x)
		(unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))
;; jabber-clear-roster:1 ends here

;; [[file:jabber.org::#send-sexp][jabber-send-sexp:1]]
(defun jabber-send-sexp (jc sexp)
  "Send the xml corresponding to SEXP to connection JC."
  (condition-case e
      (jabber-log-xml jc "sending" sexp)
    (error
     (ding)
     (message "Couldn't write XML log: %s" (error-message-string e))
     (sit-for 2)))
  (jabber-send-string jc (jabber-sexp2xml sexp)))
;; jabber-send-sexp:1 ends here

;; [[file:jabber.org::#send-sexp-if-connected][jabber-send-sexp-if-connected:1]]
(defun jabber-send-sexp-if-connected (jc sexp)
  "Send the stanza SEXP only if JC has established a session."
  (fsm-send-sync jc (cons :send-if-connected sexp)))
;; jabber-send-sexp-if-connected:1 ends here

;; [[file:jabber.org::#send-stream-header][jabber-send-stream-header:1]]
(defun jabber-send-stream-header (jc)
  "Send stream header to connection JC."
  (let ((stream-header
         (concat "<?xml version='1.0'?><stream:stream to='"
		 (plist-get (fsm-get-state-data jc) :server)
		 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'"
		 ;; Not supporting SASL is not XMPP compliant,
		 ;; so don't pretend we are.
		 (if (and (jabber-have-sasl-p) jabber-use-sasl)
		     " version='1.0'"
		   "")
		 ">
")))
    (jabber-log-xml jc "sending" stream-header)
    (jabber-send-string jc stream-header)))
;; jabber-send-stream-header:1 ends here

;; [[file:jabber.org::#send-string][jabber-send-string:1]]
(defun jabber-send-string (jc string)
  "Send STRING through the connection JC."
  (let* ((state-data (fsm-get-state-data jc))
         (connection (plist-get state-data :connection))
         (send-function (plist-get state-data :send-function)))
    (unless connection
      (error "%s has no connection" (jabber-connection-jid jc)))
    (funcall send-function connection string)))
;; jabber-send-string:1 ends here

;; [[file:jabber.org::#logon][logon:1]]
(unless (fboundp 'sha1)
  (require 'sha1))
;; logon:1 ends here

;; [[file:jabber.org::#get-auth][jabber-get-auth:1]]
(defun jabber-get-auth (jc to session-id)
  "Send IQ get request in namespace \"jabber:iq:auth\".
JC is the Jabber connection."
  (jabber-send-iq jc to
		  "get"
		  `(query ((xmlns . "jabber:iq:auth"))
			  (username () ,(plist-get (fsm-get-state-data jc) :username)))
		  #'jabber-do-logon session-id
		  #'jabber-report-success "Impossible error - auth field request"))
;; jabber-get-auth:1 ends here

;; [[file:jabber.org::#do-logon][jabber-do-logon:1]]
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
;; jabber-do-logon:1 ends here

;; [[file:jabber.org::#process-logon][jabber-process-logon:1]]
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
;; jabber-process-logon:1 ends here

;; [[file:jabber.org::#displaying-roster][Displaying the roster:1]]
(require 'format-spec)
;; Displaying the roster:1 ends here

;; [[file:jabber.org::#roster][jabber-roster:1]]
(defgroup jabber-roster nil "roster display options"
  :group 'jabber)
;; jabber-roster:1 ends here

;; [[file:jabber.org::#roster-line-format][jabber-roster-line-format:1]]
(defcustom jabber-roster-line-format " %a %c %-25n %u %-8s  %S"
  "The format specification of the lines in the roster display.

These fields are available:

%a   Avatar, if any
%c   \"*\" if the contact is connected, or \" \" if not
%u   sUbscription state - see below
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%r   Highest-priority resource of contact
%s   Availability of contact as string (\"Online\", \"Away\" etc)
%S   Status string specified by contact

%u is replaced by one of the strings given by
`jabber-roster-subscription-display'."
  :type 'string
  :group 'jabber-roster)
;; jabber-roster-line-format:1 ends here

;; [[file:jabber.org::#roster-subscription-display][jabber-roster-subscription-display:1]]
(defcustom jabber-roster-subscription-display '(("none" . "   ")
						("from" . "<  ")
						("to" . "  >")
						("both" . "<->"))
  "Strings used for indicating subscription status of contacts.
\"none\" means that there is no subscription between you and the
contact.
\"from\" means that the contact has a subscription to you, but you
have no subscription to the contact.
\"to\" means that you have a subscription to the contact, but the
contact has no subscription to you.
\"both\" means a mutual subscription.

Having a \"presence subscription\" means being able to see the
other person's presence.

Some fancy arrows you might want to use, if your system can
display them: ← → ⇄ ↔."
  :type '(list (cons :format "%v" (const :format "" "none") (string :tag "None"))
	       (cons :format "%v" (const :format "" "from") (string :tag "From"))
	       (cons :format "%v" (const :format "" "to") (string :tag "To"))
	       (cons :format "%v" (const :format "" "both") (string :tag "Both")))
  :group 'jabber-roster)
;; jabber-roster-subscription-display:1 ends here

;; [[file:jabber.org::#resource-line-format][jabber-resource-line-format:1]]
(defcustom jabber-resource-line-format "     %r - %s (%S), priority %p"
  "The format specification of resource lines in the roster display.
These are displayed when `jabber-show-resources' permits it.

These fields are available:

%c   \"*\" if the contact is connected, or \" \" if not
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%p   Priority of this resource
%r   Name of this resource
%s   Availability of resource as string (\"Online\", \"Away\" etc)
%S   Status string specified by resource."
  :type 'string
  :group 'jabber-roster)
;; jabber-resource-line-format:1 ends here

;; [[file:jabber.org::#roster-sort-functions][jabber-roster-sort-functions:1]]
(defcustom jabber-roster-sort-functions
  '(jabber-roster-sort-by-status jabber-roster-sort-by-displayname)
  "Sort roster according to these criteria.

These functions should take two roster items A and B, and return:
<0 if A < B
0  if A = B
>0 if A > B."
  :type 'hook
  :options '(jabber-roster-sort-by-status
	     jabber-roster-sort-by-displayname
	     jabber-roster-sort-by-group)
  :group 'jabber-roster)
;; jabber-roster-sort-functions:1 ends here

;; [[file:jabber.org::#sort-order][jabber-sort-order:1]]
(defcustom jabber-sort-order '("chat" "" "away" "dnd" "xa")
  "Sort by status in this order.  Anything not in list goes last.
Offline is represented as nil."
  :type '(repeat (restricted-sexp :match-alternatives (stringp nil)))
  :group 'jabber-roster)
;; jabber-sort-order:1 ends here

;; [[file:jabber.org::#show-resources][jabber-show-resources:1]]
(defcustom jabber-show-resources 'sometimes
  "Show contacts' resources in roster?
This can be one of the following symbols:

nil       Never show resources
sometimes Show resources when there are more than one
always    Always show resources."
  :type '(radio (const :tag "Never" nil)
		(const :tag "When more than one connected resource" sometimes)
		(const :tag "Always" always))
  :group 'jabber-roster)
;; jabber-show-resources:1 ends here

;; [[file:jabber.org::#show-offline-contacts][jabber-show-offline-contacts:1]]
(defcustom jabber-show-offline-contacts t
  "Show offline contacts in roster when non-nil."
  :type 'boolean
  :group 'jabber-roster)
;; jabber-show-offline-contacts:1 ends here

;; [[file:jabber.org::#remove-newlines][jabber-remove-newlines:1]]
(defcustom jabber-remove-newlines t
  "Remove newlines in status messages?
Newlines in status messages mess up the roster display.  However,
they are essential to status message poets.  Therefore, you get to
choose the behaviour.

Trailing newlines are always removed, regardless of this variable."
  :type 'boolean
  :group 'jabber-roster)
;; jabber-remove-newlines:1 ends here

;; [[file:jabber.org::#roster-show-bindings][jabber-roster-show-bindings:1]]
(defcustom jabber-roster-show-bindings t
  "Show keybindings in roster buffer?."
  :type 'boolean
  :group 'jabber-roster)
;; jabber-roster-show-bindings:1 ends here

;; [[file:jabber.org::#roster-show-title][jabber-roster-show-title:1]]
(defcustom jabber-roster-show-title t
  "Show title in roster buffer?."
  :type 'boolean
  :group 'jabber-roster)
;; jabber-roster-show-title:1 ends here

;; [[file:jabber.org::#roster-mode-hook][jabber-roster-mode-hook:1]]
(defcustom jabber-roster-mode-hook nil
  "Hook run when entering Roster mode."
  :group 'jabber-roster
  :type 'hook)
;; jabber-roster-mode-hook:1 ends here

;; [[file:jabber.org::#roster-default-group-name][jabber-roster-default-group-name:1]]
(defcustom jabber-roster-default-group-name "other"
  "Default group name for buddies without groups."
  :group 'jabber-roster
  :type 'string
  :get '(lambda (var)
	  (let ((val (symbol-value var)))
	    (when (stringp val)
	      (set-text-properties 0 (length val) nil val))
	    val))
  :set '(lambda (var val)
          (when (stringp val)
	    (set-text-properties 0 (length val) nil val))
          (custom-set-default var val)))
;; jabber-roster-default-group-name:1 ends here

;; [[file:jabber.org::#roster-show-empty-group][jabber-roster-show-empty-group:1]]
(defcustom jabber-roster-show-empty-group nil
  "Show empty groups in roster?."
  :group 'jabber-roster
  :type 'boolean)
;; jabber-roster-show-empty-group:1 ends here

;; [[file:jabber.org::#roster-roll-up-group][jabber-roster-roll-up-group:1]]
(defcustom jabber-roster-roll-up-group nil
  "Show empty groups in roster?."
  :group 'jabber-roster
  :type 'boolean)
;; jabber-roster-roll-up-group:1 ends here

;; [[file:jabber.org::#roster-user-online][jabber-roster-user-online:1]]
(defface jabber-roster-user-online
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying online users."
  :group 'jabber-roster)
;; jabber-roster-user-online:1 ends here

;; [[file:jabber.org::#roster-user-xa][jabber-roster-user-xa:1]]
(defface jabber-roster-user-xa
  '((((background dark)) (:foreground "magenta" :weight normal :slant italic))
    (t (:foreground "black" :weight normal :slant italic)))
  "face for displaying extended away users."
  :group 'jabber-roster)
;; jabber-roster-user-xa:1 ends here

;; [[file:jabber.org::#roster-user-dnd][jabber-roster-user-dnd:1]]
(defface jabber-roster-user-dnd
  '((t (:foreground "red" :weight normal :slant italic)))
  "face for displaying do not disturb users."
  :group 'jabber-roster)
;; jabber-roster-user-dnd:1 ends here

;; [[file:jabber.org::#roster-user-away][jabber-roster-user-away:1]]
(defface jabber-roster-user-away
  '((t (:foreground "dark green" :weight normal :slant italic)))
  "face for displaying away users."
  :group 'jabber-roster)
;; jabber-roster-user-away:1 ends here

;; [[file:jabber.org::#roster-user-chatty][jabber-roster-user-chatty:1]]
(defface jabber-roster-user-chatty
  '((t (:foreground "dark orange" :weight bold :slant normal)))
  "face for displaying chatty users."
  :group 'jabber-roster)
;; jabber-roster-user-chatty:1 ends here

;; [[file:jabber.org::#roster-user-error][jabber-roster-user-error:1]]
(defface jabber-roster-user-error
  '((t (:foreground "red" :weight light :slant italic)))
  "face for displaying users sending presence errors."
  :group 'jabber-roster)
;; jabber-roster-user-error:1 ends here

;; [[file:jabber.org::#roster-user-offline][jabber-roster-user-offline:1]]
(defface jabber-roster-user-offline
  '((t (:foreground "dark grey" :weight light :slant italic)))
  "face for displaying offline users."
  :group 'jabber-roster)
;; jabber-roster-user-offline:1 ends here

;; [[file:jabber.org::#roster-debug][jabber-roster-debug:1]]
(defvar jabber-roster-debug nil
  "Debug roster draw.")
;; jabber-roster-debug:1 ends here

;; [[file:jabber.org::#roster-mode-map][jabber-roster-mode-map:1]]
(defvar jabber-roster-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map jabber-common-keymap)
    (define-key map [mouse-2] 'jabber-roster-mouse-2-action-at-point)
    (define-key map (kbd "TAB") 'jabber-go-to-next-roster-item)
    (define-key map (kbd "S-TAB") 'jabber-go-to-previous-roster-item)
    (define-key map (kbd "M-TAB") 'jabber-go-to-previous-roster-item)
    (define-key map (kbd "<backtab>") 'jabber-go-to-previous-roster-item)
    (define-key map (kbd "RET") 'jabber-roster-ret-action-at-point)
    (define-key map (kbd "C-k") 'jabber-roster-delete-at-point)

    (define-key map "e" 'jabber-roster-edit-action-at-point)
    (define-key map "s" 'jabber-send-subscription-request)
    (define-key map "q" 'bury-buffer)
    (define-key map "i" 'jabber-get-disco-items)
    (define-key map "j" 'jabber-muc-join)
    (define-key map "I" 'jabber-get-disco-info)
    (define-key map "b" 'jabber-get-browse)
    (define-key map "v" 'jabber-get-version)
    (define-key map "a" 'jabber-send-presence)
    (define-key map "g" 'jabber-display-roster)
    (define-key map "S" 'jabber-ft-send)
    (define-key map "o" 'jabber-roster-toggle-offline-display)
    (define-key map "H" 'jabber-roster-toggle-binding-display)
    ;;(define-key map "D" 'jabber-disconnect)
    map))
;; jabber-roster-mode-map:1 ends here

;; [[file:jabber.org::#roster-ret-action-at-point][jabber-roster-ret-action-at-point:1]]
(defun jabber-roster-ret-action-at-point ()
  "Action for ret.
Before try to roll up/down group.  Eval `chat-with-jid-at-point' is no group at
point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account))
        (jid-at-point (get-text-property (point)
					 'jabber-jid)))
    (if (and group-at-point account-at-point)
	(jabber-roster-roll-group account-at-point group-at-point)
      ;; Is this a normal contact, or a groupchat?  Let's ask it.
      (jabber-disco-get-info
       account-at-point (jabber-jid-user jid-at-point) nil
       #'jabber-roster-ret-action-at-point-1
       jid-at-point))))
;; jabber-roster-ret-action-at-point:1 ends here

;; [[file:jabber.org::#roster-ret-action-at-point-1][jabber-roster-ret-action-at-point-1:1]]
(defun jabber-roster-ret-action-at-point-1 (jc jid result)
  ;; If we get an error, assume it's a normal contact.
  (if (eq (car result) 'error)
      (jabber-chat-with jc jid)
    ;; Otherwise, let's check whether it has a groupchat identity.
    (let ((identities (car result)))
      (if (cl-find "conference" (if (sequencep identities) identities nil)
		:key (lambda (i) (aref i 1))
		:test #'string=)
	  ;; Yes!  Let's join it.
	  (jabber-muc-join jc jid
			   (jabber-muc-read-my-nickname jc jid t)
			   t)
	;; No.  Let's open a normal chat buffer.
	(jabber-chat-with jc jid)))))
;; jabber-roster-ret-action-at-point-1:1 ends here

;; [[file:jabber.org::#roster-mouse-2-action-at-point][jabber-roster-mouse-2-action-at-point:1]]
(defun jabber-roster-mouse-2-action-at-point (e)
  "Action for mouse 2.
Before try to roll up/down group.  Eval `chat-with-jid-at-point' is no group
at point."
  (interactive "e")
  (mouse-set-point e)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(jabber-roster-roll-group account-at-point group-at-point)
      (jabber-popup-combined-menu))))
;; jabber-roster-mouse-2-action-at-point:1 ends here

;; [[file:jabber.org::#roster-delete-at-point][jabber-roster-delete-at-point:1]]
(defun jabber-roster-delete-at-point ()
  "Delete at point from roster.
Try to delete the group from all contaacs.
Delete a jid if there is no group at point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(let ((jids-with-group
	       (gethash group-at-point
			(plist-get
			 (fsm-get-state-data account-at-point)
			 :roster-hash))))
	  (jabber-roster-delete-group-from-jids account-at-point
						jids-with-group
						group-at-point))
      (jabber-roster-delete-jid-at-point))))
;; jabber-roster-delete-at-point:1 ends here

;; [[file:jabber.org::#roster-edit-action-at-point][jabber-roster-edit-action-at-point:1]]
(defun jabber-roster-edit-action-at-point ()
  "Action for e.  Before try to edit group name.
Eval `jabber-roster-change' is no group at point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(let ((jids-with-group
	       (gethash group-at-point
			(plist-get
			 (fsm-get-state-data account-at-point)
			 :roster-hash))))
	  (jabber-roster-edit-group-from-jids account-at-point
					      jids-with-group
					      group-at-point))
      (call-interactively 'jabber-roster-change))))
;; jabber-roster-edit-action-at-point:1 ends here

;; [[file:jabber.org::#roster-roll-group][jabber-roster-roll-group:1]]
(defun jabber-roster-roll-group (jc group-name &optional set)
  "Roll up/down group in roster.
If optional SET is t, roll up group.
If SET is nor t or nil, roll down group."
  (let* ((state-data (fsm-get-state-data jc))
	 (roll-groups (plist-get state-data :roster-roll-groups))
         (new-roll-groups (if (cl-find group-name roll-groups :test 'string=)
                              ;; group is rolled up, roll it down if needed
                              (if (or (not set) (and set (not (eq set t))))
                                  (cl-remove-if-not (lambda (group-name-in-list)
                                                   (not (string= group-name
                                                                 group-name-in-list)))
                                                 roll-groups)
                                roll-groups)
                            ;; group is rolled down, roll it up if needed
                            (if (or (not set) (and set (eq set t)))
                                (append roll-groups (list group-name))
                              roll-groups))))
    (unless (equal roll-groups new-roll-groups)
      (plist-put
       state-data :roster-roll-groups
       new-roll-groups)
      (jabber-display-roster))))
;; jabber-roster-roll-group:1 ends here

;; [[file:jabber.org::#roster-mode][jabber-roster-mode:1]]
(defun jabber-roster-mode ()
  "Major mode for Jabber roster display.
Use the keybindings (mnemonic as Chat, Roster, Info, MUC, Service) to
bring up menus of actions.
\\{jabber-roster-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-roster-mode
	mode-name "jabber-roster")
  (use-local-map jabber-roster-mode-map)
  (setq buffer-read-only t)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-roster-mode-hook)
    (run-hooks 'jabber-roster-mode-hook)))
;; jabber-roster-mode:1 ends here

;; [[file:jabber.org::#roster-mode][jabber-roster-mode:2]]
(put 'jabber-roster-mode 'mode-class 'special)
;; jabber-roster-mode:2 ends here

;; [[file:jabber.org::#switch-to-roster-buffer][jabber-switch-to-roster-buffer:1]]
;;;###autoload
(defun jabber-switch-to-roster-buffer (&optional _jc)
  "Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'."
  (interactive)
  (if (not (get-buffer jabber-roster-buffer))
      (jabber-display-roster)
    (switch-to-buffer jabber-roster-buffer)))
;; jabber-switch-to-roster-buffer:1 ends here

;; [[file:jabber.org::#sort-roster][jabber-sort-roster:1]]
(defun jabber-sort-roster (jc)
  "Sort roster according to online status.
JC is the Jabber connection."
  (let ((state-data (fsm-get-state-data jc)))
    (dolist (group (plist-get state-data :roster-groups))
      (let ((group-name (car group)))
	(puthash group-name
		 (sort
		  (gethash group-name
			   (plist-get state-data :roster-hash))
		  #'jabber-roster-sort-items)
		 (plist-get state-data :roster-hash))))))
;; jabber-sort-roster:1 ends here

;; [[file:jabber.org::#roster-prepare-roster][jabber-roster-prepare-roster:1]]
(defun jabber-roster-prepare-roster (jc)
  "Make a hash based roster.
JC is the Jabber connection."
  (let* ((state-data (fsm-get-state-data jc))
	 (hash (make-hash-table :test 'equal))
	 (buddies (plist-get state-data :roster))
	 (all-groups '()))
    (dolist (buddy buddies)
      (let ((groups (get buddy 'groups)))
	(if groups
	    (progn
	      (dolist (group groups)
		(progn
		  (setq all-groups (append all-groups (list group)))
		  (puthash group
			   (append (gethash group hash)
				   (list buddy))
			   hash))))
	  (progn
	    (setq all-groups (append all-groups
				     (list jabber-roster-default-group-name)))
	    (puthash jabber-roster-default-group-name
		     (append (gethash jabber-roster-default-group-name hash)
			     (list buddy))
		     hash)))))

    ;; remove duplicates name of group
    (setq all-groups (sort
		      (cl-remove-duplicates all-groups
					 :test 'string=)
		      'string<))

    ;; put to state-data all-groups as list of list
    (plist-put state-data :roster-groups
	       (mapcar #'list all-groups))

    ;; put to state-data hash-roster
    (plist-put state-data :roster-hash
	       hash)))
;; jabber-roster-prepare-roster:1 ends here

;; [[file:jabber.org::#roster-sort-items][jabber-roster-sort-items:1]]
(defun jabber-roster-sort-items (a b)
  "Sort roster items A and B according to `jabber-roster-sort-functions'.
Return t if A is less than B."
  (let ((result nil))
    (seq-find (lambda (fn)
		(setq result (funcall fn a b))
		(not (= result 0)))
	      jabber-roster-sort-functions)
    (< result 0)))
;; jabber-roster-sort-items:1 ends here

;; [[file:jabber.org::#roster-sort-by-status][jabber-roster-sort-by-status:1]]
(defun jabber-roster-sort-by-status (a b)
  "Sort roster items by online status.
See `jabber-sort-order' for order used."
  (cl-flet ((order (item) (length (member (get item 'show) jabber-sort-order))))
    (let ((a-order (order a))
	  (b-order (order b)))
      ;; Note reversed test.  Items with longer X-order go first.
      (cond
       ((< a-order b-order)
	1)
       ((> a-order b-order)
	-1)
       (t
	0)))))
;; jabber-roster-sort-by-status:1 ends here

;; [[file:jabber.org::#roster-sort-by-displayname][jabber-roster-sort-by-displayname:1]]
(defun jabber-roster-sort-by-displayname (a b)
  "Sort roster items by displayed name."
  (let ((a-name (jabber-jid-displayname a))
	(b-name (jabber-jid-displayname b)))
    (cond
     ((string-lessp a-name b-name) -1)
     ((string= a-name b-name) 0)
     (t 1))))
;; jabber-roster-sort-by-displayname:1 ends here

;; [[file:jabber.org::#roster-sort-by-group][jabber-roster-sort-by-group:1]]
(defun jabber-roster-sort-by-group (a b)
  "Sort roster items by group membership."
  (cl-flet ((first-group (item) (or (car (get item 'groups)) "")))
    (let ((a-group (first-group a))
	  (b-group (first-group b)))
      (cond
       ((string-lessp a-group b-group) -1)
       ((string= a-group b-group) 0)
       (t 1)))))
;; jabber-roster-sort-by-group:1 ends here

;; [[file:jabber.org::#fix-status][jabber-fix-status:1]]
(defun jabber-fix-status (status)
  "Make status strings more readable."
  (when status
    (when (string-match "\n+$" status)
      (setq status (replace-match "" t t status)))
    (when jabber-remove-newlines
      (while (string-match "\n" status)
	(setq status (replace-match " " t t status))))
    status))
;; jabber-fix-status:1 ends here

;; [[file:jabber.org::#roster-ewoc][jabber-roster-ewoc:1]]
(defvar jabber-roster-ewoc nil
  "Ewoc displaying the roster.
There is only one; we don't rely on buffer-local variables or
such.")
;; jabber-roster-ewoc:1 ends here

;; [[file:jabber.org::#roster-filter-display][jabber-roster-filter-display:1]]
(defun jabber-roster-filter-display (buddies)
  "Filter BUDDIES for items to be displayed in the roster."
  (cl-remove-if-not (lambda (buddy) (or jabber-show-offline-contacts
				     (get buddy 'connected)))
		 buddies))
;; jabber-roster-filter-display:1 ends here

;; [[file:jabber.org::#roster-toggle-offline-display][jabber-roster-toggle-offline-display:1]]
(defun jabber-roster-toggle-offline-display ()
  "Toggle display of offline contacts.
To change this permanently, customize the `jabber-show-offline-contacts'."
  (interactive)
  (setq jabber-show-offline-contacts
	(not jabber-show-offline-contacts))
  (jabber-display-roster))
;; jabber-roster-toggle-offline-display:1 ends here

;; [[file:jabber.org::#roster-toggle-binding-display][jabber-roster-toggle-binding-display:1]]
(defun jabber-roster-toggle-binding-display ()
  "Toggle display of the roster binding text."
  (interactive)
  (setq jabber-roster-show-bindings
	(not jabber-roster-show-bindings))
  (jabber-display-roster))
;; jabber-roster-toggle-binding-display:1 ends here

;; [[file:jabber.org::#display-roster][jabber-display-roster:1]]
(defun jabber-display-roster ()
  "Switch to the main jabber buffer and refresh the roster display to reflect the current information."
  (interactive)
  (with-current-buffer (get-buffer-create jabber-roster-buffer)
    (if (not (eq major-mode 'jabber-roster-mode))
	(jabber-roster-mode))
    (setq buffer-read-only nil)
    ;; line-number-at-pos is in Emacs >= 21.4.  Only used to avoid
    ;; excessive scrolling when updating roster, so not absolutely
    ;; necessary.
    (let ((current-line (and (fboundp 'line-number-at-pos) (line-number-at-pos)))
	  (current-column (current-column)))
      (erase-buffer)
      (setq jabber-roster-ewoc nil)
      (when jabber-roster-show-title
	(insert (jabber-propertize "Jabber roster" 'face 'jabber-title-large) "\n"))
      (when jabber-roster-show-bindings
	(insert "RET      Open chat buffer        C-k      Delete roster item
e        Edit item               s        Send subscription request
q        Bury buffer             i        Get disco items
I        Get disco info          b        Browse
j        Join groupchat (MUC)    v        Get client version
a        Send presence           o        Show offline contacts on/off
C-c C-c  Chat menu               C-c C-m  Multi-User Chat menu
C-c C-i  Info menu               C-c C-r  Roster menu
C-c C-s  Service menu

H        Toggle displaying this text
"))
      (insert "__________________________________\n\n")
      (if (null jabber-connections)
	  (insert "Not connected\n")
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-2] #'jabber-send-presence)
	  (insert (jabber-propertize (concat (format " - %s"
						     (cdr (assoc *jabber-current-show* jabber-presence-strings)))
					     (if (not (zerop (length *jabber-current-status*)))
						 (format " (%s)"
							 (jabber-fix-status *jabber-current-status*)))
					     " -")
				     'face (or (cdr (assoc *jabber-current-show* jabber-presence-faces))
					       'jabber-roster-user-online)
				     ;;'mouse-face (cons 'background-color "light grey")
				     'keymap map)
		  "\n")))

      (dolist (jc jabber-connections)
	;; use a hash-based roster
	(when (not (plist-get (fsm-get-state-data jc) :roster-hash))
	  (jabber-roster-prepare-roster jc))
	;; We sort everything before putting it in the ewoc
	(jabber-sort-roster jc)
	(let ((before-ewoc (point))
	      (ewoc (ewoc-create
		       (let ((jc jc))
			 (lambda (data)
			   (let* ((group (car data))
				  (group-name (car group))
				  (buddy (car (cdr data))))
			     (jabber-display-roster-entry jc group-name buddy))))
		     (concat
		      (jabber-propertize (concat
					  (plist-get (fsm-get-state-data jc) :username)
					  "@"
					  (plist-get (fsm-get-state-data jc) :server))
					 'face 'jabber-title-medium)
		      "\n__________________________________\n")
		     "__________________________________"))
	      (new-groups '()))
	  (plist-put(fsm-get-state-data jc) :roster-ewoc ewoc)
	  (dolist (group (plist-get (fsm-get-state-data jc) :roster-groups))
	    (let* ((group-name (car group))
		   (buddies (jabber-roster-filter-display
			    (gethash group-name
				     (plist-get (fsm-get-state-data jc) :roster-hash)))))
	      (when (or jabber-roster-show-empty-group
			(> (length buddies) 0))
		(let ((group-node (ewoc-enter-last ewoc (list group nil))))
		  (if (not (cl-find
			    group-name
			    (plist-get (fsm-get-state-data jc) :roster-roll-groups)
			    :test 'string=))
		      (dolist (buddy (reverse buddies))
			(ewoc-enter-after ewoc group-node (list group buddy))))))))
	  (goto-char (point-max))
	  (insert "\n")
	  (put-text-property before-ewoc (point)
			     'jabber-account jc)))

      (goto-char (point-min))
      (setq buffer-read-only t)
      (if (called-interactively-p 'interactive)
	  (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
	    (run-hook-with-args hook 'roster (current-buffer) (funcall jabber-alert-info-message-function 'roster (current-buffer)))))
      (when current-line
	;; Go back to previous line - don't use goto-line, since it
	;; sets the mark.
	(goto-char (point-min))
	(forward-line (1- current-line))
	;; ...and go back to previous column
	(move-to-column current-column)))))
;; jabber-display-roster:1 ends here

;; [[file:jabber.org::#display-roster-entry][jabber-display-roster-entry:1]]
(defun jabber-display-roster-entry (jc group-name buddy)
  "Format and insert a roster entry for BUDDY at point.
BUDDY is a JID symbol.
JC is the Jabber connection."
  (if buddy
      (let ((buddy-str (format-spec
                        jabber-roster-line-format
                        (list
                         (cons ?a (jabber-propertize " " 'display (get buddy 'avatar)))
                         (cons ?c (if (get buddy 'connected) "*" " "))
                         (cons ?u (cdr (assoc
                                        (or
                                         (get buddy 'subscription) "none")
                                        jabber-roster-subscription-display)))
                         (cons ?n (if (> (length (get buddy 'name)) 0)
                                      (get buddy 'name)
                                    (symbol-name buddy)))
                         (cons ?j (symbol-name buddy))
                         (cons ?r (or (get buddy 'resource) ""))
                         (cons ?s (or (cdr (assoc (get buddy 'show)
					          jabber-presence-strings))
                                      (get buddy 'show)))
                         (cons ?S (if (get buddy 'status)
                                      (jabber-fix-status (get buddy 'status))
                                    ""))))))
	(add-text-properties 0
			     (length buddy-str)
			     (list
			      'face
			      (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
				  'jabber-roster-user-online)
			      ;;'mouse-face
			      ;;(cons 'background-color "light grey")
			      'help-echo
			      (symbol-name buddy)
			      'jabber-jid
			      (symbol-name buddy)
			      'jabber-account
			      jc)
			     buddy-str)
	(insert buddy-str)

	(when (or (eq jabber-show-resources 'always)
		  (and (eq jabber-show-resources 'sometimes)
		       (> (jabber-count-connected-resources buddy) 1)))
	  (dolist (resource (get buddy 'resources))
	    (when (plist-get (cdr resource) 'connected)
	      (let ((resource-str (format-spec jabber-resource-line-format
					       (list
						(cons ?c "*")
						(cons ?n (if (>
							      (length
							       (get buddy 'name)) 0)
							     (get buddy 'name)
							   (symbol-name buddy)))
						(cons ?j (symbol-name buddy))
						(cons ?r (if (>
							      (length
							       (car resource)) 0)
							     (car resource)
							   "empty"))
						(cons ?s (or
							  (cdr (assoc
								(plist-get
								 (cdr resource) 'show)
								jabber-presence-strings))
							  (plist-get
							   (cdr resource) 'show)))
						(cons ?S (if (plist-get
							      (cdr resource) 'status)
							     (jabber-fix-status
							      (plist-get (cdr resource)
									 'status))
							   ""))
						(cons ?p (number-to-string
							  (plist-get (cdr resource)
								     'priority)))))))
		(add-text-properties 0
				     (length resource-str)
				     (list
				      'face
				      (or (cdr (assoc (plist-get
						       (cdr resource)
						       'show)
						      jabber-presence-faces))
					  'jabber-roster-user-online)
				      'jabber-jid
				      (format "%s/%s" (symbol-name buddy) (car resource))
				      'jabber-account
				      jc)
				     resource-str)
		(insert "\n" resource-str))))))
    (let ((group-name (or group-name
			  jabber-roster-default-group-name)))
      (add-text-properties 0
			   (length group-name)
			   (list
			    'face 'jabber-title-small
			    'jabber-group group-name
			    'jabber-account jc)
			   group-name)
      (insert group-name))))
;; jabber-display-roster-entry:1 ends here

;; [[file:jabber.org::#roster-update][jabber-roster-update:1]]
;;;###autoload
(defun jabber-roster-update (jc new-items changed-items deleted-items)
  "Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.
JC is the Jabber connection."
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
	 (hash (plist-get (fsm-get-state-data jc) :roster-hash))
	 (ewoc (plist-get (fsm-get-state-data jc) :roster-ewoc))
	 (all-groups (plist-get (fsm-get-state-data jc) :roster-groups))
	 (terminator
	  (lambda (deleted-items)
	    (dolist (delete-this deleted-items)
	      (let ((groups (get delete-this 'groups))
		    (terminator
		     (lambda (g)
		       (let*
			   ((group (or g jabber-roster-default-group-name))
			    (buddies (gethash group hash)))
			 (when (not buddies)
			   (setq new-groups (append new-groups (list group))))
			 (puthash group
				  (delq delete-this buddies)
				  hash)))))
		(if groups
		    (dolist (group groups)
		      (terminator group))
		  (terminator groups)))))))

    ;; fix a old-roster
    (dolist (delete-this deleted-items)
      (setq roster (delq delete-this roster)))
    (setq roster (append new-items roster))
    (plist-put (fsm-get-state-data jc) :roster roster)

    ;; update a hash-roster
    (if (not hash)
	(jabber-roster-prepare-roster jc)

      (when jabber-roster-debug
	(message "update hash-based roster"))

      ;; delete items
      (dolist (delete-this (append deleted-items changed-items))
	(let ((jid (symbol-name delete-this)))
	  (when jabber-roster-debug
	    (message (concat "delete jid: " jid)))
	  (dolist (group (mapcar (lambda (g) (car g)) all-groups))
	    (when jabber-roster-debug
	      (message (concat "try to delete jid: " jid " from group " group)))
	    (puthash group
		     (delq delete-this (gethash group hash))
		     hash))))

      ;; insert changed-items
      (dolist (insert-this (append changed-items new-items))
	(let ((jid (symbol-name insert-this)))
	  (when jabber-roster-debug
	    (message (concat "insert jid: " jid)))
	  (dolist (group (or (get insert-this 'groups)
			     (list jabber-roster-default-group-name)))
	    (when jabber-roster-debug
	      (message (concat "insert jid: " jid " to group " group)))
	    (puthash group
		     (append (gethash group hash)
			     (list insert-this))
		     hash)
	    (setq all-groups (append all-groups (list (list group)))))))

      (when jabber-roster-debug
	(message "remove duplicates from new group"))
      (setq all-groups (sort
			(cl-remove-duplicates all-groups
					   :test (lambda (g1 g2)
						   (let ((g1-name (car g1))
							 (g2-name (car g2)))
						     (string= g1-name
							      g2-name))))
			(lambda (g1 g2)
			  (let ((g1-name (car g1))
				(g2-name (car g2)))
			    (string< g1-name
				     g2-name)))))

      (plist-put (fsm-get-state-data jc) :roster-groups all-groups))

    (when jabber-roster-debug
      (message "re display roster"))

    ;; recreate roster buffer
    (jabber-display-roster)))
;; jabber-roster-update:1 ends here

;; [[file:jabber.org::#roster-update][jabber-roster-update:2]]
(defalias 'jabber-presence-update-roster 'ignore)
;;jabber-presence-update-roster is not needed anymore.
;;Its work is done in `jabber-process-presence'."
(make-obsolete 'jabber-presence-update-roster 'ignore "27.2")
;; jabber-roster-update:2 ends here

;; [[file:jabber.org::#next-property][jabber-next-property:1]]
(defun jabber-next-property (&optional prev)
  "Return position of next property appearence or nil if there is none.
If optional PREV is non-nil, return position of previous property appearence."
  (let ((pos (point))
        (found nil)
        (nextprev (if prev 'previous-single-property-change
                    'next-single-property-change)))
    (while (not found)
      (setq pos
            (let ((jid (funcall nextprev pos 'jabber-jid))
                  (group (funcall nextprev pos 'jabber-group)))
              (cond
               ((not jid) group)
               ((not group) jid)
               (t (funcall (if prev 'max 'min) jid group)))))
      (if (not pos)
          (setq found t)
        (setq found (or (get-text-property pos 'jabber-jid)
                        (get-text-property pos 'jabber-group)))))
    pos))
;; jabber-next-property:1 ends here

;; [[file:jabber.org::#go-to-next-roster-item][jabber-go-to-next-roster-item:1]]
(defun jabber-go-to-next-roster-item ()
  "Move the cursor to the next jid/group in the buffer."
  (interactive)
  (let* ((next (jabber-next-property))
         (next (if (not next)
                   (progn (goto-char (point-min))
                          (jabber-next-property)) next)))
    (if next (goto-char next)
      (goto-char (point-min)))))
;; jabber-go-to-next-roster-item:1 ends here

;; [[file:jabber.org::#go-to-previous-roster-item][jabber-go-to-previous-roster-item:1]]
(defun jabber-go-to-previous-roster-item ()
  "Move the cursor to the previous jid/group in the buffer."
  (interactive)
  (let* ((previous (jabber-next-property 'prev))
         (previous (if (not previous)
                       (progn (goto-char (point-max))
                              (jabber-next-property 'prev)) previous)))
    (if previous (goto-char previous)
      (goto-char (point-max)))))
;; jabber-go-to-previous-roster-item:1 ends here

;; [[file:jabber.org::#roster-restore-groups][jabber-roster-restore-groups:1]]
(defun jabber-roster-restore-groups (jc)
  "Restore roster's groups rolling state from private storage.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-private-get jc 'roster "emacs-jabber"
                      'jabber-roster-restore-groups-1 'ignore))
;; jabber-roster-restore-groups:1 ends here

;; [[file:jabber.org::#roster-restore-groups-1][jabber-roster-restore-groups-1:1]]
(defun jabber-roster-restore-groups-1 (jc xml-data)
  "Parse roster groups and restore rolling state.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (when (string= (jabber-xml-get-xmlns xml-data) "emacs-jabber")
    (let* ((data (car (last xml-data)))
           (groups (if (stringp data) (split-string data "\n") nil)))
      (dolist (group groups)
        (jabber-roster-roll-group jc group t)))))
;; jabber-roster-restore-groups-1:1 ends here

;; [[file:jabber.org::#roster-save-groups][jabber-roster-save-groups:1]]
(defun jabber-roster-save-groups ()
  "Save roster's groups rolling state in private storage."
  (interactive)
  (dolist (jc jabber-connections)
    (let* ((groups (plist-get (fsm-get-state-data jc) :roster-roll-groups))
           (roll-groups
            (if groups
                (mapconcat (lambda (a) (substring-no-properties a)) groups "\n")
              "")))
      (jabber-private-set jc
                          `(roster ((xmlns . "emacs-jabber"))
                                   ,roll-groups)
                          'jabber-report-success "Roster groups saved"
                          'jabber-report-success "Failed to save roster groups"))))
;; jabber-roster-save-groups:1 ends here

;; [[file:jabber.org::#export-roster-widget][jabber-export-roster-widget:1]]
(defvar jabber-export-roster-widget nil)
;; jabber-export-roster-widget:1 ends here

;; [[file:jabber.org::#import-subscription-p-widget][jabber-import-subscription-p-widget:1]]
(defvar jabber-import-subscription-p-widget nil)
;; jabber-import-subscription-p-widget:1 ends here

;; [[file:jabber.org::#export-roster][jabber-export-roster:1]]
;;;###autoload
(defun jabber-export-roster (jc)
  "Export roster for connection JC."
  (interactive (list (jabber-read-account)))
  (let ((state-data (fsm-get-state-data jc)))
    (jabber-export-roster-do-it
     (jabber-roster-to-sexp (plist-get state-data :roster)))))
;; jabber-export-roster:1 ends here

;; [[file:jabber.org::#export-roster-do-it][jabber-export-roster-do-it:1]]
(defun jabber-export-roster-do-it (roster)
  "Create buffer from which ROSTER can be exported to a file."
  (interactive)
  (with-current-buffer (get-buffer-create "Export roster")
    (jabber-init-widget-buffer nil)

    (widget-insert (jabber-propertize "Export roster\n"
				      'face 'jabber-title-large))
    (widget-insert "You are about to save your roster to a file.  Here
you can edit it before saving.  Changes done here will
not affect your actual roster.

")

    (widget-create 'push-button :notify #'jabber-export-save "Save to file")
    (widget-insert " ")
    (widget-create 'push-button :notify #'jabber-export-remove-regexp "Remove by regexp")
    (widget-insert "\n\n")
    (make-local-variable 'jabber-export-roster-widget)

    (jabber-export-display roster)

    (widget-setup)
    (widget-minor-mode 1)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))
;; jabber-export-roster-do-it:1 ends here

;; [[file:jabber.org::#import-roster][jabber-import-roster:1]]
;;;###autoload
(defun jabber-import-roster (jc file)
  "Create buffer for roster import for connection JC from FILE."
  (interactive (list (jabber-read-account)
		     (read-file-name "Import roster from file: ")))
  (let ((roster
	 (with-temp-buffer
	   (let ((coding-system-for-read 'utf-8))
	     (jabber-roster-xml-to-sexp
	      (car (xml-parse-file file)))))))
    (with-current-buffer (get-buffer-create "Import roster")
      (setq jabber-buffer-connection jc)

      (jabber-init-widget-buffer nil)

      (widget-insert (jabber-propertize "Import roster\n"
					'face 'jabber-title-large))
      (widget-insert "You are about to import the contacts below to your roster.

")

      (make-local-variable 'jabber-import-subscription-p-widget)
      (setq jabber-import-subscription-p-widget
	    (widget-create 'checkbox))
      (widget-insert " Adjust subscriptions\n")

      (widget-create 'push-button :notify #'jabber-import-doit "Import to roster")
      (widget-insert " ")
      (widget-create 'push-button :notify #'jabber-export-remove-regexp "Remove by regexp")
      (widget-insert "\n\n")
      (make-local-variable 'jabber-export-roster-widget)

      (jabber-export-display roster)

      (widget-setup)
      (widget-minor-mode 1)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))
;; jabber-import-roster:1 ends here

;; [[file:jabber.org::#export-remove-regexp][jabber-export-remove-regexp:1]]
(defun jabber-export-remove-regexp (&rest ignore)
  (let* ((value (widget-value jabber-export-roster-widget))
	 (length-before (length value))
	 (regexp (read-string "Remove JIDs matching regexp: ")))
    (setq value (cl-delete-if
		 #'(lambda (a)
		     (string-match regexp (nth 0 a)))
		 value))
    (widget-value-set jabber-export-roster-widget value)
    (widget-setup)
    (message "%d items removed" (- length-before (length value)))))
;; jabber-export-remove-regexp:1 ends here

;; [[file:jabber.org::#export-save][jabber-export-save:1]]
(defun jabber-export-save (&rest ignore)
  "Export roster to file."
  (let ((items (mapcar #'jabber-roster-sexp-to-xml (widget-value jabber-export-roster-widget)))
	(coding-system-for-write 'utf-8))
    (with-temp-file (read-file-name "Export roster to file: ")
      (insert "<iq xmlns='jabber:client'><query xmlns='jabber:iq:roster'>\n")
      (dolist (item items)
	(insert (jabber-sexp2xml item) "\n"))
      (insert "</query></iq>\n"))
    (message "Roster saved")))
;; jabber-export-save:1 ends here

;; [[file:jabber.org::#import-doit][jabber-import-doit:1]]
(defun jabber-import-doit (&rest ignore)
  "Import roster being edited in widget."
  (let* ((state-data (fsm-get-state-data jabber-buffer-connection))
	 (jabber-roster (plist-get state-data :roster))
	 roster-delta)

    (dolist (n (widget-value jabber-export-roster-widget))
      (let* ((jid (nth 0 n))
	     (name (and (not (zerop (length (nth 1 n))))
			(nth 1 n)))
	     (subscription (nth 2 n))
	     (groups (nth 3 n))
	     (jid-symbol (jabber-jid-symbol jid))
	     (in-roster-p (memq jid-symbol jabber-roster))
	     (jid-name (and in-roster-p (get jid-symbol 'name)))
	     (jid-subscription (and in-roster-p (get jid-symbol 'subscription)))
	     (jid-groups (and in-roster-p (get jid-symbol 'groups))))
	;; Do we need to change the roster?
	(when (or
	       ;; If the contact is not in the roster already,
	       (not in-roster-p)
	       ;; or if the import introduces a name,
	       (and name (not jid-name))
	       ;; or changes a name,
	       (and name jid-name (not (string= name jid-name)))
	       ;; or introduces new groups.
	       (cl-set-difference groups jid-groups :test #'string=))
	  (push (jabber-roster-sexp-to-xml
		 (list jid (or name jid-name) nil (cl-union groups jid-groups :test #'string=))
		 t)
		roster-delta))
	;; And adujst subscription.
	(when (widget-value jabber-import-subscription-p-widget)
	  (let ((want-to (member subscription '("to" "both")))
		(want-from (member subscription '("from" "both")))
		(have-to (member jid-subscription '("to" "both")))
		(have-from (member jid-subscription '("from" "both"))))
	    (cl-flet ((request-subscription
		    (type)
		    (jabber-send-sexp jabber-buffer-connection
				      `(presence ((to . ,jid)
						  (type . ,type))))))
	      (cond
	       ((and want-to (not have-to))
		(request-subscription "subscribe"))
	       ((and have-to (not want-to))
		(request-subscription "unsubscribe")))
	      (cond
	       ((and want-from (not have-from))
		;; not much to do here
		)
	       ((and have-from (not want-from))
		(request-subscription "unsubscribed"))))))))
    (when roster-delta
      (jabber-send-iq jabber-buffer-connection
		      nil "set"
		      `(query ((xmlns . "jabber:iq:roster")) ,@roster-delta)
		      #'jabber-report-success "Roster import"
		      #'jabber-report-success "Roster import"))))
;; jabber-import-doit:1 ends here

;; [[file:jabber.org::#roster-to-sexp][jabber-roster-to-sexp:1]]
(defun jabber-roster-to-sexp (roster)
  "Convert ROSTER to simpler sexp format.
Return a list, where each item is a vector:
\[jid name subscription groups]
where groups is a list of strings."
  (mapcar
   #'(lambda (n)
       (list
	(symbol-name n)
	(or (get n 'name) "")
	(get n 'subscription)
	(get n 'groups)))
   roster))
;; jabber-roster-to-sexp:1 ends here

;; [[file:jabber.org::#roster-sexp-to-xml][jabber-roster-sexp-to-xml:1]]
(defun jabber-roster-sexp-to-xml (sexp &optional omit-subscription)
  "Convert SEXP to XML format.
Return an XML node."
  `(item ((jid . ,(nth 0 sexp))
	  ,@(let ((name (nth 1 sexp)))
	      (unless (zerop (length name))
		`((name . ,name))))
	  ,@(unless omit-subscription
	      `((subscription . ,(nth 2 sexp)))))
	 ,@(mapcar
	    #'(lambda (g)
		(list 'group nil g))
	    (nth 3 sexp))))
;; jabber-roster-sexp-to-xml:1 ends here

;; [[file:jabber.org::#roster-xml-to-sexp][jabber-roster-xml-to-sexp:1]]
(defun jabber-roster-xml-to-sexp (xml-data)
  "Convert XML-DATA to simpler sexp format.
XML-DATA is an <iq> node with a <query xmlns='jabber:iq:roster'> child.
See `jabber-roster-to-sexp' for description of output format."
  (cl-assert (eq (jabber-xml-node-name xml-data) 'iq))
  (let ((query (car (jabber-xml-get-children xml-data 'query))))
    (cl-assert query)
    (mapcar
     #'(lambda (n)
	 (list
	  (jabber-xml-get-attribute n 'jid)
	  (or (jabber-xml-get-attribute n 'name) "")
	  (jabber-xml-get-attribute n 'subscription)
	  (mapcar
	   #'(lambda (g)
	       (car (jabber-xml-node-children g)))
	   (jabber-xml-get-children n 'group))))
     (jabber-xml-get-children query 'item))))
;; jabber-roster-xml-to-sexp:1 ends here

;; [[file:jabber.org::#export-display][jabber-export-display:1]]
(defun jabber-export-display (roster)
  (setq jabber-export-roster-widget
	(widget-create
	 '(repeat
	   :tag "Roster"
	   (list :format "%v"
		   (string :tag "JID")
		   (string :tag "Name")
		   (choice :tag "Subscription"
			   (const "none")
			   (const "both")
			   (const "to")
			   (const "from"))
		   (repeat :tag "Groups"
			   (string :tag "Group"))))
	 :value roster)))
;; jabber-export-display:1 ends here

;; [[file:jabber.org::#*jabber-open-info-queries*][*jabber-open-info-queries*:1]]
(defvar *jabber-open-info-queries* nil
  "An alist of open query id and their callback functions.")
;; *jabber-open-info-queries*:1 ends here

;; [[file:jabber.org::#iq-get-xmlns-alist][jabber-iq-get-xmlns-alist:1]]
(defvar jabber-iq-get-xmlns-alist nil
  "Mapping from XML namespace to handler for IQ GET requests.")
;; jabber-iq-get-xmlns-alist:1 ends here

;; [[file:jabber.org::#iq-set-xmlns-alist][jabber-iq-set-xmlns-alist:1]]
(defvar jabber-iq-set-xmlns-alist nil
  "Mapping from XML namespace to handler for IQ SET requests.")
;; jabber-iq-set-xmlns-alist:1 ends here

;; [[file:jabber.org::#browse-mode-map][jabber-browse-mode-map:1]]
(defvar jabber-browse-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map [mouse-2] 'jabber-popup-combined-menu)
    map))
;; jabber-browse-mode-map:1 ends here

;; [[file:jabber.org::#browse-mode-hook][jabber-browse-mode-hook:1]]
(defcustom jabber-browse-mode-hook nil
  "Hook run when entering Browse mode."
  :group 'jabber
  :type 'hook)
;; jabber-browse-mode-hook:1 ends here

;; [[file:jabber.org::#browse][jabber-browse:1]]
(defgroup jabber-browse nil "browse display options"
  :group 'jabber)
;; jabber-browse:1 ends here

;; [[file:jabber.org::#browse-buffer-format][jabber-browse-buffer-format:1]]
(defcustom jabber-browse-buffer-format "*-jabber-browse:-%n-*"
  "The format specification for the name of browse buffers.

These fields are available at this moment:

%n   JID to browse"
  :type 'string
  :group 'jabber-browse)
;; jabber-browse-buffer-format:1 ends here

;; [[file:jabber.org::#browse-mode][jabber-browse-mode:1]]
(defun jabber-browse-mode ()
"Jabber browse mode.
\\{jabber-browse-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-browse-mode
        mode-name "jabber-browse")
  (use-local-map jabber-browse-mode-map)
  (setq buffer-read-only t)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-browse-mode-hook)
    (run-hooks 'jabber-browse-mode-hook)))
;; jabber-browse-mode:1 ends here

;; [[file:jabber.org::#browse-mode][jabber-browse-mode:2]]
(put 'jabber-browse-mode 'mode-class 'special)
;; jabber-browse-mode:2 ends here

;; [[file:jabber.org::#process-iq][jabber-process-iq:1]]
(add-to-list 'jabber-iq-chain 'jabber-process-iq)
(defun jabber-process-iq (jc xml-data)
  "Process an incoming iq stanza.

JC is the Jabber Connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((id (jabber-xml-get-attribute xml-data 'id))
         (type (jabber-xml-get-attribute xml-data 'type))
         (from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
         (callback (assoc id *jabber-open-info-queries*)))
    (cond
     ;; if type is "result" or "error", this is a response to a query we sent.
     ((or (string= type "result")
	  (string= type "error"))
      (let ((callback-cons (nth (cdr (assoc type '(("result" . 0)
						   ("error" . 1)))) (cdr callback))))
	(if (consp callback-cons)
	    (funcall (car callback-cons) jc xml-data (cdr callback-cons))))
      (setq *jabber-open-info-queries* (delq callback *jabber-open-info-queries*)))

     ;; if type is "get" or "set", correct action depends on namespace of request.
     ((and (listp query)
	   (or (string= type "get")
	       (string= type "set")))
      (let* ((which-alist (eval (cdr (assoc type
					    (list
					     (cons "get" 'jabber-iq-get-xmlns-alist)
					     (cons "set" 'jabber-iq-set-xmlns-alist))))))
	     (handler (cdr (assoc (jabber-xml-get-attribute query 'xmlns) which-alist))))
	(if handler
	    (condition-case error-var
		(funcall handler jc xml-data)
	      (jabber-error
	       (apply 'jabber-send-iq-error jc from id query (cdr error-var)))
	      (error (jabber-send-iq-error jc from id query "wait" 'internal-server-error (error-message-string error-var))))
	  (jabber-send-iq-error jc from id query "cancel" 'feature-not-implemented)))))))
;; jabber-process-iq:1 ends here

;; [[file:jabber.org::#send-iq][jabber-send-iq:1]]
(defun jabber-send-iq (jc to type query success-callback success-closure-data
			  error-callback error-closure-data &optional result-id)
  "Send an iq stanza to the specified entity, and optionally set up a callback.
JC is the Jabber connection.
TO is the addressee.
TYPE is one of \"get\", \"set\", \"result\" or \"error\".
QUERY is a list containing the child of the iq node in the format
`jabber-sexp2xml' accepts.
SUCCESS-CALLBACK is the function to be called when a successful result arrives.
SUCCESS-CLOSURE-DATA is an extra argument to SUCCESS-CALLBACK.
ERROR-CALLBACK is the function to be called when an error arrives.
ERROR-CLOSURE-DATA is an extra argument to ERROR-CALLBACK.
RESULT-ID is the id to be used for a response to a received iq message.
`jabber-report-success' and `jabber-process-data' are common callbacks.

The callback functions are called like this:
\(funcall CALLBACK JC XML-DATA CLOSURE-DATA)
with XML-DATA being the IQ stanza received in response."
  (let ((id (or result-id (apply 'format "emacs-iq-%d.%d.%d" (current-time)))))
    (if (or success-callback error-callback)
	(setq *jabber-open-info-queries* (cons (list id
						     (cons success-callback success-closure-data)
						     (cons error-callback error-closure-data))

					       *jabber-open-info-queries*)))
    (jabber-send-sexp jc
		      (list 'iq (append
				 (if to (list (cons 'to to)))
				 (list (cons 'type type))
				 (list (cons 'id id)))
			    query))))
;; jabber-send-iq:1 ends here

;; [[file:jabber.org::#send-iq-error][jabber-send-iq-error:1]]
(defun jabber-send-iq-error (jc to id original-query error-type condition
				&optional text app-specific)
  "Send an error iq stanza in response to a previously sent iq stanza.
Send an error iq stanza to the specified entity in response to a
previously sent iq stanza.
TO is the addressee.
ID is the id of the iq stanza that caused the error.
ORIGINAL-QUERY is the original query, which should be included in the
error, or nil.
ERROR-TYPE is one of \"cancel\", \"continue\", \"modify\", \"auth\"
and \"wait\".
CONDITION is a symbol denoting a defined XMPP condition.
TEXT is a string to be sent in the error message, or nil for no text.
APP-SPECIFIC is a list of extra XML tags.
JC is the Jabber connection.

See section 9.3 of XMPP Core."
  (jabber-send-sexp
   jc
   `(iq (,@(when to `((to . ,to)))
	 (type . "error")
	 (id . ,(or id "")))
	,original-query
	(error ((type . ,error-type))
	       (,condition ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))
	       ,(if text
		    `(text ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))
			   ,text))
	       ,@app-specific))))
;; jabber-send-iq-error:1 ends here

;; [[file:jabber.org::#process-data][jabber-process-data:1]]
(defun jabber-process-data (jc xml-data closure-data)
  "Process random results from various requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((from (or (jabber-xml-get-attribute xml-data 'from) (plist-get (fsm-get-state-data jc) :server)))
	(xmlns (jabber-iq-xmlns xml-data))
	(type (jabber-xml-get-attribute xml-data 'type)))
    (with-current-buffer (get-buffer-create (format-spec jabber-browse-buffer-format
                                                         (list (cons ?n from))))
      (if (not (eq major-mode 'jabber-browse-mode))
	  (jabber-browse-mode))

      (setq buffer-read-only nil)
      (goto-char (point-max))

      (insert (jabber-propertize from
			  'face 'jabber-title-large) "\n\n")

      ;; Put point at beginning of data
      (save-excursion
	;; If closure-data is a function, call it.  If it is a string,
	;; output it along with a description of the error.  For other
	;; values (e.g. nil), just dump the XML.
	(cond
	 ((functionp closure-data)
	  (funcall closure-data jc xml-data))
	 ((stringp closure-data)
	  (insert closure-data ": " (jabber-parse-error (jabber-iq-error xml-data)) "\n\n"))
	 (t
	  (insert (format "%S\n\n" xml-data))))

	(dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
	  (run-hook-with-args hook 'browse (current-buffer) (funcall jabber-alert-info-message-function 'browse (current-buffer))))))))
;; jabber-process-data:1 ends here

;; [[file:jabber.org::#silent-process-data][jabber-silent-process-data:1]]
(defun jabber-silent-process-data (jc xml-data closure-data)
  "Process random results from various requests to only alert hooks.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((text (cond
               ((functionp closure-data)
                (funcall closure-data jc xml-data))
               ((stringp closure-data)
                (concat closure-data ": " (jabber-parse-error (jabber-iq-error xml-data))))
               (t
                (format "%S" xml-data)))))
    (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
      (run-hook-with-args hook 'browse (current-buffer)
                          text))))
;; jabber-silent-process-data:1 ends here

;; [[file:jabber.org::#alerts][jabber-alerts:1]]
(defgroup jabber-alerts nil "auditory and visual alerts for jabber events"
  :group 'jabber)
;; jabber-alerts:1 ends here

;; [[file:jabber.org::#alert-message-hooks][jabber-alert-message-hooks:1]]
(defcustom jabber-alert-message-hooks '(jabber-message-echo
					jabber-message-scroll)
  "Hooks run when a new message arrives.

Arguments are FROM, BUFFER, TEXT and TITLE.  FROM is the JID of
the sender, BUFFER is the the buffer where the message can be
read, and TEXT is the text of the message.  TITLE is the string
returned by `jabber-alert-message-function' for these arguments,
so that hooks do not have to call it themselves.

This hook is meant for user customization of message alerts.  For
other uses, see `jabber-message-hooks'."
  :type 'hook
  :options '(jabber-message-beep
	     jabber-message-wave
	     jabber-message-echo
	     jabber-message-switch
	     jabber-message-display
	     jabber-message-scroll)
  :group 'jabber-alerts)
;; jabber-alert-message-hooks:1 ends here

;; [[file:jabber.org::#message-hooks][jabber-message-hooks:1]]
(defvar jabber-message-hooks nil
  "Internal hooks run when a new message arrives.

This hook works just like `jabber-alert-message-hooks', except that
it's not meant to be customized by the user.")
;; jabber-message-hooks:1 ends here

;; [[file:jabber.org::#alert-message-function][jabber-alert-message-function:1]]
(defcustom jabber-alert-message-function
  'jabber-message-default-message
  "Function for constructing short message alert messages.

Arguments are FROM, BUFFER, and TEXT.  This function should return a
string containing an appropriate text message, or nil if no message
should be displayed.

The provided hooks displaying a text message get it from this function,
and show no message if it returns nil.  Other hooks do what they do
every time."
  :type 'function
  :group 'jabber-alerts)
;; jabber-alert-message-function:1 ends here

;; [[file:jabber.org::#alert-muc-hooks][jabber-alert-muc-hooks:1]]
(defcustom jabber-alert-muc-hooks '(jabber-muc-echo jabber-muc-scroll)
  "Hooks run when a new MUC message arrives.

Arguments are NICK, GROUP, BUFFER, TEXT and TITLE.  NICK is the
nickname of the sender.  GROUP is the JID of the group.  BUFFER
is the the buffer where the message can be read, and TEXT is the
text of the message.  TITLE is the string returned by
`jabber-alert-muc-function' for these arguments, so that hooks do
not have to call it themselves."
  :type 'hook
  :options '(jabber-muc-beep
	     jabber-muc-wave
	     jabber-muc-echo
	     jabber-muc-switch
	     jabber-muc-display
	     jabber-muc-scroll)
  :group 'jabber-alerts)
;; jabber-alert-muc-hooks:1 ends here

;; [[file:jabber.org::#muc-hooks][jabber-muc-hooks:1]]
(defvar jabber-muc-hooks '()
  "Internal hooks run when a new MUC message arrives.

This hook works just like `jabber-alert-muc-hooks', except that
it's not meant to be customized by the user.")
;; jabber-muc-hooks:1 ends here

;; [[file:jabber.org::#alert-muc-function][jabber-alert-muc-function:1]]
(defcustom jabber-alert-muc-function
  'jabber-muc-default-message
  "Function for constructing short message alert messages.

Arguments are NICK, GROUP, BUFFER, and TEXT.  This function
should return a string containing an appropriate text message, or
nil if no message should be displayed.

The provided hooks displaying a text message get it from this function,
and show no message if it returns nil.  Other hooks do what they do
every time."
  :type 'function
  :group 'jabber-alerts)
;; jabber-alert-muc-function:1 ends here

;; [[file:jabber.org::#alert-presence-hooks][jabber-alert-presence-hooks:1]]
(defcustom jabber-alert-presence-hooks
  '(jabber-presence-echo)
  "Hooks run when a user's presence changes.

Arguments are WHO, OLDSTATUS, NEWSTATUS, STATUSTEXT and
PROPOSED-ALERT.  WHO is a symbol whose text is the JID of the contact,
and which has various interesting properties.  OLDSTATUS is the old
presence or nil if disconnected.  NEWSTATUS is the new presence, or
one of \"subscribe\", \"unsubscribe\", \"subscribed\" and
\"unsubscribed\".  TITLE is the string returned by
`jabber-alert-presence-message-function' for these arguments."
  :type 'hook
  :options '(jabber-presence-beep
	     jabber-presence-wave
	     jabber-presence-switch
	     jabber-presence-display
	     jabber-presence-echo)
  :group 'jabber-alerts)
;; jabber-alert-presence-hooks:1 ends here

;; [[file:jabber.org::#presence-hooks][jabber-presence-hooks:1]]
(defvar jabber-presence-hooks '(jabber-presence-watch)
  "Internal hooks run when a user's presence changes.

This hook works just like `jabber-alert-presence-hooks', except that
it's not meant to be customized by the user.")
;; jabber-presence-hooks:1 ends here

;; [[file:jabber.org::#alert-presence-message-function][jabber-alert-presence-message-function:1]]
(defcustom jabber-alert-presence-message-function
  'jabber-presence-default-message
  "Function for constructing title of presence alert messages.

Arguments are WHO, OLDSTATUS, NEWSTATUS and STATUSTEXT.  See
`jabber-alert-presence-hooks' for documentation.  This function
should return a string containing an appropriate text message, or nil
if no message should be displayed.

The provided hooks displaying a text message get it from this function.
All hooks refrain from action if this function returns nil."
  :type 'function
  :group 'jabber-alerts)
;; jabber-alert-presence-message-function:1 ends here

;; [[file:jabber.org::#alert-info-message-hooks][jabber-alert-info-message-hooks:1]]
(defcustom jabber-alert-info-message-hooks '(jabber-info-display jabber-info-echo)
  "Hooks run when an info request is completed.

First argument is WHAT, a symbol telling the kind of info request completed.
That might be 'roster, for requested roster updates, and 'browse, for
browse requests.  Second argument in BUFFER, a buffer containing the result.
Third argument is PROPOSED-ALERT, containing the string returned by
`jabber-alert-info-message-function' for these arguments."
  :type 'hook
  :options '(jabber-info-beep
	     jabber-info-wave
	     jabber-info-echo
	     jabber-info-switch
	     jabber-info-display)
  :group 'jabber-alerts)
;; jabber-alert-info-message-hooks:1 ends here

;; [[file:jabber.org::#info-message-hooks][jabber-info-message-hooks:1]]
(defvar jabber-info-message-hooks '()
  "Internal hooks run when an info request is completed.

This hook works just like `jabber-alert-info-message-hooks',
except that it's not meant to be customized by the user.")
;; jabber-info-message-hooks:1 ends here

;; [[file:jabber.org::#alert-info-message-function][jabber-alert-info-message-function:1]]
(defcustom jabber-alert-info-message-function
  'jabber-info-default-message
  "Function for constructing info alert messages.

Arguments are WHAT, a symbol telling the kind of info request completed,
and BUFFER, a buffer containing the result."
  :type 'function
  :group 'jabber-alerts)
;; jabber-alert-info-message-function:1 ends here

;; [[file:jabber.org::#info-message-alist][jabber-info-message-alist:1]]
(defcustom jabber-info-message-alist
  '((roster . "Roster display updated")
    (browse . "Browse request completed"))
  "Alist for info alert messages, used by `jabber-info-default-message'."
  :type '(alist :key-type symbol :value-type string
		:options (roster browse))
  :group 'jabber-alerts)
;; jabber-info-message-alist:1 ends here

;; [[file:jabber.org::#alert-message-wave][jabber-alert-message-wave:1]]
(defcustom jabber-alert-message-wave ""
  "A sound file to play when a message arrived.
See `jabber-alert-message-wave-alist' if you want other sounds
for specific contacts."
  :type 'file
  :group 'jabber-alerts)
;; jabber-alert-message-wave:1 ends here

;; [[file:jabber.org::#alert-message-wave-alist][jabber-alert-message-wave-alist:1]]
(defcustom jabber-alert-message-wave-alist nil
  "Specific sound files for messages from specific contacts.
The keys are regexps matching the JID, and the values are sound
files."
  :type '(alist :key-type regexp :value-type file)
  :group 'jabber-alerts)
;; jabber-alert-message-wave-alist:1 ends here

;; [[file:jabber.org::#alert-muc-wave][jabber-alert-muc-wave:1]]
(defcustom jabber-alert-muc-wave ""
  "A sound file to play when a MUC message arrived."
  :type 'file
  :group 'jabber-alerts)
;; jabber-alert-muc-wave:1 ends here

;; [[file:jabber.org::#alert-presence-wave][jabber-alert-presence-wave:1]]
(defcustom jabber-alert-presence-wave ""
  "A sound file to play when a presence arrived."
  :type 'file
  :group 'jabber-alerts)
;; jabber-alert-presence-wave:1 ends here

;; [[file:jabber.org::#alert-presence-wave-alist][jabber-alert-presence-wave-alist:1]]
(defcustom jabber-alert-presence-wave-alist nil
  "Specific sound files for presence from specific contacts.
The keys are regexps matching the JID, and the values are sound
files."
  :type '(alist :key-type regexp :value-type file)
  :group 'jabber-alerts)
;; jabber-alert-presence-wave-alist:1 ends here

;; [[file:jabber.org::#alert-info-wave][jabber-alert-info-wave:1]]
(defcustom jabber-alert-info-wave ""
  "A sound file to play when an info query result arrived."
  :type 'file
  :group 'jabber-alerts)
;; jabber-alert-info-wave:1 ends here

;; [[file:jabber.org::#play-sound-file][jabber-play-sound-file:1]]
(defcustom jabber-play-sound-file 'play-sound-file
  "A function to call to play alert sound files."
  :type 'function
  :group 'jabber-alerts)
;; jabber-play-sound-file:1 ends here

;; [[file:jabber.org::#define-jabber-alert][define-jabber-alert:1]]
(defmacro define-jabber-alert (name docstring function)
  "Define a new family of external alert hooks.
Use this macro when your hooks do nothing except displaying a string
in some new innovative way.  You write a string display function, and
this macro does all the boring and repetitive work.

NAME is the name of the alert family.  The resulting hooks will be
called jabber-{message,muc,presence,info}-NAME.
DOCSTRING is the docstring to use for those hooks.
FUNCTION is a function that takes one argument, a string,
and displays it in some meaningful way.  It can be either a
lambda form or a quoted function name.
The created functions are inserted as options in Customize.

Examples:
\(define-jabber-alert foo \"Send foo alert\" 'foo-message)
\(define-jabber-alert bar \"Send bar alert\"
  (lambda (msg) (bar msg 42)))"
  (let ((sn (symbol-name name)))
    (let ((msg (intern (format "jabber-message-%s" sn)))
	  (muc (intern (format "jabber-muc-%s" sn)))
	  (pres (intern (format "jabber-presence-%s" sn)))
	  (info (intern (format "jabber-info-%s" sn))))
      `(progn
	 (defun ,msg (from buffer text title)
	   ,docstring
	   (when title
	     (funcall ,function text title)))
	 (cl-pushnew (quote ,msg) (get 'jabber-alert-message-hooks 'custom-options))
	 (defun ,muc (nick group buffer text title)
	   ,docstring
	   (when title
	     (funcall ,function text title)))
	 (cl-pushnew (quote ,muc) (get 'jabber-alert-muc-hooks 'custom-options))
	 (defun ,pres (who oldstatus newstatus statustext title)
	   ,docstring
	   (when title
	     (funcall ,function statustext title)))
	 (cl-pushnew (quote ,pres) (get 'jabber-alert-presence-hooks 'custom-options))
	 (defun ,info (infotype buffer text)
	   ,docstring
	   (when text
	     (funcall ,function text)))
	 (cl-pushnew (quote ,info) (get 'jabber-alert-info-message-hooks 'custom-options))))))
;; define-jabber-alert:1 ends here

;; [[file:jabber.org::#define-jabber-alert][define-jabber-alert:2]]
;; Alert hooks
(define-jabber-alert echo "Show a message in the echo area"
  (lambda (text &optional title) (message "%s" (or title text))))
(define-jabber-alert beep "Beep on event"
  (lambda (&rest ignore) (beep)))
;; define-jabber-alert:2 ends here

;; [[file:jabber.org::#message-default-message][jabber-message-default-message:1]]
;; Message alert hooks
(defun jabber-message-default-message (from buffer text)
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if (jabber-muc-sender-p from)
	(format "Private message from %s in %s"
		(jabber-jid-resource from)
		(jabber-jid-displayname (jabber-jid-user from)))
      (format "Message from %s" (jabber-jid-displayname from)))))
;; jabber-message-default-message:1 ends here

;; [[file:jabber.org::#message-alert-same-buffer][jabber-message-alert-same-buffer:1]]
(defcustom jabber-message-alert-same-buffer t
  "If nil, don't display message alerts for the current buffer."
  :type 'boolean
  :group 'jabber-alerts)
;; jabber-message-alert-same-buffer:1 ends here

;; [[file:jabber.org::#muc-alert-self][jabber-muc-alert-self:1]]
(defcustom jabber-muc-alert-self nil
  "If nil, don't display MUC alerts for your own messages."
  :type 'boolean
  :group 'jabber-alerts)
;; jabber-muc-alert-self:1 ends here

;; [[file:jabber.org::#message-wave][jabber-message-wave:1]]
(defun jabber-message-wave (from buffer text title)
  "Play the wave file specified in `jabber-alert-message-wave'."
  (when title
    (let* ((case-fold-search t)
	   (bare-jid (jabber-jid-user from))
	   (sound-file (or (dolist (entry jabber-alert-message-wave-alist)
			     (when (string-match (car entry) bare-jid)
			       (cl-return (cdr entry))))
			   jabber-alert-message-wave)))
      (unless (equal sound-file "")
	(funcall jabber-play-sound-file sound-file)))))
;; jabber-message-wave:1 ends here

;; [[file:jabber.org::#message-display][jabber-message-display:1]]
(defun jabber-message-display (from buffer text title)
  "Display the buffer where a new message has arrived."
  (when title
    (display-buffer buffer)))
;; jabber-message-display:1 ends here

;; [[file:jabber.org::#message-switch][jabber-message-switch:1]]
(defun jabber-message-switch (from buffer text title)
  "Switch to the buffer where a new message has arrived."
  (when title
    (switch-to-buffer buffer)))
;; jabber-message-switch:1 ends here

;; [[file:jabber.org::#message-scroll][jabber-message-scroll:1]]
(defun jabber-message-scroll (from buffer text title)
  "Scroll all nonselected windows where the chat buffer is displayed."
  ;; jabber-chat-buffer-display will DTRT with point in the buffer.
  ;; But this change will not take effect in nonselected windows.
  ;; Therefore we do that manually here.
  ;;
  ;; There are three cases:
  ;; 1. The user started typing a message in this window.  Point is
  ;;    greater than jabber-point-insert.  In that case, we don't
  ;;    want to move point.
  ;; 2. Point was at the end of the buffer, but no message was being
  ;;    typed.  After displaying the message, point is now close to
  ;;    the end of the buffer.  We advance it to the end.
  ;; 3. The user was perusing history in this window.  There is no
  ;;    simple way to distinguish this from 2, so the user loses.
  (let ((windows (get-buffer-window-list buffer nil t))
	(new-point-max (with-current-buffer buffer (point-max))))
    (dolist (w windows)
      (unless (eq w (selected-window))
	(set-window-point w new-point-max)))))
;; jabber-message-scroll:1 ends here

;; [[file:jabber.org::#muc-default-message][jabber-muc-default-message:1]]
;; MUC alert hooks
(defun jabber-muc-default-message (nick group buffer text)
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if nick
	(when (or jabber-muc-alert-self
		  (not (string= nick (cdr (assoc group *jabber-active-groupchats*)))))
	  (format "Message from %s in %s" nick (jabber-jid-displayname
						group)))
      (format "Message in %s" (jabber-jid-displayname group)))))
;; jabber-muc-default-message:1 ends here

;; [[file:jabber.org::#muc-wave][jabber-muc-wave:1]]
(defun jabber-muc-wave (nick group buffer text title)
  "Play the wave file specified in `jabber-alert-muc-wave'."
  (when title
    (funcall jabber-play-sound-file jabber-alert-muc-wave)))
;; jabber-muc-wave:1 ends here

;; [[file:jabber.org::#muc-display][jabber-muc-display:1]]
(defun jabber-muc-display (nick group buffer text title)
  "Display the buffer where a new message has arrived."
  (when title
    (display-buffer buffer)))
;; jabber-muc-display:1 ends here

;; [[file:jabber.org::#muc-switch][jabber-muc-switch:1]]
(defun jabber-muc-switch (nick group buffer text title)
  "Switch to the buffer where a new message has arrived."
  (when title
    (switch-to-buffer buffer)))
;; jabber-muc-switch:1 ends here

;; [[file:jabber.org::#muc-scroll][jabber-muc-scroll:1]]
(defun jabber-muc-scroll (nick group buffer text title)
  "Scroll buffer even if it is in an unselected window."
  (jabber-message-scroll nil buffer nil nil))
;; jabber-muc-scroll:1 ends here

;; [[file:jabber.org::#presence-default-message][jabber-presence-default-message:1]]
;; Presence alert hooks
(defun jabber-presence-default-message (who oldstatus newstatus statustext)
  "Return a string with the status change if OLDSTATUS and NEWSTATUS differs.

Return nil if OLDSTATUS and NEWSTATUS are equal, and in other
cases a string of the form \"'name' (jid) is now NEWSTATUS (STATUSTEXT)\".

This function is not called directly, but is the default for
`jabber-alert-presence-message-function'."
  (cond
   ((equal oldstatus newstatus)
      nil)
   (t
    (let ((formattedname
	   (if (> (length (get who 'name)) 0)
	       (get who 'name)
	     (symbol-name who)))
	  (formattedstatus
	   (or
	    (cdr (assoc newstatus
			'(("subscribe" . " requests subscription to your presence")
			  ("subscribed" . " has granted presence subscription to you")
			  ("unsubscribe" . " no longer subscribes to your presence")
			  ("unsubscribed" . " cancels your presence subscription"))))
	    (concat " is now "
		    (or
		     (cdr (assoc newstatus jabber-presence-strings))
		     newstatus)))))
      (concat formattedname formattedstatus)))))
;; jabber-presence-default-message:1 ends here

;; [[file:jabber.org::#presence-only-chat-open-message][jabber-presence-only-chat-open-message:1]]
(defun jabber-presence-only-chat-open-message (who oldstatus newstatus statustext)
  "Same as `jabber-presence-default-message' but managing the presence messages.

Return the same as `jabber-presence-default-message' but only
if there is a chat buffer open for WHO, keeping the amount of presence messages
at a more manageable level when there are lots of users.

This function is not called directly, but can be used as the value for
`jabber-alert-presence-message-function'."
  (when (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from)))
    (jabber-presence-default-message who oldstatus newstatus statustext)))
;; jabber-presence-only-chat-open-message:1 ends here

;; [[file:jabber.org::#presence-wave][jabber-presence-wave:1]]
(defun jabber-presence-wave (who oldstatus newstatus statustext proposed-alert)
  "Play the wave file specified in `jabber-alert-presence-wave'."
  (when proposed-alert
    (let* ((case-fold-search t)
	   (bare-jid (symbol-name who))
	   (sound-file (or (dolist (entry jabber-alert-presence-wave-alist)
			     (when (string-match (car entry) bare-jid)
			       (cl-return (cdr entry))))
			   jabber-alert-presence-wave)))
      (unless (equal sound-file "")
	(funcall jabber-play-sound-file sound-file)))))
;; jabber-presence-wave:1 ends here

;; [[file:jabber.org::#+jabber-presence-update-roster+][+jabber-presence-update-roster+:1]]
;; This is now defined in jabber-roster.el.
;; (defun jabber-presence-update-roster (who oldstatus newstatus statustext proposed-alert)
;;   "Update the roster display by calling `jabber-display-roster'"
;;   (jabber-display-roster))
;; +jabber-presence-update-roster+:1 ends here

;; [[file:jabber.org::#presence-display][jabber-presence-display:1]]
(defun jabber-presence-display (who oldstatus newstatus statustext proposed-alert)
  "Display the roster buffer."
  (when proposed-alert
    (display-buffer jabber-roster-buffer)))
;; jabber-presence-display:1 ends here

;; [[file:jabber.org::#presence-switch][jabber-presence-switch:1]]
(defun jabber-presence-switch (who oldstatus newstatus statustext proposed-alert)
  "Switch to the roster buffer."
  (when proposed-alert
    (switch-to-buffer jabber-roster-buffer)))
;; jabber-presence-switch:1 ends here

;; [[file:jabber.org::#info-default-message][jabber-info-default-message:1]]
(defun jabber-info-default-message (infotype buffer)
  "Function for constructing info alert messages.

The argument is INFOTYPE, a symbol telling the kind of info request completed.
This function uses `jabber-info-message-alist' to find a message."
  (concat (cdr (assq infotype jabber-info-message-alist))
	  " (buffer "(buffer-name buffer) ")"))
;; jabber-info-default-message:1 ends here

;; [[file:jabber.org::#info-wave][jabber-info-wave:1]]
(defun jabber-info-wave (infotype buffer proposed-alert)
  "Play the wave file specified in `jabber-alert-info-wave'."
  (if proposed-alert
      (funcall jabber-play-sound-file jabber-alert-info-wave)))
;; jabber-info-wave:1 ends here

;; [[file:jabber.org::#info-display][jabber-info-display:1]]
(defun jabber-info-display (infotype buffer proposed-alert)
  "Display buffer of completed request."
  (when proposed-alert
    (display-buffer buffer)))
;; jabber-info-display:1 ends here

;; [[file:jabber.org::#info-switch][jabber-info-switch:1]]
(defun jabber-info-switch (infotype buffer proposed-alert)
  "Switch to buffer of completed request."
  (when proposed-alert
    (switch-to-buffer buffer)))
;; jabber-info-switch:1 ends here

;; [[file:jabber.org::#define-personal-jabber-alert][define-personal-jabber-alert:1]]
;;; Personal alert hooks
(defmacro define-personal-jabber-alert (name)
  "From ALERT function, make ALERT-personal function.

This makes sense only for MUC.

NAME: the name of the sender."
  (let ((sn (symbol-name name)))
    (let ((func (intern (format "%s-personal" sn))))
    `(progn
       (defun ,func (nick group buffer text title)
         (if (jabber-muc-looks-like-personal-p text group)
             (,name nick group buffer text title)))
       (cl-pushnew (quote ,func) (get 'jabber-alert-muc-hooks 'custom-options))))))
;; define-personal-jabber-alert:1 ends here

;; [[file:jabber.org::#define-personal-jabber-alert][define-personal-jabber-alert:2]]
(define-personal-jabber-alert jabber-muc-beep)
(define-personal-jabber-alert jabber-muc-wave)
(define-personal-jabber-alert jabber-muc-echo)
(define-personal-jabber-alert jabber-muc-switch)
(define-personal-jabber-alert jabber-muc-display)
;; define-personal-jabber-alert:2 ends here

;; [[file:jabber.org::#autoanswer-alist][jabber-autoanswer-alist:1]]
(defcustom jabber-autoanswer-alist nil
  "Specific phrases to autoanswer on specific message.
The keys are regexps matching the incoming message text, and the values are
autoanswer phrase."
  :type '(alist :key-type regexp :value-type string)
  :group 'jabber-alerts)
;; jabber-autoanswer-alist:1 ends here

;; [[file:jabber.org::#autoanswer-answer][jabber-autoanswer-answer:1]]
(defun jabber-autoanswer-answer (from buffer text proposed-alert)
  "Answer automaticaly when incoming text is in `jabber-autoanswer-alist'.
Answer automaticaly when incoming text match the first element of
`jabber-autoanswer-alist'"
  (when (and from buffer text proposed-alert jabber-autoanswer-alist)
    (let ((message
           (dolist (entry jabber-autoanswer-alist)
             (when (string-match (car entry) text)
               (cl-return (cdr entry))))))
      (if message
          (jabber-chat-send jabber-buffer-connection message)))))
(cl-pushnew 'jabber-autoanswer-answer (get 'jabber-alert-message-hooks 'custom-options))
;; jabber-autoanswer-answer:1 ends here

;; [[file:jabber.org::#autoanswer-answer-muc][jabber-autoanswer-answer-muc:1]]
(defun jabber-autoanswer-answer-muc (nick group buffer text proposed-alert)
  "Answer automaticaly when incoming text is in `jabber-autoanswer-alist'.
Answer automaticaly when incoming text match first element
of `jabber-autoanswer-alist'."
  (when (and nick group buffer text proposed-alert jabber-autoanswer-alist)
    (let ((message
           (dolist (entry jabber-autoanswer-alist)
             (when (string-match (car entry) text)
               (cl-return (cdr entry))))))
      (if message
          (jabber-chat-send jabber-buffer-connection message)))))
(cl-pushnew 'jabber-autoanswer-answer-muc (get 'jabber-alert-muc-hooks 'custom-options))
;; jabber-autoanswer-answer-muc:1 ends here

;; [[file:jabber.org::#history][jabber-history:1]]
(defgroup jabber-history nil "Customization options for Emacs
Jabber history files."
  :group 'jabber)
;; jabber-history:1 ends here

;; [[file:jabber.org::#history-enabled][jabber-history-enabled:1]]
(defcustom jabber-history-enabled nil
  "Non-nil means message logging is enabled."
  :type 'boolean
  :group 'jabber-history)
;; jabber-history-enabled:1 ends here

;; [[file:jabber.org::#history-muc-enabled][jabber-history-muc-enabled:1]]
(defcustom jabber-history-muc-enabled nil
  "Non-nil means MUC logging is enabled.
Default is nil, cause MUC logging may be i/o-intensive."
  :type 'boolean
  :group 'jabber-history)
;; jabber-history-muc-enabled:1 ends here

;; [[file:jabber.org::#history-dir][jabber-history-dir:1]]
(defcustom jabber-history-dir
  (locate-user-emacs-file "jabber-history" ".emacs-jabber")
  "Base directory where per-contact history files are stored.
Used only when `jabber-use-global-history' is nil."
  :type 'directory
  :group 'jabber-history)
;; jabber-history-dir:1 ends here

;; [[file:jabber.org::#global-history-filename][jabber-global-history-filename:1]]
(defcustom jabber-global-history-filename
  (locate-user-emacs-file "jabber-global-message-log" ".jabber_global_message_log")
  "Global file where all messages are logged.
Used when `jabber-use-global-history' is non-nil."
  :type 'file
  :group 'jabber-history)
;; jabber-global-history-filename:1 ends here

;; [[file:jabber.org::#use-global-history][jabber-use-global-history:1]]
(defcustom jabber-use-global-history
  ;; Using a global history file by default was a bad idea.  Let's
  ;; default to per-user files unless the global history file already
  ;; exists, to avoid breaking existing installations.
  (file-exists-p jabber-global-history-filename)
  "Whether to use a global file for message history.
If non-nil, `jabber-global-history-filename' is used, otherwise,
messages are stored in per-user files under the
`jabber-history-dir' directory."
  :type 'boolean
  :group 'jabber-history)
;; jabber-use-global-history:1 ends here

;; [[file:jabber.org::#history-enable-rotation][jabber-history-enable-rotation:1]]
(defcustom jabber-history-enable-rotation nil
  "Whether history files should be renamed when reach certain kilobytes.
Whether history files should be renamed when reach
`jabber-history-size-limit' kilobytes.  If nil, history files
will grow indefinitely, otherwise they'll be renamed to
<history-file>-<number>, where <number> is 1 or the smallest
number after the last rotation."
  :type 'boolean
  :group 'jabber-history)
;; jabber-history-enable-rotation:1 ends here

;; [[file:jabber.org::#history-size-limit][jabber-history-size-limit:1]]
(defcustom jabber-history-size-limit 1024
  "Maximum history file size in kilobytes.
When history file reaches this limit, it is renamed to
<history-file>-<number>, where <number> is 1 or the smallest
number after the last rotation."
  :type 'integer
  :group 'jabber-history)
;; jabber-history-size-limit:1 ends here

;; [[file:jabber.org::#history-inhibit-received-message-functions][jabber-history-inhibit-received-message-functions:1]]
(defvar jabber-history-inhibit-received-message-functions nil
  "Functions determining whether to log an incoming message stanza.
The functions in this list are called with two arguments,
the connection and the full message stanza.
If any of the functions returns non-nil, the stanza is not logged
in the message history.")
;; jabber-history-inhibit-received-message-functions:1 ends here

;; [[file:jabber.org::#rotate-history-p][jabber-rotate-history-p:1]]
(defun jabber-rotate-history-p (history-file)
  "Return non-nil if HISTORY-FILE should be rotated."
  (when (and jabber-history-enable-rotation
	     (file-exists-p history-file))
    (> (/ (nth 7 (file-attributes history-file)) 1024)
       jabber-history-size-limit)))
;; jabber-rotate-history-p:1 ends here

;; [[file:jabber.org::#history-rotate][jabber-history-rotate:1]]
(defun jabber-history-rotate (history-file &optional try)
  "Rename HISTORY-FILE to HISTORY-FILE-TRY."
  (let ((suffix (number-to-string (or try 1))))
    (if (file-exists-p (concat history-file "-"  suffix))
	(jabber-history-rotate history-file (if try (1+ try) 1))
      (rename-file history-file (concat history-file "-" suffix)))))
;; jabber-history-rotate:1 ends here

;; [[file:jabber.org::#message-history][jabber-message-history:1]]
(add-to-list 'jabber-message-chain 'jabber-message-history)
(defun jabber-message-history (jc xml-data)
  "Log message to log file.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (when (and (not jabber-use-global-history)
	     (not (file-directory-p jabber-history-dir)))
    (make-directory jabber-history-dir))
  (let ((is-muc (jabber-muc-message-p xml-data)))
    (when (and jabber-history-enabled
	       (or
		(not is-muc)                ;chat message or private MUC message
		(and jabber-history-muc-enabled is-muc))) ;muc message and muc logging active
      (unless (run-hook-with-args-until-success
	       'jabber-history-inhibit-received-message-functions
	       jc xml-data)
	(let ((from (jabber-xml-get-attribute xml-data 'from))
	      (text (car (jabber-xml-node-children
			  (car (jabber-xml-get-children xml-data 'body)))))
	      (timestamp (jabber-message-timestamp xml-data)))
	  (when (and from text)
	    (jabber-history-log-message "in" from nil text timestamp)))))))
;; jabber-message-history:1 ends here

;; [[file:jabber.org::#message-history][jabber-message-history:2]]
(add-hook 'jabber-chat-send-hooks 'jabber-history-send-hook)
;; jabber-message-history:2 ends here

;; [[file:jabber.org::#history-send-hook][jabber-history-send-hook:1]]
(defun jabber-history-send-hook (body id)
  "Log outgoing message to log file."
  (when (and (not jabber-use-global-history)
	     (not (file-directory-p jabber-history-dir)))
    (make-directory jabber-history-dir))
  ;; This function is called from a chat buffer, so jabber-chatting-with
  ;; contains the desired value.
  (if jabber-history-enabled
      (jabber-history-log-message "out" nil jabber-chatting-with body (current-time))))
;; jabber-history-send-hook:1 ends here

;; [[file:jabber.org::#history-filename][jabber-history-filename:1]]
(defun jabber-history-filename (contact)
  "Return a history filename for CONTACT.
Return a history filename for CONTACT if the per-user file
loggin strategy is used or the global history filename."
  (if jabber-use-global-history
      jabber-global-history-filename
    ;; jabber-jid-symbol is the best canonicalization we have.
    (concat jabber-history-dir
	    "/" (symbol-name (jabber-jid-symbol contact)))))
;; jabber-history-filename:1 ends here

;; [[file:jabber.org::#history-log-message][jabber-history-log-message:1]]
(defun jabber-history-log-message (direction from to body timestamp)
  "Log a message."
  (with-temp-buffer
    ;; Remove properties
    (set-text-properties 0 (length body) nil body)
    ;; Encode text as Lisp string - get decoding for free
    (setq body (prin1-to-string body))
    ;; Encode LF and CR
    (while (string-match "\n" body)
      (setq body (replace-match "\\n" nil t body nil)))
    (while (string-match "\r" body)
      (setq body (replace-match "\\r" nil t body nil)))
    (insert (format "[\"%s\" \"%s\" %s %s %s]\n"
		    (jabber-encode-time (or timestamp (current-time)))
		    (or direction
			"in")
		    (or (when from
			  (prin1-to-string from))
			"\"me\"")
		    (or (when to
			  (prin1-to-string to))
			"\"me\"")
		    body))
    (let ((coding-system-for-write 'utf-8)
	  (history-file (jabber-history-filename (or from to))))
      (when (and (not jabber-use-global-history)
		 (not (file-directory-p jabber-history-dir)))
	(make-directory jabber-history-dir))
      (when (jabber-rotate-history-p history-file)
	(jabber-history-rotate history-file))
      (condition-case e
	  (write-region (point-min) (point-max) history-file t 'quiet)
	(error
	 (message "Unable to write history: %s" (error-message-string e)))))))
;; jabber-history-log-message:1 ends here

;; [[file:jabber.org::#history-query][jabber-history-query:1]]
(defun jabber-history-query (start-time
			     end-time
			     number
			     direction
			     jid-regexp
			     history-file)
  "Return a list of vectors, one for each message matching the criteria.
START-TIME and END-TIME are floats as obtained from `float-time'.
Either or both may be nil, meaning no restriction.
NUMBER is the maximum number of messages to return, or t for
unlimited.
DIRECTION is either \"in\" or \"out\", or t for no limit on direction.
JID-REGEXP is a regexp which must match the JID.
HISTORY-FILE is the file in which to search.

Currently jabber-history-query performs a linear search from the end
of the log file."
  (when (file-readable-p history-file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
	(if jabber-use-global-history
            (insert-file-contents history-file)
          (let* ((lines-collected nil)
                (matched-files
		 (directory-files jabber-history-dir t
				  (concat "^"
					  (regexp-quote (file-name-nondirectory
							 history-file)))))
                (matched-files
		 (cons (car matched-files)
		       (sort (cdr matched-files) 'string>-numerical))))
            (while (not lines-collected)
              (if (null matched-files)
                  (setq lines-collected t)
                (let ((file (pop matched-files)))
                  (progn
                    (insert-file-contents file)
                    (when (numberp number)
                      (if (>= (count-lines (point-min) (point-max)) number)
                        (setq lines-collected t))))))))))
      (let (collected current-line)
	(goto-char (point-max))
	(catch 'beginning-of-file
	    (while (progn
		     (backward-sexp)
		     (setq current-line (car (read-from-string
					      (buffer-substring
					       (point)
					       (save-excursion
						 (forward-sexp)
						 (point))))))
		     (and (or (null start-time)
			      (> (jabber-float-time (jabber-parse-time
						     (aref current-line 0)))
				 start-time))
			  (or (eq number t)
			      (< (length collected) number))))
	      (if (and (or (eq direction t)
			   (string= direction (aref current-line 1)))
		       (or (null end-time)
			   (> end-time (jabber-float-time (jabber-parse-time
							   (aref current-line 0)))))
		       (string-match
			jid-regexp
			(car
			 (remove "me"
				 (list (aref current-line 2)
				       (aref current-line 3))))))
		  (push current-line collected))
	      (when (bobp)
		(throw 'beginning-of-file nil))))
	collected))))
;; jabber-history-query:1 ends here

;; [[file:jabber.org::#backlog-days][jabber-backlog-days:1]]
(defcustom jabber-backlog-days 3.0
  "Age limit on messages in chat buffer backlog, in days."
  :group 'jabber
  :type '(choice (number :tag "Number of days")
		 (const :tag "No limit" nil)))
;; jabber-backlog-days:1 ends here

;; [[file:jabber.org::#backlog-number][jabber-backlog-number:1]]
(defcustom jabber-backlog-number 10
  "Maximum number of messages in chat buffer backlog."
  :group 'jabber
  :type 'integer)
;; jabber-backlog-number:1 ends here

;; [[file:jabber.org::#history-backlog][jabber-history-backlog:1]]
(defun jabber-history-backlog (jid &optional before)
  "Fetch context from previous chats with JID.
Return a list of history entries (vectors), limited by
`jabber-backlog-days' and `jabber-backlog-number'.
If BEFORE is non-nil, it should be a float-time after which
no entries will be fetched.  `jabber-backlog-days' still
applies, though."
  (jabber-history-query
   (and jabber-backlog-days
	(- (jabber-float-time) (* jabber-backlog-days 86400.0)))
   before
   jabber-backlog-number
   t					; both incoming and outgoing
   (concat "^" (regexp-quote (jabber-jid-user jid)) "\\(/.*\\)?$")
   (jabber-history-filename jid)))
;; jabber-history-backlog:1 ends here

;; [[file:jabber.org::#history-move-to-per-user][jabber-history-move-to-per-user:1]]
(defun jabber-history-move-to-per-user ()
  "Migrate global history to per-user files."
  (interactive)
  (when (file-directory-p jabber-history-dir)
    (error "Per-user history directory already exists"))
  (make-directory jabber-history-dir)
  (let ((jabber-use-global-history nil))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
	(insert-file-contents jabber-global-history-filename))
      (let ((progress-reporter
	     (when (fboundp 'make-progress-reporter)
	       (make-progress-reporter "Migrating history..."
				       (point-min) (point-max))))
	    ;;(file-table (make-hash-table :test 'equal))
	    ;; Keep track of blocks of entries pertaining to the same JID.
	    current-jid jid-start)
	(while (not (eobp))
	  (let* ((start (point))
		 (end (progn (forward-line) (point)))
		 (line (buffer-substring start end))
		 (parsed (car (read-from-string line)))
		 (jid (if (string= (aref parsed 2) "me")
			  (aref parsed 3)
			(aref parsed 2))))
	    ;; Whenever there is a change in JID...
	    (when (not (equal jid current-jid))
	      (when current-jid
		;; ...save data for previous JID...
		(let ((history-file (jabber-history-filename current-jid)))
		  (write-region jid-start start history-file t 'quiet)))
	      ;; ...and switch to new JID.
	      (setq current-jid jid)
	      (setq jid-start start))
	    (when (fboundp 'progress-reporter-update)
	      (progress-reporter-update progress-reporter (point)))))
	;; Finally, save the last block, if any.
	(when current-jid
	  (let ((history-file (jabber-history-filename current-jid)))
	    (write-region jid-start (point-max) history-file t 'quiet))))))
  (message "Done.  Please change `jabber-use-global-history' now."))
;; jabber-history-move-to-per-user:1 ends here

;; [[file:jabber.org::#point-insert-1][jabber-point-insert:1]]
(defvar jabber-point-insert nil
  "Position where the message being composed starts.")
;; jabber-point-insert:1 ends here

;; [[file:jabber.org::#send-function-1][jabber-send-function:1]]
(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")
;; jabber-send-function:1 ends here

;; [[file:jabber.org::#chat-mode-hook][jabber-chat-mode-hook:1]]
(defvar jabber-chat-mode-hook nil
  "Hook called at the end of `jabber-chat-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")
;; jabber-chat-mode-hook:1 ends here

;; [[file:jabber.org::#chat-fill-long-lines][jabber-chat-fill-long-lines:1]]
(defcustom jabber-chat-fill-long-lines t
  "If non-nil, fill long lines in chat buffers.
Lines are broken at word boundaries at the width of the
window or at `fill-column', whichever is shorter."
  :group 'jabber-chat
  :type 'boolean)
;; jabber-chat-fill-long-lines:1 ends here

;; [[file:jabber.org::#chat-ewoc][jabber-chat-ewoc:1]]
(defvar jabber-chat-ewoc nil
  "The ewoc showing the messages of this chat buffer.")
;; jabber-chat-ewoc:1 ends here

;; [[file:jabber.org::#buffer-connection][jabber-buffer-connection:1]]
;;;###autoload
(defvar jabber-buffer-connection nil
  "The connection used by this buffer.")
;;;###autoload
(make-variable-buffer-local 'jabber-buffer-connection)
;; jabber-buffer-connection:1 ends here

;; [[file:jabber.org::#chat-mode][jabber-chat-mode:1]]
(defun jabber-chat-mode (jc ewoc-pp)
  "Jabber chat mode.
\\{jabber-chat-mode-map}

JC is the Jabber connection."
  (kill-all-local-variables)
  ;; Make sure to set this variable somewhere
  (make-local-variable 'jabber-send-function)
  (make-local-variable 'scroll-conservatively)
  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-chat-ewoc)
  (make-local-variable 'buffer-undo-list)

  (setq jabber-buffer-connection jc
        scroll-conservatively 5
        buffer-undo-list t)             ;dont keep undo list for chatbuffer

  (unless jabber-chat-ewoc
    (setq jabber-chat-ewoc
	  (ewoc-create ewoc-pp nil "---"))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker)))

  ;;(setq header-line-format jabber-chat-header-line-format)

  (setq major-mode 'jabber-chat-mode
        mode-name "jabber-chat")
  (use-local-map jabber-chat-mode-map)

  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-chat-mode-hook)
    (run-hooks 'jabber-chat-mode-hook)))
;; jabber-chat-mode:1 ends here

;; [[file:jabber.org::#chat-mode-flyspell-verify][jabber-chat-mode-flyspell-verify:1]]
(put 'jabber-chat-mode 'mode-class 'special)

;; Spell check only what you're currently writing
(defun jabber-chat-mode-flyspell-verify ()
  (>= (point) jabber-point-insert))
(put 'jabber-chat-mode 'flyspell-mode-predicate
  'jabber-chat-mode-flyspell-verify)
;; jabber-chat-mode-flyspell-verify:1 ends here

;; [[file:jabber.org::#chat-mode-map][jabber-chat-mode-map:1]]
(defvar jabber-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map "\r" 'jabber-chat-buffer-send)
    map))
;; jabber-chat-mode-map:1 ends here

;; [[file:jabber.org::#chat-buffer-send][jabber-chat-buffer-send:1]]
(defun jabber-chat-buffer-send ()
  (interactive)
  ;; If user accidentally hits RET without writing anything, just
  ;; ignore it.
  (when (cl-plusp (- (point-max) jabber-point-insert))
    ;; If connection was lost...
    (unless (memq jabber-buffer-connection jabber-connections)
      ;; ...maybe there is a new connection to the same account.
      (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
	(if new-jc
	    ;; If so, just use it.
	    (setq jabber-buffer-connection new-jc)
	  ;; Otherwise, ask for a new account.
	  (setq jabber-buffer-connection (jabber-read-account t)))))

    (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
      (funcall jabber-send-function jabber-buffer-connection body))))
;; jabber-chat-buffer-send:1 ends here

;; [[file:jabber.org::#chat-buffer-fill-long-lines][jabber-chat-buffer-fill-long-lines:1]]
(defun jabber-chat-buffer-fill-long-lines ()
  "Fill lines that are wider than the window width."
  ;; This was mostly stolen from article-fill-long-lines
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (width (window-width (get-buffer-window (current-buffer)))))
      (goto-char (point-min))
      (let ((adaptive-fill-mode nil))	;Why?  -sm
	(while (not (eobp))
	  (end-of-line)
	  (when (>= (current-column) (min fill-column width))
	    (save-restriction
	      (narrow-to-region (min (1+ (point)) (point-max))
				(point-at-bol))
	      (let ((goback (point-marker)))
		(fill-paragraph nil)
		(goto-char (marker-position goback)))))
	  (forward-line 1))))))
;; jabber-chat-buffer-fill-long-lines:1 ends here

;; [[file:jabber.org::#compose][jabber-compose:1]]
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
;; jabber-compose:1 ends here

;; [[file:jabber.org::#compose-send][jabber-compose-send:1]]
(defun jabber-compose-send (&rest ignore)
  (let ((recipients (widget-value (cdr (assq :recipients jabber-widget-alist))))
	(subject (widget-value (cdr (assq :subject jabber-widget-alist))))
	(text (widget-value (cdr (assq :text jabber-widget-alist)))))
    (when (null recipients)
      (error "No recipients specified"))

    (dolist (to recipients)
      (jabber-send-message jabber-buffer-connection to subject text nil))

    (bury-buffer)
    (message "Message sent")))
;; jabber-compose-send:1 ends here

;; [[file:jabber.org::#one-to-one-chats][One-to-one chats:1]]
(require 'ewoc)
;; One-to-one chats:1 ends here

;; [[file:jabber.org::#chat][jabber-chat:1]]
(defgroup jabber-chat nil "chat display options"
  :group 'jabber)
;; jabber-chat:1 ends here

;; [[file:jabber.org::#chat-buffer-format][jabber-chat-buffer-format:1]]
(defcustom jabber-chat-buffer-format "*-jabber-chat-%n-*"
  "The format specification for the name of chat buffers.

These fields are available (all are about the person you are chatting
with):

%n   Nickname, or JID if no nickname set
%j   Bare JID (without resource)
%r   Resource"
  :type 'string
  :group 'jabber-chat)
;; jabber-chat-buffer-format:1 ends here

;; [[file:jabber.org::#chat-header-line-format][jabber-chat-header-line-format:1]]
(defcustom jabber-chat-header-line-format
  '("" (jabber-chat-buffer-show-avatar
	(:eval
	 (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
	   (jabber-propertize " "
			      'display (get buddy 'avatar)))))
    (:eval (jabber-jid-displayname jabber-chatting-with))
    "\t" (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
		  (propertize
		   (or
		    (cdr (assoc (get buddy 'show) jabber-presence-strings))
		    (get buddy 'show))
		   'face
		   (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
		       'jabber-roster-user-online))))
    "\t" (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with) 'status)))
    "\t" jabber-events-message		;see jabber-events.el
    "\t" jabber-chatstates-message)		;see jabber-chatstates.el
  "The specification for the header line of chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)
;; jabber-chat-header-line-format:1 ends here

;; [[file:jabber.org::#chat-buffer-show-avatar][jabber-chat-buffer-show-avatar:1]]
(defcustom jabber-chat-buffer-show-avatar t
  "Show avatars in header line of chat buffer?
This variable might not take effect if you have changed
`jabber-chat-header-line-format'."
  :type 'boolean
  :group 'jabber-chat)
;; jabber-chat-buffer-show-avatar:1 ends here

;; [[file:jabber.org::#chat-time-format][jabber-chat-time-format:1]]
(defcustom jabber-chat-time-format "%H:%M"
  "The format specification for instant messages in the chat buffer.
See also `jabber-chat-delayed-time-format'.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)
;; jabber-chat-time-format:1 ends here

;; [[file:jabber.org::#chat-delayed-time-format][jabber-chat-delayed-time-format:1]]
(defcustom jabber-chat-delayed-time-format "%Y-%m-%d %H:%M"
  "The format specification for delayed messages in the chat buffer.
See also `jabber-chat-time-format'.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)
;; jabber-chat-delayed-time-format:1 ends here

;; [[file:jabber.org::#print-rare-time][jabber-print-rare-time:1]]
(defcustom jabber-print-rare-time t
  "Non-nil means to print \"rare time\" indications in chat buffers.
The default settings tell every new hour."
  :type 'boolean
  :group 'jabber-chat)
;; jabber-print-rare-time:1 ends here

;; [[file:jabber.org::#rare-time-format][jabber-rare-time-format:1]]
(defcustom jabber-rare-time-format "%a %e %b %Y %H:00"
  "The format specification for the rare time information.
Rare time information will be printed whenever the current time,
formatted according to this string, is different to the last
rare time printed."
  :type 'string
  :group 'jabber-chat)
;; jabber-rare-time-format:1 ends here

;; [[file:jabber.org::#rare-time-face][jabber-rare-time-face:1]]
(defface jabber-rare-time-face
  '((t (:foreground "darkgreen" :underline t)))
  "face for displaying the rare time info"
  :group 'jabber-chat)
;; jabber-rare-time-face:1 ends here

;; [[file:jabber.org::#chat-local-prompt-format][jabber-chat-local-prompt-format:1]]
(defcustom jabber-chat-local-prompt-format "[%t] %n> "
  "The format specification for lines you type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%u   Username
%n   Nickname (obsolete, same as username)
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)
;; jabber-chat-local-prompt-format:1 ends here

;; [[file:jabber.org::#chat-foreign-prompt-format][jabber-chat-foreign-prompt-format:1]]
(defcustom jabber-chat-foreign-prompt-format "[%t] %n> "
  "The format specification for lines others type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%n   Nickname, or JID if no nickname set
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)
;; jabber-chat-foreign-prompt-format:1 ends here

;; [[file:jabber.org::#chat-system-prompt-format][jabber-chat-system-prompt-format:1]]
(defcustom jabber-chat-system-prompt-format "[%t] *** "
  "The format specification for lines from the system or that are special in the chat buffer."
  :type 'string
  :group 'jabber-chat)
;; jabber-chat-system-prompt-format:1 ends here

;; [[file:jabber.org::#chat-prompt-local][jabber-chat-prompt-local:1]]
(defface jabber-chat-prompt-local
  '((t (:foreground "blue" :weight bold)))
  "face for displaying the chat prompt for what you type in"
  :group 'jabber-chat)
;; jabber-chat-prompt-local:1 ends here

;; [[file:jabber.org::#chat-prompt-foreign][jabber-chat-prompt-foreign:1]]
(defface jabber-chat-prompt-foreign
  '((t (:foreground "red" :weight bold)))
  "face for displaying the chat prompt for what they send"
  :group 'jabber-chat)
;; jabber-chat-prompt-foreign:1 ends here

;; [[file:jabber.org::#chat-prompt-system][jabber-chat-prompt-system:1]]
(defface jabber-chat-prompt-system
  '((t (:foreground "green" :weight bold)))
  "face used for system and special messages"
  :group 'jabber-chat)
;; jabber-chat-prompt-system:1 ends here

;; [[file:jabber.org::#chat-text-local][jabber-chat-text-local:1]]
(defface jabber-chat-text-local '((t ()))
  "Face used for text you write"
  :group 'jabber-chat)
;; jabber-chat-text-local:1 ends here

;; [[file:jabber.org::#chat-text-foreign][jabber-chat-text-foreign:1]]
(defface jabber-chat-text-foreign '((t ()))
  "Face used for text others write"
  :group 'jabber-chat)
;; jabber-chat-text-foreign:1 ends here

;; [[file:jabber.org::#chat-error][jabber-chat-error:1]]
(defface jabber-chat-error
  '((t (:foreground "red" :weight bold)))
  "Face used for error messages"
  :group 'jabber-chat)
;; jabber-chat-error:1 ends here

;; [[file:jabber.org::#chatting-][jabber-chatting-with:1]]
;;;###autoload
(defvar jabber-chatting-with nil
  "JID of the person you are chatting with.")
;; jabber-chatting-with:1 ends here

;; [[file:jabber.org::#chat-printers][jabber-chat-printers:1]]
(defvar jabber-chat-printers '(jabber-chat-print-subject
			       jabber-chat-print-body
			       jabber-chat-print-url
			       jabber-chat-goto-address)
  "List of functions that may be able to print part of a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local or :foreign, for sent or received stanza, respectively
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.")
;; jabber-chat-printers:1 ends here

;; [[file:jabber.org::#body-printers][jabber-body-printers:1]]
(defvar jabber-body-printers '(jabber-chat-normal-body)
  "List of functions that may be able to print a body for a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local, :foreign or :error
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.

These functions are called in order, until one of them returns
non-nil.

Add a function to the beginning of this list if the tag it handles
replaces the contents of the <body/> tag.")
;; jabber-body-printers:1 ends here

;; [[file:jabber.org::#chat-send-hooks][jabber-chat-send-hooks:1]]
(defvar jabber-chat-send-hooks nil
  "List of functions called when a chat message is sent.
The arguments are the text to send, and the id attribute of the
message.

The functions should return a list of XML nodes they want to be
added to the outgoing message.")
;; jabber-chat-send-hooks:1 ends here

;; [[file:jabber.org::#chat-earliest-backlog][jabber-chat-earliest-backlog:1]]
(defvar jabber-chat-earliest-backlog nil
  "Float-time of earliest backlog entry inserted into buffer.
nil if no backlog has been inserted.")
;; jabber-chat-earliest-backlog:1 ends here

;; [[file:jabber.org::#chat-get-buffer][jabber-chat-get-buffer:1]]
;;;###autoload
(defun jabber-chat-get-buffer (chat-with)
  "Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-chat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname chat-with))
		(cons ?j (jabber-jid-user chat-with))
		(cons ?r (or (jabber-jid-resource chat-with) "")))))
;; jabber-chat-get-buffer:1 ends here

;; [[file:jabber.org::#chat-create-buffer][jabber-chat-create-buffer:1]]
(defun jabber-chat-create-buffer (jc chat-with)
  "Prepare a buffer for chatting with CHAT-WITH.
This function is idempotent.
JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-chat-get-buffer chat-with))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode jc #'jabber-chat-pp)

      (make-local-variable 'jabber-chatting-with)
      (setq jabber-chatting-with chat-with)
      (setq jabber-send-function 'jabber-chat-send)
      (setq header-line-format jabber-chat-header-line-format)

      (make-local-variable 'jabber-chat-earliest-backlog)

      ;; insert backlog
      (when (null jabber-chat-earliest-backlog)
	(let ((backlog-entries (jabber-history-backlog chat-with)))
	  (if (null backlog-entries)
	      (setq jabber-chat-earliest-backlog (jabber-float-time))
	    (setq jabber-chat-earliest-backlog
		  (jabber-float-time (jabber-parse-time
				      (aref (car backlog-entries) 0))))
	    (mapc 'jabber-chat-insert-backlog-entry (nreverse backlog-entries))))))

    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)

    (current-buffer)))
;; jabber-chat-create-buffer:1 ends here

;; [[file:jabber.org::#chat-insert-backlog-entry][jabber-chat-insert-backlog-entry:1]]
(defun jabber-chat-insert-backlog-entry (msg)
  "Insert backlog entry MSG at beginning of buffer."
  ;; Rare timestamps are especially important in backlog.  We risk
  ;; having superfluous timestamps if we just add before each backlog
  ;; entry.
  (let* ((message-time (jabber-parse-time (aref msg 0)))
	 (fake-stanza `(message ((from . ,(aref msg 2)))
				(body nil ,(aref msg 4))
				(x ((xmlns . "jabber:x:delay")
				    (stamp . ,(jabber-encode-legacy-time message-time))))))
	 (node-data (list (if (string= (aref msg 1) "in") :foreign :local)
			  fake-stanza :delayed t)))

    ;; Insert after existing rare timestamp?
    (if (and jabber-print-rare-time
	     (ewoc-nth jabber-chat-ewoc 0)
	     (eq (car (ewoc-data (ewoc-nth jabber-chat-ewoc 0))) :rare-time)
	     (not (jabber-rare-time-needed message-time (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc 0))))))
	(ewoc-enter-after jabber-chat-ewoc (ewoc-nth jabber-chat-ewoc 0) node-data)
      ;; Insert first.
      (ewoc-enter-first jabber-chat-ewoc node-data)
      (when jabber-print-rare-time
	(ewoc-enter-first jabber-chat-ewoc (list :rare-time message-time))))))
;; jabber-chat-insert-backlog-entry:1 ends here

;; [[file:jabber.org::#chat-insert-backlog-entry][jabber-chat-insert-backlog-entry:2]]
(add-to-list 'jabber-jid-chat-menu
	     (cons "Display more context" 'jabber-chat-display-more-backlog))
;; jabber-chat-insert-backlog-entry:2 ends here

;; [[file:jabber.org::#chat-display-more-backlog][jabber-chat-display-more-backlog:1]]
(defun jabber-chat-display-more-backlog (how-many)
  "Display more context.  HOW-MANY is number of messages.  Specify 0 to display all messages."
  (interactive "nHow many more messages (Specify 0 to display all)? ")
  (let* ((inhibit-read-only t)
	 (jabber-backlog-days nil)
	 (jabber-backlog-number (if (= how-many 0) t how-many))
	 (backlog-entries (jabber-history-backlog
			   (or jabber-chatting-with jabber-group) jabber-chat-earliest-backlog)))
    (when backlog-entries
      (setq jabber-chat-earliest-backlog
	    (jabber-float-time (jabber-parse-time
				(aref (car backlog-entries) 0))))
      (save-excursion
	(goto-char (point-min))
	(mapc 'jabber-chat-insert-backlog-entry (nreverse backlog-entries))))))
;; jabber-chat-display-more-backlog:1 ends here

;; [[file:jabber.org::#chat-display-more-backlog][jabber-chat-display-more-backlog:2]]
(add-to-list 'jabber-message-chain 'jabber-process-chat)
;; jabber-chat-display-more-backlog:2 ends here

;; [[file:jabber.org::#get-forwarded-message][jabber-get-forwarded-message:1]]
(defun jabber-get-forwarded-message (xml-data)
  (let* ((sent (car (jabber-xml-get-children xml-data 'sent)))
         (forwarded (car (jabber-xml-get-children sent 'forwarded)))
         (forwarded-message (car (jabber-xml-get-children forwarded 'message))))
    (when forwarded-message
      forwarded-message)))
;; jabber-get-forwarded-message:1 ends here

;; [[file:jabber.org::#process-chat][jabber-process-chat:1]]
(defun jabber-process-chat (jc xml-data)
  "If XML-DATA is a one-to-one chat message, handle it as such.
JC is the Jabber connection."
  ;; For now, everything that is not a public MUC message is
  ;; potentially a 1to1 chat message.
  (when (not (jabber-muc-message-p xml-data))
    ;; Note that we handle private MUC messages here.
    (cl-destructuring-bind (xml-data chat-buffer)
        (if (car (jabber-xml-get-children xml-data 'sent))
            (let* ((fwd-msg (jabber-get-forwarded-message xml-data))
                   (to (jabber-xml-get-attribute fwd-msg 'to)))
              (list fwd-msg
                    (jabber-chat-create-buffer jc to)))
          (list xml-data nil))
      (let ((from (jabber-xml-get-attribute xml-data 'from))
	    (error-p (jabber-xml-get-children xml-data 'error))
	    (body-text (car (jabber-xml-node-children
			     (car (jabber-xml-get-children
				   xml-data 'body))))))
        ;; First check if we would output anything for this stanza.
        (when (or error-p
		  (run-hook-with-args-until-success 'jabber-chat-printers
                                                    xml-data
                                                    :foreign :printp))
          ;; If so, create chat buffer, if necessary...
	  (with-current-buffer (if (jabber-muc-sender-p from)
				   (jabber-muc-private-create-buffer
				    jc
				    (jabber-jid-user from)
				    (jabber-jid-resource from))
			         (or chat-buffer
                                     (jabber-chat-create-buffer jc from)))
            ;; ...add the message to the ewoc...
	    (let ((node (ewoc-enter-last jabber-chat-ewoc
                                         (list (if error-p :error :foreign)
                                               xml-data
                                               :time
                                               (current-time)))))
	      (jabber-maybe-print-rare-time node))

            ;; ...and call alert hooks.
	    (dolist (hook '(jabber-message-hooks jabber-alert-message-hooks))
	      (run-hook-with-args hook
				  from (current-buffer) body-text
				  (funcall jabber-alert-message-function
					   from (current-buffer) body-text)))))))))
;; jabber-process-chat:1 ends here

;; [[file:jabber.org::#chat-send][jabber-chat-send:1]]
(defun jabber-chat-send (jc body)
  "Send BODY through connection JC, and display it in chat buffer.
JC is the Jabber connection."
  ;; Build the stanza...
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
	 (stanza-to-send `(message
			   ((to . ,jabber-chatting-with)
			    (type . "chat")
			    (id . ,id))
			   (body () ,body))))
    ;; ...add additional elements...
    ;; TODO: Once we require Emacs 24.1, use `run-hook-wrapped' instead.
    ;; That way we don't need to eliminate the "local hook" functionality
    ;; here.
    (dolist (hook jabber-chat-send-hooks)
      (if (eq hook t)
	  ;; Local hook referring to global...
	  (when (local-variable-p 'jabber-chat-send-hooks)
	    (dolist (global-hook (default-value 'jabber-chat-send-hooks))
	      (nconc stanza-to-send (funcall global-hook body id))))
      (nconc stanza-to-send (funcall hook body id))))
    ;; ...display it, if it would be displayed.
    (when (run-hook-with-args-until-success 'jabber-chat-printers stanza-to-send :local :printp)
      (jabber-maybe-print-rare-time
       (ewoc-enter-last jabber-chat-ewoc (list :local stanza-to-send :time (current-time)))))
    ;; ...and send it...
    (jabber-send-sexp jc stanza-to-send)))
;; jabber-chat-send:1 ends here

;; [[file:jabber.org::#chat-pp][jabber-chat-pp:1]]
(defun jabber-chat-pp (data)
  "Pretty-print a <message/> stanza.
\(car data) is either :local, :foreign, :error or :notice.
\(cadr data) is the <message/> stanza.
This function is used as an ewoc prettyprinter."
  (let* ((beg (point))
         (original-timestamp (when (listp (cadr data))
                               (jabber-message-timestamp (cadr data))))
         (internal-time
          (plist-get (cddr data) :time))
         (body (ignore-errors (car
               (jabber-xml-node-children
                (car
                 (jabber-xml-get-children (cadr data) 'body))))))
         (/me-p
          (and (> (length body) 4)
               (string= (substring body 0 4) "/me "))))

    ;; Print prompt...
    (let ((delayed (or original-timestamp (plist-get (cddr data) :delayed)))
          (prompt-start (point)))
      (cl-case (car data)
        (:local
         (jabber-chat-self-prompt (or original-timestamp internal-time)
                                  delayed
                                  /me-p))
        (:foreign
         (if (and (listp (cadr data))
                  (jabber-muc-private-message-p (cadr data)))
             (jabber-muc-private-print-prompt (cadr data))
           ;; For :error and :notice, this might be a string... beware
           (jabber-chat-print-prompt (when (listp (cadr data)) (cadr data))
                                     (or original-timestamp internal-time)
                                     delayed
                                     /me-p)))
        ((:error :notice :subscription-request)
         (jabber-chat-system-prompt (or original-timestamp internal-time)))
        (:muc-local
         (jabber-muc-print-prompt (cadr data) t /me-p))
        (:muc-foreign
         (jabber-muc-print-prompt (cadr data) nil /me-p))
        ((:muc-notice :muc-error)
         (jabber-muc-system-prompt)))
      (put-text-property prompt-start (point) 'field 'jabber-prompt))

    ;; ...and body
    (cl-case (car data)
      ((:local :foreign)
       (run-hook-with-args 'jabber-chat-printers (cadr data) (car data) :insert))
      ((:muc-local :muc-foreign)
       (dolist (hook '(jabber-muc-printers jabber-chat-printers))
         (run-hook-with-args hook (cadr data) (car data) :insert)))
      ((:error :muc-error)
       (if (stringp (cadr data))
            (insert (jabber-propertize (cadr data) 'face 'jabber-chat-error))
         (jabber-chat-print-error (cadr data))))
      ((:notice :muc-notice)
       (insert (cadr data)))
      (:rare-time
       (insert (jabber-propertize (format-time-string jabber-rare-time-format (cadr data))
                                  'face 'jabber-rare-time-face)))
      (:subscription-request
       (insert "This user requests subscription to your presence.\n")
       (when (and (stringp (cadr data)) (not (zerop (length (cadr data)))))
         (insert "Message: " (cadr data) "\n"))
       (insert "Accept?\n\n")
       (cl-flet ((button
               (text action)
               (if (fboundp 'insert-button)
                   (insert-button text 'action action)
                 ;; simple button replacement
                 (let ((keymap (make-keymap)))
                   (define-key keymap "\r" action)
                   (insert (jabber-propertize text 'keymap keymap 'face 'highlight))))
               (insert "\t")))
         (button "Mutual" 'jabber-subscription-accept-mutual)
         (button "One-way" 'jabber-subscription-accept-one-way)
         (button "Decline" 'jabber-subscription-decline))))

    (when jabber-chat-fill-long-lines
      (save-restriction
        (narrow-to-region beg (point))
        (jabber-chat-buffer-fill-long-lines)))

    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)))
;; jabber-chat-pp:1 ends here

;; [[file:jabber.org::#rare-time-needed][jabber-rare-time-needed:1]]
(defun jabber-rare-time-needed (time1 time2)
  "Return non-nil if a timestamp should be printed between TIME1 and TIME2."
  (not (string= (format-time-string jabber-rare-time-format time1)
		(format-time-string jabber-rare-time-format time2))))
;; jabber-rare-time-needed:1 ends here

;; [[file:jabber.org::#maybe-print-rare-time][jabber-maybe-print-rare-time:1]]
(defun jabber-maybe-print-rare-time (node)
  "Print rare time before NODE, if appropriate."
  (let* ((prev (ewoc-prev jabber-chat-ewoc node))
	 (data (ewoc-data node))
	 (prev-data (when prev (ewoc-data prev))))
    (cl-flet ((entry-time (entry)
		       (or (when (listp (cadr entry))
			     (jabber-message-timestamp (cadr entry)))
			   (plist-get (cddr entry) :time))))
      (when (and jabber-print-rare-time
		 (or (null prev)
		     (jabber-rare-time-needed (entry-time prev-data)
					      (entry-time data))))
	(ewoc-enter-before jabber-chat-ewoc node
			   (list :rare-time (entry-time data)))))))
;; jabber-maybe-print-rare-time:1 ends here

;; [[file:jabber.org::#chat-print-prompt][jabber-chat-print-prompt:1]]
(defun jabber-chat-print-prompt (xml-data timestamp delayed dont-print-nick-p)
  "Print prompt for received message in XML-DATA.
TIMESTAMP is the timestamp to print, or nil to get it
from a jabber:x:delay element.
If DELAYED is non-nil, print long timestamp
\(`jabber-chat-delayed-time-format' as opposed to
`jabber-chat-time-format').
If DONT-PRINT-NICK-P is non-nil, don't include nickname."
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(timestamp (or timestamp (jabber-message-timestamp xml-data))))
    (insert (jabber-propertize
	     (format-spec jabber-chat-foreign-prompt-format
			  (list
			   (cons ?t (format-time-string
				     (if delayed
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (if dont-print-nick-p "" (jabber-jid-displayname from)))
			   (cons ?u (or (jabber-jid-username from) from))
			   (cons ?r (jabber-jid-resource from))
			   (cons ?j (jabber-jid-user from))))
	     'face 'jabber-chat-prompt-foreign
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " from)))))
;; jabber-chat-print-prompt:1 ends here

;; [[file:jabber.org::#chat-system-prompt][jabber-chat-system-prompt:1]]
(defun jabber-chat-system-prompt (timestamp)
  (insert (jabber-propertize
	   (format-spec jabber-chat-foreign-prompt-format
			(list
			 (cons ?t (format-time-string jabber-chat-time-format
						      timestamp))
			 (cons ?n "")
			 (cons ?u "")
			 (cons ?r "")
			 (cons ?j "")))
	   'face 'jabber-chat-prompt-system
	   'help-echo
	   (concat (format-time-string "System message on %Y-%m-%d %H:%M:%S" timestamp)))))
;; jabber-chat-system-prompt:1 ends here

;; [[file:jabber.org::#chat-self-prompt][jabber-chat-self-prompt:1]]
(defun jabber-chat-self-prompt (timestamp delayed dont-print-nick-p)
  "Print prompt for sent message.
TIMESTAMP is the timestamp to print, or nil for now.
If DELAYED is non-nil, print long timestamp
\(`jabber-chat-delayed-time-format' as opposed to
`jabber-chat-time-format').
If DONT-PRINT-NICK-P is non-nil, don't include nickname."
  (let* ((state-data (fsm-get-state-data jabber-buffer-connection))
	 (username (plist-get state-data :username))
	 (server (plist-get state-data :server))
	 (resource (plist-get state-data :resource))
	 (nickname username))
    (insert (jabber-propertize
	     (format-spec jabber-chat-local-prompt-format
			  (list
			   (cons ?t (format-time-string
				     (if delayed
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (if dont-print-nick-p "" nickname))
			   (cons ?u username)
			   (cons ?r resource)
			   (cons ?j (concat username "@" server))))
	     'face 'jabber-chat-prompt-local
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from you")))))
;; jabber-chat-self-prompt:1 ends here

;; [[file:jabber.org::#chat-print-error][jabber-chat-print-error:1]]
(defun jabber-chat-print-error (xml-data)
  "Print error in given <message/> in a readable way.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((the-error (car (jabber-xml-get-children xml-data 'error))))
    (insert
     (jabber-propertize
      (concat "Error: " (jabber-parse-error the-error))
      'face 'jabber-chat-error))))
;; jabber-chat-print-error:1 ends here

;; [[file:jabber.org::#chat-print-subject][jabber-chat-print-subject:1]]
(defun jabber-chat-print-subject (xml-data who mode)
  "Print subject of given <message/>, if any.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((subject (car
		  (jabber-xml-node-children
		   (car
		    (jabber-xml-get-children xml-data 'subject))))))
    (when (not (zerop (length subject)))
      (cl-case mode
	(:printp
	 t)
	(:insert
	 (insert (jabber-propertize
		  "Subject: " 'face 'jabber-chat-prompt-system)
		 (jabber-propertize
		  subject
		  'face 'jabber-chat-text-foreign)
		 "\n"))))))
;; jabber-chat-print-subject:1 ends here

;; [[file:jabber.org::#chat-print-body][jabber-chat-print-body:1]]
(defun jabber-chat-print-body (xml-data who mode)
  (run-hook-with-args-until-success 'jabber-body-printers xml-data who mode))
;; jabber-chat-print-body:1 ends here

;; [[file:jabber.org::#chat-normal-body][jabber-chat-normal-body:1]]
(defun jabber-chat-normal-body (xml-data who mode)
  "Print body for received message in XML-DATA."
  (let ((body (car
	       (jabber-xml-node-children
		(car
		 (jabber-xml-get-children xml-data 'body))))))
    (when body

      (when (eql mode :insert)
	(if (and (> (length body) 4)
		 (string= (substring body 0 4) "/me "))
	    (let ((action (substring body 4))
		  (nick (cond
			 ((eq who :local)
			  (plist-get (fsm-get-state-data jabber-buffer-connection) :username))
			 ((or (jabber-muc-message-p xml-data)
			      (jabber-muc-private-message-p xml-data))
			  (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
			 (t
			  (jabber-jid-displayname (jabber-xml-get-attribute xml-data 'from))))))
	      (insert (jabber-propertize
		       (concat nick
			       " "
			       action)
		       'face 'jabber-chat-prompt-system)))
	  (insert (jabber-propertize
		   body
		   'face (cl-case who
			   ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
			   ((:local :muc-local) 'jabber-chat-text-local))))))
      t)))
;; jabber-chat-normal-body:1 ends here

;; [[file:jabber.org::#chat-print-url][jabber-chat-print-url:1]]
(defun jabber-chat-print-url (xml-data who mode)
  "Print URLs provided in jabber:x:oob namespace.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((foundp nil))
    (dolist (x (jabber-xml-node-children xml-data))
      (when (and (listp x) (eq (jabber-xml-node-name x) 'x)
		 (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:oob"))
	(setq foundp t)

	(when (eql mode :insert)
	  (let ((url (car (jabber-xml-node-children
			   (car (jabber-xml-get-children x 'url)))))
		(desc (car (jabber-xml-node-children
			    (car (jabber-xml-get-children x 'desc))))))
	    (insert "\n"
                    (jabber-propertize
		     "URL: " 'face 'jabber-chat-prompt-system)
                    (format "%s <%s>" desc url))))))
    foundp))
;; jabber-chat-print-url:1 ends here

;; [[file:jabber.org::#chat-goto-address][jabber-chat-goto-address:1]]
(defun jabber-chat-goto-address (xml-data who mode)
  "Call `goto-address' on the newly written text.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (when (eq mode :insert)
    (ignore-errors
      (let ((end (point))
	    (limit (max (- (point) 1000) (1+ (point-min)))))
	;; We only need to fontify the text written since the last
	;; prompt.  The prompt has a field property, so we can find it
	;; using `field-beginning'.
	(goto-address-fontify (field-beginning nil nil limit) end)))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Compose message" 'jabber-compose))
;; jabber-chat-goto-address:1 ends here

;; [[file:jabber.org::#send-message][jabber-send-message:1]]
(defun jabber-send-message (jc to subject body type)
  "Send a message tag to the server.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "subject: ")
		     (jabber-read-with-input-method "body: ")
		     (read-string "type: ")))
  (jabber-send-sexp jc
		    `(message ((to . ,to)
                               ,(if (> (length type) 0)
                                    `(type . ,type)))
                              ,(if (> (length subject) 0)
                                   `(subject () ,subject))
                              ,(if (> (length body) 0)
                                   `(body () ,body))))
  (if (and jabber-history-enabled (not (string= type "groupchat")))
      (jabber-history-log-message "out" nil to body (current-time))))
;; jabber-send-message:1 ends here

;; [[file:jabber.org::#send-message][jabber-send-message:2]]
(add-to-list 'jabber-jid-chat-menu
	     (cons "Start chat" 'jabber-chat-with))
;; jabber-send-message:2 ends here

;; [[file:jabber.org::#chat-][jabber-chat-with:1]]
(defun jabber-chat-with (jc jid &optional other-window)
  "Open an empty chat window for chatting with JID.
With a prefix argument, open buffer in other window.
Returns the chat buffer.
JC is the Jabber connection."
  (interactive (let* ((jid
		      (jabber-read-jid-completing "chat with:"))
		      (account
		       (jabber-read-account nil jid)))
		 (list
		  account jid current-prefix-arg)))
  (let ((buffer (jabber-chat-create-buffer jc jid)))
    (if other-window
	(switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))
;; jabber-chat-with:1 ends here

;; [[file:jabber.org::#chat-jid-at-point][jabber-chat-with-jid-at-point:1]]
(defun jabber-chat-with-jid-at-point (&optional other-window)
  "Start chat with JID at point.
Signal an error if there is no JID at point.
With a prefix argument, open buffer in other window."
  (interactive "P")
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid))
	(account (get-text-property (point)
				    'jabber-account)))
    (if (and jid-at-point account)
	(jabber-chat-with account jid-at-point other-window)
      (error "No contact at point"))))
;; jabber-chat-with-jid-at-point:1 ends here

;; [[file:jabber.org::#presence-element-functions][jabber-presence-element-functions:1]]
(defvar jabber-presence-element-functions nil
  "List of functions returning extra elements for <presence/> stanzas.
Each function takes one argument, the connection, and returns a
possibly empty list of extra child element of the <presence/>
stanza.")
;; jabber-presence-element-functions:1 ends here

;; [[file:jabber.org::#presence-history][jabber-presence-history:1]]
(defvar jabber-presence-history ()
  "Keeps track of previously used presence status types.")
;; jabber-presence-history:1 ends here

;; [[file:jabber.org::#process-roster][jabber-process-roster:1]]
(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "jabber:iq:roster" (function (lambda (jc x) (jabber-process-roster jc x nil)))))
(defun jabber-process-roster (jc xml-data closure-data)
  "Process an incoming roster infoquery result.
CLOSURE-DATA should be 'initial if initial roster push, nil otherwise.
JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((roster (plist-get (fsm-get-state-data jc) :roster))
	(from (jabber-xml-get-attribute xml-data 'from))
	(type (jabber-xml-get-attribute xml-data 'type))
	(id (jabber-xml-get-attribute xml-data 'id))
	(username (plist-get (fsm-get-state-data jc) :username))
	(server (plist-get (fsm-get-state-data jc) :server))
	(resource (plist-get (fsm-get-state-data jc) :resource))
	new-items changed-items deleted-items)
    ;; Perform sanity check on "from" attribute: it should be either absent
    ;; match our own JID, or match the server's JID (the latter is what
    ;; Facebook does).
    (if (not (or (null from)
		 (string= from server)
		 (string= from (concat username "@" server))
		 (string= from (concat username "@" server "/" resource))))
	(message "Roster push with invalid \"from\": \"%s\" (expected \"%s\", \"%s@%s\" or \"%s@%s/%s\")"
		 from
		 server username server username server resource)

      (dolist (item (jabber-xml-get-children (car (jabber-xml-get-children xml-data 'query)) 'item))
	(let (roster-item
	      (jid (jabber-jid-symbol (jabber-xml-get-attribute item 'jid))))

	  ;; If subscripton="remove", contact is to be removed from roster
	  (if (string= (jabber-xml-get-attribute item 'subscription) "remove")
	      (progn
		(if (jabber-jid-rostername jid)
		    (message "%s (%s) removed from roster" (jabber-jid-rostername jid) jid)
		  (message "%s removed from roster" jid))
		(push jid deleted-items))

	    ;; Find contact if already in roster
	    (setq roster-item (car (memq jid roster)))

	    (if roster-item
		(push roster-item changed-items)
	      ;; If not found, create a new roster item.
	      (unless (eq closure-data 'initial)
		(if (jabber-xml-get-attribute item 'name)
		    (message "%s (%s) added to roster" (jabber-xml-get-attribute item 'name) jid)
		  (message "%s added to roster" jid)))
	      (setq roster-item jid)
	      (push roster-item new-items))

	    ;; If this is an initial push, we want to forget
	    ;; everything we knew about this contact before - e.g. if
	    ;; the contact was online when we disconnected and offline
	    ;; when we reconnect, we don't want to see stale presence
	    ;; information.  This assumes that no contacts are shared
	    ;; between accounts.
	    (when (eq closure-data 'initial)
	      (setplist roster-item nil))

	    ;; Now, get all data associated with the contact.
	    (put roster-item 'name (jabber-xml-get-attribute item 'name))
	    (put roster-item 'subscription (jabber-xml-get-attribute item 'subscription))
	    (put roster-item 'ask (jabber-xml-get-attribute item 'ask))

	    ;; Since roster items can't be changed incrementally, we
	    ;; save the original XML to be able to modify it, instead of
	    ;; having to reproduce it.  This is for forwards
	    ;; compatibility.
	    (put roster-item 'xml item)

	    (put roster-item 'groups
		 (mapcar (lambda (foo) (nth 2 foo))
			 (jabber-xml-get-children item 'group)))))))
    ;; This is the function that does the actual updating and
    ;; redrawing of the roster.
    (jabber-roster-update jc new-items changed-items deleted-items)

    (if (and id (string= type "set"))
	(jabber-send-iq jc nil "result" nil
			nil nil nil nil id)))

  ;; After initial roster push, run jabber-post-connect-hooks.  We do
  ;; it here and not before since we want to have the entire roster
  ;; before we receive any presence stanzas.
  (when (eq closure-data 'initial)
    (run-hook-with-args 'jabber-post-connect-hooks jc)))
;; jabber-process-roster:1 ends here

;; [[file:jabber.org::#initial-roster-failure][jabber-initial-roster-failure:1]]
(defun jabber-initial-roster-failure (jc xml-data _closure-data)
  "Report the initial roster failure.
If the initial roster request fails, let's report it, but run
`jabber-post-connect-hooks' anyway.  According to the spec, there
is nothing exceptional about the server not returning a roster.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (jabber-report-success jc xml-data "Initial roster retrieval")
  (run-hook-with-args 'jabber-post-connect-hooks jc))
;; jabber-initial-roster-failure:1 ends here

;; [[file:jabber.org::#process-presence][jabber-process-presence:1]]
(add-to-list 'jabber-presence-chain 'jabber-process-presence)
(defun jabber-process-presence (jc xml-data)
  "Process incoming presence tags.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  ;; XXX: use JC argument
  (let ((roster (plist-get (fsm-get-state-data jc) :roster))
	(from (jabber-xml-get-attribute xml-data 'from))
	(to (jabber-xml-get-attribute xml-data 'to))
	(type (jabber-xml-get-attribute xml-data 'type))
	(presence-show (car (jabber-xml-node-children
			     (car (jabber-xml-get-children xml-data 'show)))))
	(presence-status (car (jabber-xml-node-children
			       (car (jabber-xml-get-children xml-data 'status)))))
	(error (car (jabber-xml-get-children xml-data 'error)))
	(priority (string-to-number (or (car (jabber-xml-node-children (car (jabber-xml-get-children xml-data 'priority))))
					"0"))))
    (cond
     ((string= type "subscribe")
      (run-with-idle-timer 0.01 nil #'jabber-process-subscription-request jc from presence-status))

     ((jabber-muc-presence-p xml-data)
      (jabber-muc-process-presence jc xml-data))

     (t
      ;; XXX: Think about what to do about out-of-roster presences.
      (let ((buddy (jabber-jid-symbol from)))
	(if (memq buddy roster)
	    (let* ((oldstatus (get buddy 'show))
		   (resource (or (jabber-jid-resource from) ""))
		   (resource-plist (cdr (assoc resource
					       (get buddy 'resources))))
		   newstatus)
	      (cond
	       ((and (string= resource "") (member type '("unavailable" "error")))
		;; 'unavailable' or 'error' from bare JID means that all resources
		;; are offline.
		(setq resource-plist nil)
		(setq newstatus (if (string= type "error") "error" nil))
		(let ((new-message (if error
				       (jabber-parse-error error)
				     presence-status)))
		  ;; erase any previous information
		  (put buddy 'resources nil)
		  (put buddy 'connected nil)
		  (put buddy 'show newstatus)
		  (put buddy 'status new-message)))

	       ((string= type "unavailable")
		(setq resource-plist
		      (plist-put resource-plist 'connected nil))
		(setq resource-plist
		      (plist-put resource-plist 'show nil))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 presence-status)))

	       ((string= type "error")
		(setq newstatus "error")
		(setq resource-plist
		      (plist-put resource-plist 'connected nil))
		(setq resource-plist
		      (plist-put resource-plist 'show "error"))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 (if error
				     (jabber-parse-error error)
				   presence-status))))
	       ((or
		 (string= type "unsubscribe")
		 (string= type "subscribed")
		 (string= type "unsubscribed"))
		;; Do nothing, except letting the user know.  The Jabber protocol
		;; places all this complexity on the server.
		(setq newstatus type))
	       (t
		(setq resource-plist
		      (plist-put resource-plist 'connected t))
		(setq resource-plist
		      (plist-put resource-plist 'show (or presence-show "")))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 presence-status))
		(setq resource-plist
		      (plist-put resource-plist 'priority priority))
		(setq newstatus (or presence-show ""))))

	      (when resource-plist
		;; this is for `assoc-set!' in guile
		(if (assoc resource (get buddy 'resources))
		    (setcdr (assoc resource (get buddy 'resources)) resource-plist)
		  (put buddy 'resources (cons (cons resource resource-plist) (get buddy 'resources))))
		(jabber-prioritize-resources buddy))

	      (fsm-send jc (cons :roster-update buddy))

	      (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
		(run-hook-with-args hook
				    buddy
				    oldstatus
				    newstatus
				    (plist-get resource-plist 'status)
				    (funcall jabber-alert-presence-message-function
					     buddy
					     oldstatus
					     newstatus
					     (plist-get resource-plist 'status)))))))))))
;; jabber-process-presence:1 ends here

;; [[file:jabber.org::#process-subscription-request][jabber-process-subscription-request:1]]
(defun jabber-process-subscription-request (jc from presence-status)
  "Process an incoming subscription request.
JC is the Jabber connection."
  (with-current-buffer (jabber-chat-create-buffer jc from)
    (ewoc-enter-last jabber-chat-ewoc (list :subscription-request presence-status :time (current-time)))

    (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
      (run-hook-with-args hook (jabber-jid-symbol from) nil "subscribe" presence-status (funcall jabber-alert-presence-message-function (jabber-jid-symbol from) nil "subscribe" presence-status)))))
;; jabber-process-subscription-request:1 ends here

;; [[file:jabber.org::#subscription-accept-mutual][jabber-subscription-accept-mutual:1]]
(defun jabber-subscription-accept-mutual (&rest ignored)
  (message "Subscription accepted; reciprocal subscription request sent")
  (jabber-subscription-reply "subscribed" "subscribe"))
;; jabber-subscription-accept-mutual:1 ends here

;; [[file:jabber.org::#subscription-accept-one-way][jabber-subscription-accept-one-way:1]]
(defun jabber-subscription-accept-one-way (&rest ignored)
  (message "Subscription accepted")
  (jabber-subscription-reply "subscribed"))
;; jabber-subscription-accept-one-way:1 ends here

;; [[file:jabber.org::#subscription-decline][jabber-subscription-decline:1]]
(defun jabber-subscription-decline (&rest ignored)
  (message "Subscription declined")
  (jabber-subscription-reply "unsubscribed"))
;; jabber-subscription-decline:1 ends here

;; [[file:jabber.org::#subscription-reply][jabber-subscription-reply:1]]
(defun jabber-subscription-reply (&rest types)
  (let ((to (jabber-jid-user jabber-chatting-with)))
    (dolist (type types)
      (jabber-send-sexp jabber-buffer-connection `(presence ((to . ,to) (type . ,type)))))))
;; jabber-subscription-reply:1 ends here

;; [[file:jabber.org::#prioritize-resources][jabber-prioritize-resources:1]]
(defun jabber-prioritize-resources (buddy)
  "Set connected, show and status properties for BUDDY.
Show status properties from highest-priority resource."
  (let ((resource-alist (get buddy 'resources))
	(highest-priority nil))
    ;; Reset to nil at first, for cases (a) resource-alist is nil
    ;; and (b) all resources are disconnected.
    (put buddy 'connected nil)
    (put buddy 'show nil)
    (put buddy 'status nil)
    (mapc #'(lambda (resource)
	      (let* ((resource-plist (cdr resource))
		     (priority (plist-get resource-plist 'priority)))
		(if (plist-get resource-plist 'connected)
		    (when (or (null highest-priority)
			      (and priority
				   (> priority highest-priority)))
		      ;; if no priority specified, interpret as zero
		      (setq highest-priority (or priority 0))
		      (put buddy 'connected (plist-get resource-plist 'connected))
		      (put buddy 'show (plist-get resource-plist 'show))
		      (put buddy 'status (plist-get resource-plist 'status))
		      (put buddy 'resource (car resource)))

		  ;; if we have not found a connected resource yet, but this
		  ;; disconnected resource has a status message, display it.
		  (when (not (get buddy 'connected))
		    (if (plist-get resource-plist 'status)
			(put buddy 'status (plist-get resource-plist 'status)))
		    (if (plist-get resource-plist 'show)
			(put buddy 'show (plist-get resource-plist 'show)))))))
	  resource-alist)))
;; jabber-prioritize-resources:1 ends here

;; [[file:jabber.org::#count-connected-resources][jabber-count-connected-resources:1]]
(defun jabber-count-connected-resources (buddy)
  "Return the number of connected resources for BUDDY."
  (let ((resource-alist (get buddy 'resources))
	(count 0))
    (dolist (resource resource-alist)
      (if (plist-get (cdr resource) 'connected)
	  (setq count (1+ count))))
    count))
;; jabber-count-connected-resources:1 ends here

;; [[file:jabber.org::#send-presence][jabber-send-presence:1]]
;;;###autoload
(defun jabber-send-presence (show status priority)
  "Set presence for all accounts."
  (interactive
   (list
    (completing-read "show: " '("" "away" "xa" "dnd" "chat")
		     nil t nil 'jabber-presence-history)
    (jabber-read-with-input-method "status message: " *jabber-current-status*
				   '*jabber-status-history*)
    (read-string "priority: " (int-to-string (if *jabber-current-priority*
						 *jabber-current-priority*
					       jabber-default-priority)))))

  (setq *jabber-current-show* show *jabber-current-status* status)
  (setq *jabber-current-priority*
	(if (numberp priority) priority (string-to-number priority)))

  (let (subelements-map)
    ;; For each connection, we use a different set of subelements.  We
    ;; cache them, to only generate them once.

    ;; Ordinary presence, with no specified recipient
    (dolist (jc jabber-connections)
      (let ((subelements (jabber-presence-children jc)))
        (push (cons jc subelements) subelements-map)
	(jabber-send-sexp-if-connected jc `(presence () ,@subelements))))

    ;; Then send presence to groupchats
    (dolist (gc *jabber-active-groupchats*)
      (let* ((buffer (get-buffer (jabber-muc-get-buffer (car gc))))
	     (jc (when buffer
		   (buffer-local-value 'jabber-buffer-connection buffer)))
	     (subelements (cdr (assq jc subelements-map))))
	(when jc
	  (jabber-send-sexp-if-connected
	   jc `(presence ((to . ,(concat (car gc) "/" (cdr gc))))
			 ,@subelements))))))

  (jabber-display-roster))
;; jabber-send-presence:1 ends here

;; [[file:jabber.org::#presence-children][jabber-presence-children:1]]
(defun jabber-presence-children (jc)
  "Return the children for a <presence/> stanza.
JC is the Jabber connection."
  `(,(when (> (length *jabber-current-status*) 0)
       `(status () ,*jabber-current-status*))
    ,(when (> (length *jabber-current-show*) 0)
	 `(show () ,*jabber-current-show*))
    ,(when *jabber-current-priority*
       `(priority () ,(number-to-string *jabber-current-priority*)))
    ,@(apply 'append (mapcar (lambda (f)
			       (funcall f jc))
			     jabber-presence-element-functions))))
;; jabber-presence-children:1 ends here

;; [[file:jabber.org::#send-directed-presence][jabber-send-directed-presence:1]]
(defun jabber-send-directed-presence (jc jid type)
  "Send a directed presence stanza to JID.
TYPE is one of:
\"online\", \"away\", \"xa\", \"dnd\", \"chatty\":
  Appear as present with the given status.
\"unavailable\":
  Appear as offline.
\"probe\":
  Ask the contact's server for updated presence.
\"subscribe\":
  Ask for subscription to contact's presence.
  (see also `jabber-send-subscription-request')
\"unsubscribe\":
  Cancel your subscription to contact's presence.
\"subscribed\":
  Accept contact's request for presence subscription.
  (this is usually done within a chat buffer)
\"unsubscribed\":
  Cancel contact's subscription to your presence.

JC is the Jabber connection."
  (interactive
   (list (jabber-read-account)
	 (jabber-read-jid-completing "Send directed presence to: ")
	 (completing-read "Type (default is online): "
			  '(("online")
			    ("away")
			    ("xa")
			    ("dnd")
			    ("chatty")
			    ("probe")
			    ("unavailable")
			    ("subscribe")
			    ("unsubscribe")
			    ("subscribed")
			    ("unsubscribed"))
			  nil t nil 'jabber-presence-history "online")))
  (cond
   ((member type '("probe" "unavailable"
		   "subscribe" "unsubscribe"
		   "subscribed" "unsubscribed"))
    (jabber-send-sexp jc `(presence ((to . ,jid)
				     (type . ,type)))))

   (t
    (let ((*jabber-current-show*
	   (if (string= type "online")
	       ""
	     type))
	  (*jabber-current-status* nil))
      (jabber-send-sexp jc `(presence ((to . ,jid))
				      ,@(jabber-presence-children jc)))))))
;; jabber-send-directed-presence:1 ends here

;; [[file:jabber.org::#send-away-presence][jabber-send-away-presence:1]]
(defun jabber-send-away-presence (&optional status)
  "Set status to away.
With prefix argument, ask for status message."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " *jabber-current-status* '*jabber-status-history*))))
  (jabber-send-presence "away" (if status status *jabber-current-status*)
			*jabber-current-priority*))
;; jabber-send-away-presence:1 ends here

;; [[file:jabber.org::#send-xa-presence][jabber-send-xa-presence:1]]
;; XXX code duplication!
(defun jabber-send-xa-presence (&optional status)
  "Send extended away presence.
With prefix argument, ask for status message."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " *jabber-current-status* '*jabber-status-history*))))
  (jabber-send-presence "xa" (if status status *jabber-current-status*)
			*jabber-current-priority*))
;; jabber-send-xa-presence:1 ends here

;; [[file:jabber.org::#send-default-presence][jabber-send-default-presence:1]]
;;;###autoload
(defun jabber-send-default-presence (&optional ignore)
  "Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'."
  (interactive)
  (jabber-send-presence
   jabber-default-show jabber-default-status jabber-default-priority))
;; jabber-send-default-presence:1 ends here

;; [[file:jabber.org::#send-current-presence][jabber-send-current-presence:1]]
(defun jabber-send-current-presence (&optional ignore)
  "(Re-)send current presence.
That is, if presence has already been sent, use current settings,
otherwise send defaults (see `jabber-send-default-presence')."
  (interactive)
  (if *jabber-current-show*
      (jabber-send-presence *jabber-current-show* *jabber-current-status*
			    *jabber-current-priority*)
    (jabber-send-default-presence)))
;; jabber-send-current-presence:1 ends here

;; [[file:jabber.org::#send-subscription-request][jabber-send-subscription-request:1]]
(add-to-list 'jabber-jid-roster-menu (cons "Send subscription request"
					   'jabber-send-subscription-request))
(defun jabber-send-subscription-request (jc to &optional request)
  "Send a subscription request to jid.
Show him your request text, if specified.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "request: ")))
  (jabber-send-sexp jc
		    `(presence
		      ((to . ,to)
		       (type . "subscribe"))
		      ,@(when (and request (> (length request) 0))
			  (list `(status () ,request))))))
;; jabber-send-subscription-request:1 ends here

;; [[file:jabber.org::#roster-group-history][jabber-roster-group-history:1]]
(defvar jabber-roster-group-history nil
  "History of entered roster groups.")
;; jabber-roster-group-history:1 ends here

;; [[file:jabber.org::#roster-change][jabber-roster-change:1]]
(add-to-list 'jabber-jid-roster-menu
	     (cons "Add/modify roster entry" 'jabber-roster-change))
(defun jabber-roster-change (jc jid name groups)
  "Add or change a roster item.
JC is the Jabber connection."
  (interactive (let* ((jid (jabber-jid-symbol
			    (jabber-read-jid-completing "Add/change JID: ")))
		      (account (jabber-read-account))
		      (name (get jid 'name))
		      (groups (get jid 'groups))
		      (all-groups
		       (apply #'append
			      (mapcar
			       (lambda (j) (get j 'groups))
			       (plist-get (fsm-get-state-data account) :roster)))))
		 (when (string< emacs-version "22")
		   ;; Older emacsen want the completion table to be an alist...
		   (setq all-groups (mapcar #'list all-groups)))
		 (list account
		       jid (jabber-read-with-input-method (format "Name: (default `%s') " name) nil nil name)
		       (delete ""
			       (completing-read-multiple
				(format
				 "Groups, comma-separated: (default %s) "
				 (if groups
				     (mapconcat #'identity groups ",")
				   "none"))
				all-groups
				nil nil nil
				'jabber-roster-group-history
				(mapconcat #'identity groups ",")
				t)))))
  ;; If new fields are added to the roster XML structure in a future standard,
  ;; they will be clobbered by this function.
  ;; XXX: specify account
  (jabber-send-iq jc nil "set"
		  (list 'query (list (cons 'xmlns "jabber:iq:roster"))
				(append
				 (list 'item (append
				     (list (cons 'jid (symbol-name jid)))
				     (if (and name (> (length name) 0))
					 (list (cons 'name name)))))
				 (mapcar #'(lambda (x) `(group () ,x))
				      groups)))
		  #'jabber-report-success "Roster item change"
		  #'jabber-report-success "Roster item change"))
;; jabber-roster-change:1 ends here

;; [[file:jabber.org::#roster-delete][jabber-roster-delete:1]]
(add-to-list 'jabber-jid-roster-menu
	     (cons "Delete roster entry" 'jabber-roster-delete))
(defun jabber-roster-delete (jc jid)
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Delete from roster: ")))
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . "jabber:iq:roster"))
			  (item ((jid . ,jid)
				 (subscription . "remove"))))
		  #'jabber-report-success "Roster item removal"
		  #'jabber-report-success "Roster item removal"))
;; jabber-roster-delete:1 ends here

;; [[file:jabber.org::#roster-delete-jid-at-point][jabber-roster-delete-jid-at-point:1]]
(defun jabber-roster-delete-jid-at-point ()
  "Delete JID at point from roster.
Signal an error if there is no JID at point."
  (interactive)
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid))
	(account (get-text-property (point) 'jabber-account)))
    (if (and jid-at-point account
	     (or jabber-silent-mode (yes-or-no-p (format "Really delete %s from roster? " jid-at-point))))
	(jabber-roster-delete account jid-at-point)
      (error "No contact at point"))))
;; jabber-roster-delete-jid-at-point:1 ends here

;; [[file:jabber.org::#roster-delete-group-from-jids][jabber-roster-delete-group-from-jids:1]]
(defun jabber-roster-delete-group-from-jids (jc jids group)
  "Delete group `group' from all JIDs.
JC is the Jabber connection."
  (interactive)
  (dolist (jid jids)
    (jabber-roster-change
     jc jid (get jid 'name)
     (cl-remove-if-not (lambda (g) (not (string= g group)))
		    (get jid 'groups)))))
;; jabber-roster-delete-group-from-jids:1 ends here

;; [[file:jabber.org::#roster-edit-group-from-jids][jabber-roster-edit-group-from-jids:1]]
(defun jabber-roster-edit-group-from-jids (jc jids group)
  "Edit group `group' from all JIDs.
JC is the Jabber connection."
  (interactive)
  (let ((new-group
	 (jabber-read-with-input-method
	  (format "New group: (default `%s') " group) nil nil group)))
    (dolist (jid jids)
      (jabber-roster-change
       jc jid (get jid 'name)
       (cl-remove-duplicates
	(mapcar
	 (lambda (g) (if (string= g group)
			 new-group
		       g))
	 (get jid 'groups))
	:test 'string=)))))
;; jabber-roster-edit-group-from-jids:1 ends here

;; [[file:jabber.org::#entity-capabilities-()][Entity Capabilities ([[https://xmpp.org/extensions/xep-0115.html][XEP-0115]]):1]]
;;;###autoload
(eval-after-load "jabber-core"
  '(add-to-list 'jabber-presence-chain #'jabber-process-caps))
;; Entity Capabilities ([[https://xmpp.org/extensions/xep-0115.html][XEP-0115]]):1 ends here

;; [[file:jabber.org::#caps-cache][jabber-caps-cache:1]]
(defvar jabber-caps-cache (make-hash-table :test 'equal))
;; jabber-caps-cache:1 ends here

;; [[file:jabber.org::#caps-hash-names][jabber-caps-hash-names:1]]
(defconst jabber-caps-hash-names
  (if (fboundp 'secure-hash)
      '(("sha-1" . sha1)
	("sha-224" . sha224)
	("sha-256" . sha256)
	("sha-384" . sha384)
	("sha-512" . sha512))
    ;; `secure-hash' was introduced in Emacs 24.  For Emacs 23, fall
    ;; back to the `sha1' function, handled specially in
    ;; `jabber-caps--secure-hash'.
    '(("sha-1" . sha1)))
  "Hash function name map.
Maps names defined in http://www.iana.org/assignments/hash-function-text-names
to symbols accepted by `secure-hash'.

XEP-0115 currently recommends SHA-1, but let's be future-proof.")
;; jabber-caps-hash-names:1 ends here

;; [[file:jabber.org::#caps-get-cached][jabber-caps-get-cached:1]]
(defun jabber-caps-get-cached (jid)
  "Get disco info from Entity Capabilities cache.
JID should be a string containing a full JID.
Return (IDENTITIES FEATURES), or nil if not in cache."
  (let* ((symbol (jabber-jid-symbol jid))
	 (resource (or (jabber-jid-resource jid) ""))
	 (resource-plist (cdr (assoc resource (get symbol 'resources))))
	 (key (plist-get resource-plist 'caps)))
    (when key
      (let ((cache-entry (gethash key jabber-caps-cache)))
	(when (and (consp cache-entry) (not (floatp (car cache-entry))))
	  cache-entry)))))
;; jabber-caps-get-cached:1 ends here

;; [[file:jabber.org::#process-caps][jabber-process-caps:1]]
;;;###autoload
(defun jabber-process-caps (jc xml-data)
  "Look for entity capabilities in presence stanzas.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (type (jabber-xml-get-attribute xml-data 'type))
	 (c (jabber-xml-path xml-data '(("http://jabber.org/protocol/caps" . "c")))))
    (when (and (null type) c)
      (jabber-xml-let-attributes
	  (ext hash node ver) c
	(cond
	 (hash
	  ;; If the <c/> element has a hash attribute, it follows the
	  ;; "modern" version of XEP-0115.
	  (jabber-process-caps-modern jc from hash node ver))
	 (t
	  ;; No hash attribute.  Use legacy version of XEP-0115.
	  ;; TODO: do something clever here.
	  ))))))
;; jabber-process-caps:1 ends here

;; [[file:jabber.org::#process-caps-modern][jabber-process-caps-modern:1]]
(defun jabber-process-caps-modern (jc jid hash node ver)
  (when (assoc hash jabber-caps-hash-names)
    ;; We support the hash function used.
    (let* ((key (cons hash ver))
	   (cache-entry (gethash key jabber-caps-cache)))
      ;; Remember the hash in the JID symbol.
      (let* ((symbol (jabber-jid-symbol jid))
	     (resource (or (jabber-jid-resource jid) ""))
	     (resource-entry (assoc resource (get symbol 'resources)))
	     (new-resource-plist (plist-put (cdr resource-entry) 'caps key)))
	(if resource-entry
	    (setf (cdr resource-entry) new-resource-plist)
	  (push (cons resource new-resource-plist) (get symbol 'resources))))

      (cl-flet ((request-disco-info
	      ()
	      (jabber-send-iq
	       jc jid
	       "get"
	       `(query ((xmlns . "http://jabber.org/protocol/disco#info")
			(node . ,(concat node "#" ver))))
	       #'jabber-process-caps-info-result (list hash node ver)
	       #'jabber-process-caps-info-error (list hash node ver))))
	(cond
	 ((and (consp cache-entry)
	       (floatp (car cache-entry)))
	  ;; We have a record of asking someone about this hash.
	  (if (< (- (float-time) (car cache-entry)) 10.0)
	      ;; We asked someone about this hash less than 10 seconds ago.
	      ;; Let's add the new JID to the entry, just in case that
	      ;; doesn't work out.
	      (cl-pushnew jid (cdr cache-entry) :test #'string=)
	    ;; We asked someone about it more than 10 seconds ago.
	    ;; They're probably not going to answer.  Let's ask
	    ;; this contact about it instead.
	    (setf (car cache-entry) (float-time))
	    (request-disco-info)))
	 ((null cache-entry)
	  ;; We know nothing about this hash.  Let's note the
	  ;; fact that we tried to get information about it.
	  (puthash key (list (float-time)) jabber-caps-cache)
	  (request-disco-info))
	 (t
	  ;; We already know what this hash represents, so we
	  ;; can cache info for this contact.
	  (puthash (cons jid nil) cache-entry jabber-disco-info-cache)))))))
;; jabber-process-caps-modern:1 ends here

;; [[file:jabber.org::#process-caps-info-result][jabber-process-caps-info-result:1]]
(defun jabber-process-caps-info-result (jc xml-data closure-data)
  (cl-destructuring-bind (hash node ver) closure-data
    (let* ((key (cons hash ver))
	   (query (jabber-iq-query xml-data))
	   (verification-string (jabber-caps-ver-string query hash)))
      (if (string= ver verification-string)
	  ;; The hash is correct; save info.
	  (puthash key (jabber-disco-parse-info xml-data) jabber-caps-cache)
	;; The hash is incorrect.
	(jabber-caps-try-next jc hash node ver)))))
;; jabber-process-caps-info-result:1 ends here

;; [[file:jabber.org::#process-caps-info-error][jabber-process-caps-info-error:1]]
(defun jabber-process-caps-info-error (jc xml-data closure-data)
  (cl-destructuring-bind (hash node ver) closure-data
    (jabber-caps-try-next jc hash node ver)))
;; jabber-process-caps-info-error:1 ends here

;; [[file:jabber.org::#caps-try-next][jabber-caps-try-next:1]]
(defun jabber-caps-try-next (jc hash node ver)
  (let* ((key (cons hash ver))
	 (cache-entry (gethash key jabber-caps-cache)))
    (when (floatp (car-safe cache-entry))
      (let ((next-jid (pop (cdr cache-entry))))
	;; Do we know someone else we could ask about this hash?
	(if next-jid
	    (progn
	      (setf (car cache-entry) (float-time))
	      (jabber-send-iq
	       jc next-jid
	       "get"
	       `(query ((xmlns . "http://jabber.org/protocol/disco#info")
			(node . ,(concat node "#" ver))))
	       #'jabber-process-caps-info-result (list hash node ver)
	       #'jabber-process-caps-info-error (list hash node ver)))
	  ;; No, forget about it for now.
	  (remhash key jabber-caps-cache))))))
;; jabber-caps-try-next:1 ends here

;; [[file:jabber.org::#caps-ver-string][jabber-caps-ver-string:1]]
(defun jabber-caps-ver-string (query hash)
  ;; XEP-0115, section 5.1
  ;; 1. Initialize an empty string S.
  (with-temp-buffer
    (let* ((identities (jabber-xml-get-children query 'identity))
	   (disco-features (mapcar (lambda (f) (jabber-xml-get-attribute f 'var))
			     (jabber-xml-get-children query 'feature)))
	   (maybe-forms (jabber-xml-get-children query 'x))
	   (forms (cl-remove-if-not
		   (lambda (x)
		     ;; Keep elements that are forms and have a FORM_TYPE,
		     ;; according to XEP-0128.
		     (and (string= (jabber-xml-get-xmlns x) "jabber:x:data")
			  (jabber-xdata-formtype x)))
		   maybe-forms)))
      ;; 2. Sort the service discovery identities [15] by category
      ;; and then by type and then by xml:lang (if it exists),
      ;; formatted as CATEGORY '/' [TYPE] '/' [LANG] '/'
      ;; [NAME]. [16] Note that each slash is included even if the
      ;; LANG or NAME is not included (in accordance with XEP-0030,
      ;; the category and type MUST be included.
      (setq identities (sort identities #'jabber-caps-identity-<))
      ;; 3. For each identity, append the 'category/type/lang/name' to
      ;; S, followed by the '<' character.
      (dolist (identity identities)
	(jabber-xml-let-attributes (category type xml:lang name) identity
	  ;; Use `concat' here instead of passing everything to
	  ;; `insert', since `concat' tolerates nil values.
	  (insert (concat category "/" type "/" xml:lang "/" name "<"))))
      ;; 4. Sort the supported service discovery features. [17]
      (setq disco-features (sort disco-features #'string<))
      ;; 5. For each feature, append the feature to S, followed by the
      ;; '<' character.
      (dolist (f disco-features)
	(insert f "<"))
      ;; 6. If the service discovery information response includes
      ;; XEP-0128 data forms, sort the forms by the FORM_TYPE (i.e.,
      ;; by the XML character data of the <value/> element).
      (setq forms (sort forms (lambda (a b)
				(string< (jabber-xdata-formtype a)
					 (jabber-xdata-formtype b)))))
      ;; 7. For each extended service discovery information form:
      (dolist (form forms)
	;; Append the XML character data of the FORM_TYPE field's
	;; <value/> element, followed by the '<' character.
	(insert (jabber-xdata-formtype form) "<")
	;; Sort the fields by the value of the "var" attribute.
	(let ((fields (sort (jabber-xml-get-children form 'field)
			    (lambda (a b)
			      (string< (jabber-xml-get-attribute a 'var)
				       (jabber-xml-get-attribute b 'var))))))
	  (dolist (field fields)
	    ;; For each field other than FORM_TYPE:
	    (unless (string= (jabber-xml-get-attribute field 'var) "FORM_TYPE")
	      ;; Append the value of the "var" attribute, followed by the '<' character.
	      (insert (jabber-xml-get-attribute field 'var) "<")
	      ;; Sort values by the XML character data of the <value/> element.
	      (let ((values (sort (mapcar (lambda (value)
					    (car (jabber-xml-node-children value)))
					  (jabber-xml-get-children field 'value))
				  #'string<)))
		;; For each <value/> element, append the XML character
		;; data, followed by the '<' character.
		(dolist (value values)
		  (insert value "<"))))))))

    ;; 8. Ensure that S is encoded according to the UTF-8 encoding
    ;; (RFC 3269 [18]).
    (let ((s (encode-coding-string (buffer-string) 'utf-8 t))
	  (algorithm (cdr (assoc hash jabber-caps-hash-names))))
      ;; 9. Compute the verification string by hashing S using the
      ;; algorithm specified in the 'hash' attribute (e.g., SHA-1 as
      ;; defined in RFC 3174 [19]). The hashed data MUST be generated
      ;; with binary output and encoded using Base64 as specified in
      ;; Section 4 of RFC 4648 [20] (note: the Base64 output MUST NOT
      ;; include whitespace and MUST set padding bits to zero). [21]
      (base64-encode-string (jabber-caps--secure-hash algorithm s) t))))
;; jabber-caps-ver-string:1 ends here

;; [[file:jabber.org::#caps-secure-hash][jabber-caps--secure-hash:1]]
(defun jabber-caps--secure-hash (algorithm string)
  (cond
   ;; `secure-hash' was introduced in Emacs 24
   ((fboundp 'secure-hash)
    (secure-hash algorithm string nil nil t))
   ((eq algorithm 'sha1)
    ;; For SHA-1, we can use the `sha1' function.
    (sha1 string nil nil t))
   (t
    (error "Cannot use hash algorithm %s!" algorithm))))
;; jabber-caps--secure-hash:1 ends here

;; [[file:jabber.org::#caps-identity-<][jabber-caps-identity-<:1]]
(defun jabber-caps-identity-< (a b)
  (let ((a-category (jabber-xml-get-attribute a 'category))
	(b-category (jabber-xml-get-attribute b 'category)))
    (or (string< a-category b-category)
	(and (string= a-category b-category)
	     (let ((a-type (jabber-xml-get-attribute a 'type))
		   (b-type (jabber-xml-get-attribute b 'type)))
	       (or (string< a-type b-type)
		   (and (string= a-type b-type)
			(let ((a-xml:lang (jabber-xml-get-attribute a 'xml:lang))
			      (b-xml:lang (jabber-xml-get-attribute b 'xml:lang)))
			  (string< a-xml:lang b-xml:lang)))))))))
;; jabber-caps-identity-<:1 ends here

;; [[file:jabber.org::#caps-default-hash-function][jabber-caps-default-hash-function:1]]
(defvar jabber-caps-default-hash-function "sha-1"
  "Hash function to use when sending caps in presence stanzas.
The value should be a key in `jabber-caps-hash-names'.")
;; jabber-caps-default-hash-function:1 ends here

;; [[file:jabber.org::#caps-current-hash][jabber-caps-current-hash:1]]
(defvar jabber-caps-current-hash nil
  "The current disco hash we're sending out in presence stanzas.")
;; jabber-caps-current-hash:1 ends here

;; [[file:jabber.org::#caps-node][jabber-caps-node:1]]
(defconst jabber-caps-node "http://emacs-jabber.sourceforge.net")
;; jabber-caps-node:1 ends here

;; [[file:jabber.org::#disco-advertise-feature][jabber-disco-advertise-feature:1]]
;;;###autoload
(defun jabber-disco-advertise-feature (feature)
  (unless (member feature jabber-advertised-features)
    (push feature jabber-advertised-features)
    (when jabber-caps-current-hash
      (jabber-caps-recalculate-hash)
      ;; If we're already connected, we need to send updated presence
      ;; for the new feature.
      (mapc #'jabber-send-current-presence jabber-connections))))
;; jabber-disco-advertise-feature:1 ends here

;; [[file:jabber.org::#caps-recalculate-hash][jabber-caps-recalculate-hash:1]]
(defun jabber-caps-recalculate-hash ()
  "Update `jabber-caps-current-hash' for feature list change.
Also update `jabber-disco-info-nodes', so we return results for
the right node."
  (let* ((old-hash jabber-caps-current-hash)
	 (old-node (and old-hash (concat jabber-caps-node "#" old-hash)))
	 (new-hash
	  (jabber-caps-ver-string `(query () ,@(jabber-disco-return-client-info))
				  jabber-caps-default-hash-function))
	 (new-node (concat jabber-caps-node "#" new-hash)))
    (when old-node
      (let ((old-entry (assoc old-node jabber-disco-info-nodes)))
	(when old-entry
	  (setq jabber-disco-info-nodes (delq old-entry jabber-disco-info-nodes)))))
    (push (list new-node #'jabber-disco-return-client-info nil)
	  jabber-disco-info-nodes)
    (setq jabber-caps-current-hash new-hash)))
;; jabber-caps-recalculate-hash:1 ends here

;; [[file:jabber.org::#caps-presence-element][jabber-caps-presence-element:1]]
;;;###autoload
(defun jabber-caps-presence-element (_jc)
  (unless jabber-caps-current-hash
    (jabber-caps-recalculate-hash))

  (list
   `(c ((xmlns . "http://jabber.org/protocol/caps")
	(hash . ,jabber-caps-default-hash-function)
	(node . ,jabber-caps-node)
	(ver . ,jabber-caps-current-hash)))))

;;;###autoload
(eval-after-load "jabber-presence"
  '(add-to-list 'jabber-presence-element-functions #'jabber-caps-presence-element))
;; jabber-caps-presence-element:1 ends here

;; [[file:jabber.org::#advertised-features][jabber-advertised-features:1]]
(defvar jabber-advertised-features
  (list "http://jabber.org/protocol/disco#info")
  "Features advertised on service discovery requests.

Don't add your feature to this list directly.  Instead, call
`jabber-disco-advertise-feature'.")
;; jabber-advertised-features:1 ends here

;; [[file:jabber.org::#disco-items-nodes][jabber-disco-items-nodes:1]]
(defvar jabber-disco-items-nodes
  (list
   (list "" nil nil))
  "Alist of node names and information about returning disco item data.
Key is node name as a string, or \"\" for no node specified.  Value is
a list of two items.

First item is data to return.  If it is a function, that function is
called and its return value is used; if it is a list, that list is
used.  The list should be the XML data to be returned inside the
<query/> element, like this:

\((item ((name . \"Name of first item\")
	(jid . \"first.item\")
	(node . \"node\"))))

Second item is access control function.  That function is passed the
JID, and returns non-nil if access is granted.  If the second item is
nil, access is always granted.")
;; jabber-disco-items-nodes:1 ends here

;; [[file:jabber.org::#disco-info-nodes][jabber-disco-info-nodes:1]]
(defvar jabber-disco-info-nodes
  (list
   (list "" #'jabber-disco-return-client-info nil))
  "Alist of node names and information returning disco info data.
Key is node name as a string, or \"\" for no node specified.  Value is
a list of two items.

First item is data to return.  If it is a function, that function is
called and its return value is used; if it is a list, that list is
used.  The list should be the XML data to be returned inside the
<query/> element, like this:

\((identity ((category . \"client\")
	    (type . \"pc\")
	    (name . \"Jabber client\")))
 (feature ((var . \"some-feature\"))))

Second item is access control function.  That function is passed the
JID, and returns non-nil if access is granted.  If the second item is
nil, access is always granted.")
;; jabber-disco-info-nodes:1 ends here

;; [[file:jabber.org::#return-disco-info][jabber-return-disco-info:1]]
(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons "http://jabber.org/protocol/disco#info" 'jabber-return-disco-info))
(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons "http://jabber.org/protocol/disco#items" 'jabber-return-disco-info))
(defun jabber-return-disco-info (jc xml-data)
  "Respond to a service discovery request.
See XEP-0030.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((to (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (xmlns (jabber-iq-xmlns xml-data))
	 (which-alist (eval (cdr (assoc xmlns
					(list
					 (cons "http://jabber.org/protocol/disco#info" 'jabber-disco-info-nodes)
					 (cons "http://jabber.org/protocol/disco#items" 'jabber-disco-items-nodes))))))
	 (node (or
		(jabber-xml-get-attribute (jabber-iq-query xml-data) 'node)
		""))
	 (return-list (cdr (assoc node which-alist)))
	 (func (nth 0 return-list))
	 (access-control (nth 1 return-list)))
    (if return-list
	(if (and (functionp access-control)
		 (not (funcall access-control jc to)))
	    (jabber-signal-error "Cancel" 'not-allowed)
	  ;; Access control passed
	  (let ((result (if (functionp func)
			    (funcall func jc xml-data)
			  func)))
	    (jabber-send-iq jc to "result"
			    `(query ((xmlns . ,xmlns)
				     ,@(when node
					 (list (cons 'node node))))
				    ,@result)
			    nil nil nil nil id)))

      ;; No such node
      (jabber-signal-error "Cancel" 'item-not-found))))
;; jabber-return-disco-info:1 ends here

;; [[file:jabber.org::#disco-return-client-info][jabber-disco-return-client-info:1]]
(defun jabber-disco-return-client-info (&optional jc xml-data)
  `(
    ;; If running under a window system, this is
    ;; a GUI client.  If not, it is a console client.
    (identity ((category . "client")
	       (name . "Emacs Jabber client")
	       (type . ,(if (memq window-system
				  '(x w32 mac ns))
			    "pc"
			  "console"))))
    ,@(mapcar
       #'(lambda (featurename)
	   `(feature ((var . ,featurename))))
       jabber-advertised-features)))
;; jabber-disco-return-client-info:1 ends here

;; [[file:jabber.org::#get-disco-items][jabber-get-disco-items:1]]
(add-to-list 'jabber-jid-info-menu
	     (cons "Send items disco query" 'jabber-get-disco-items))
(defun jabber-get-disco-items (jc to &optional node)
  "Send a service discovery request for items.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Send items disco request to: " nil nil nil 'full t)
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq jc to
		  "get"
		  (list 'query (append (list (cons 'xmlns "http://jabber.org/protocol/disco#items"))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-items
		  #'jabber-process-data "Item discovery failed"))
;; jabber-get-disco-items:1 ends here

;; [[file:jabber.org::#get-disco-info][jabber-get-disco-info:1]]
(add-to-list 'jabber-jid-info-menu
	     (cons "Send info disco query" 'jabber-get-disco-info))
(defun jabber-get-disco-info (jc to &optional node)
  "Send a service discovery request for info.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Send info disco request to: " nil nil nil 'full t)
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq jc to
		  "get"
		  (list 'query (append (list (cons 'xmlns "http://jabber.org/protocol/disco#info"))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-info
		  #'jabber-process-data "Info discovery failed"))
;; jabber-get-disco-info:1 ends here

;; [[file:jabber.org::#process-disco-info][jabber-process-disco-info:1]]
(defun jabber-process-disco-info (jc xml-data)
  "Handle results from info disco requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let ((beginning (point)))
    (dolist (x (jabber-xml-node-children (jabber-iq-query xml-data)))
      (cond
       ((eq (jabber-xml-node-name x) 'identity)
	(let ((name (jabber-xml-get-attribute x 'name))
	      (category (jabber-xml-get-attribute x 'category))
	      (type (jabber-xml-get-attribute x 'type)))
	  (insert (jabber-propertize (if name
					 name
				       "Unnamed")
				     'face 'jabber-title-medium)
		  "\n\nCategory:\t" category "\n")
	  (if type
	      (insert "Type:\t\t" type "\n"))
	  (insert "\n")))
       ((eq (jabber-xml-node-name x) 'feature)
	(let ((var (jabber-xml-get-attribute x 'var)))
	  (insert "Feature:\t" var "\n")))))
    (put-text-property beginning (point)
		       'jabber-jid (jabber-xml-get-attribute xml-data 'from))
    (put-text-property beginning (point)
		       'jabber-account jc)))
;; jabber-process-disco-info:1 ends here

;; [[file:jabber.org::#process-disco-items][jabber-process-disco-items:1]]
(defun jabber-process-disco-items (jc xml-data)
  "Handle results from items disco requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let ((items (jabber-xml-get-children (jabber-iq-query xml-data) 'item)))
    (if items
	(dolist (item items)
	  (let ((jid (jabber-xml-get-attribute item 'jid))
		(name (jabber-xml-get-attribute item 'name))
		(node (jabber-xml-get-attribute item 'node)))
	    (insert
	     (jabber-propertize
	      (concat
	       (jabber-propertize
		(concat jid "\n" (if node (format "Node: %s\n" node)))
		'face 'jabber-title-medium)
	       name "\n\n")
	      'jabber-jid jid
	      'jabber-account jc
	      'jabber-node node))))
      (insert "No items found.\n"))))
;; jabber-process-disco-items:1 ends here

;; [[file:jabber.org::#disco-info-cache][jabber-disco-info-cache:1]]
;; Keys are ("jid" . "node"), where "node" is nil if appropriate.
;; Values are (identities features), where each identity is ["name"
;; "category" "type"], and each feature is a string.
(defvar jabber-disco-info-cache (make-hash-table :test 'equal))
;; jabber-disco-info-cache:1 ends here

;; [[file:jabber.org::#disco-items-cache][jabber-disco-items-cache:1]]
;; Keys are ("jid" . "node").  Values are (items), where each
;; item is ["name" "jid" "node"] (some values may be nil).
(defvar jabber-disco-items-cache (make-hash-table :test 'equal))
;; jabber-disco-items-cache:1 ends here

;; [[file:jabber.org::#disco-get-info][jabber-disco-get-info:1]]
(defun jabber-disco-get-info (jc jid node callback closure-data &optional force)
  "Get disco info for JID and NODE, using connection JC.
Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and result as third argument when result is available.
On success, result is (IDENTITIES FEATURES), where each identity is [\"name\"
\"category\" \"type\"], and each feature is a string.
On error, result is the error node, recognizable by (eq (car result) 'error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate cache and get fresh data."
  (when force
    (remhash (cons jid node) jabber-disco-info-cache))
  (let ((result (unless force (jabber-disco-get-info-immediately jid node))))
    (if result
	(and callback (run-with-timer 0 nil callback jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . "http://jabber.org/protocol/disco#info")
			       ,@(when node `((node . ,node)))))
		      #'jabber-disco-got-info (cons callback closure-data)
		      (lambda (jc xml-data callback-data)
			(when (car callback-data)
			  (funcall (car callback-data) jc (cdr callback-data) (jabber-iq-error xml-data))))
		      (cons callback closure-data)))))
;; jabber-disco-get-info:1 ends here

;; [[file:jabber.org::#disco-got-info][jabber-disco-got-info:1]]
(defun jabber-disco-got-info (jc xml-data callback-data)
  (let ((jid (jabber-xml-get-attribute xml-data 'from))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data)
					'node))
	(result (jabber-disco-parse-info xml-data)))
    (puthash (cons jid node) result jabber-disco-info-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))
;; jabber-disco-got-info:1 ends here

;; [[file:jabber.org::#disco-parse-info][jabber-disco-parse-info:1]]
(defun jabber-disco-parse-info (xml-data)
  "Extract data from an <iq/> stanza containing a disco#info result.
See `jabber-disco-get-info' for a description of the return value.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (list
   (mapcar
    #'(lambda (id)
	(vector (jabber-xml-get-attribute id 'name)
		(jabber-xml-get-attribute id 'category)
		(jabber-xml-get-attribute id 'type)))
    (jabber-xml-get-children (jabber-iq-query xml-data) 'identity))
   (mapcar
    #'(lambda (feature)
	(jabber-xml-get-attribute feature 'var))
    (jabber-xml-get-children (jabber-iq-query xml-data) 'feature))))
;; jabber-disco-parse-info:1 ends here

;; [[file:jabber.org::#disco-get-info-immediately][jabber-disco-get-info-immediately:1]]
(defun jabber-disco-get-info-immediately (jid node)
  "Get cached disco info for JID and NODE.
Return nil if no info available.

Fill the cache with `jabber-disco-get-info'."
  (or
   ;; Check "normal" cache...
   (gethash (cons jid node) jabber-disco-info-cache)
   ;; And then check Entity Capabilities.
   (and (null node) (jabber-caps-get-cached jid))))
;; jabber-disco-get-info-immediately:1 ends here

;; [[file:jabber.org::#disco-get-items][jabber-disco-get-items:1]]
(defun jabber-disco-get-items (jc jid node callback closure-data &optional force)
  "Get disco items for JID and NODE, using connection JC.
Call CALLBACK with JC and CLOSURE-DATA as first and second
arguments and items result as third argument when result is
available.
On success, result is a list of items, where each
item is [\"name\" \"jid\" \"node\"] (some values may be nil).
On error, result is the error node, recognizable by (eq (car result) 'error).

If CALLBACK is nil, just fetch data.  If FORCE is non-nil,
invalidate cache and get fresh data."
  (when force
    (remhash (cons jid node) jabber-disco-items-cache))
  (let ((result (gethash (cons jid node) jabber-disco-items-cache)))
    (if result
	(and callback (run-with-timer 0 nil callback jc closure-data result))
      (jabber-send-iq jc jid
		      "get"
		      `(query ((xmlns . "http://jabber.org/protocol/disco#items")
			       ,@(when node `((node . ,node)))))
		      #'jabber-disco-got-items (cons callback closure-data)
		      (lambda (jc xml-data callback-data)
			(when (car callback-data)
			  (funcall (car callback-data) jc (cdr callback-data) (jabber-iq-error xml-data))))
		      (cons callback closure-data)))))
;; jabber-disco-get-items:1 ends here

;; [[file:jabber.org::#disco-got-items][jabber-disco-got-items:1]]
(defun jabber-disco-got-items (jc xml-data callback-data)
  (let ((jid (jabber-xml-get-attribute xml-data 'from))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data)
					'node))
	(result
	 (mapcar
	  #'(lambda (item)
	      (vector
	       (jabber-xml-get-attribute item 'name)
	       (jabber-xml-get-attribute item 'jid)
	       (jabber-xml-get-attribute item 'node)))
	  (jabber-xml-get-children (jabber-iq-query xml-data) 'item))))
    (puthash (cons jid node) result jabber-disco-items-cache)
    (when (car callback-data)
      (funcall (car callback-data) jc (cdr callback-data) result))))
;; jabber-disco-got-items:1 ends here

;; [[file:jabber.org::#disco-get-items-immediately][jabber-disco-get-items-immediately:1]]
(defun jabber-disco-get-items-immediately (jid node)
  (gethash (cons jid node) jabber-disco-items-cache))
;; jabber-disco-get-items-immediately:1 ends here

;; [[file:jabber.org::#disco-publish][jabber-disco-publish:1]]
(defun jabber-disco-publish (jc node item-name item-jid item-node)
  "Publish the given item under disco node NODE."
  (jabber-send-iq jc nil
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/disco#items")
			   ,@(when node `((node . ,node))))
			  (item ((action . "update")
				 (jid . ,item-jid)
				 ,@(when item-name
				     `((name . ,item-name)))
				 ,@(when item-node
				     `((node . ,item-node))))))
		  'jabber-report-success "Disco publish"
		  'jabber-report-success "Disco publish"))
;; jabber-disco-publish:1 ends here

;; [[file:jabber.org::#disco-publish-remove][jabber-disco-publish-remove:1]]
(defun jabber-disco-publish-remove (jc node item-jid item-node)
  "Remove the given item from published disco items.

JC is the Jabber connection."
  (jabber-send-iq jc nil
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/disco#items")
			   ,@(when node `((node . ,node))))
			  (item ((action . "remove")
				 (jid . ,item-jid)
				 ,@(when item-node
				     `((node . ,item-node))))))
		  'jabber-report-success "Disco removal"
		  'jabber-report-success "Disco removal"))
;; jabber-disco-publish-remove:1 ends here

;; [[file:jabber.org::#xmpp-ping-()][XMPP Ping ([[https://xmpp.org/extensions/xep-0199.html][XEP-0199]]):1]]
(add-to-list 'jabber-jid-info-menu (cons "Ping" 'jabber-ping))
;; XMPP Ping ([[https://xmpp.org/extensions/xep-0199.html][XEP-0199]]):1 ends here

;; [[file:jabber.org::#ping-send][jabber-ping-send:1]]
(defun jabber-ping-send (jc to process-func on-success on-error)
  "Send XEP-0199 ping IQ stanza.
JC is connection to use, TO is full JID, PROCESS-FUNC is fucntion to call to
process result, ON-SUCCESS and ON-ERROR is arg for this function depending on
result."
  (jabber-send-iq jc to "get"
                  '(ping ((xmlns . "urn:xmpp:ping")))
                  process-func on-success
                  process-func on-error))
;; jabber-ping-send:1 ends here

;; [[file:jabber.org::#ping][jabber-ping:1]]
(defun jabber-ping (to)
  "Ping XMPP entity.
TO is full JID.  All connected JIDs is used."
  (interactive (list (jabber-read-jid-completing "Send ping to: " nil nil nil 'full)))
  (dolist (jc jabber-connections)
    (jabber-ping-send jc to 'jabber-silent-process-data 'jabber-process-ping "Ping is unsupported")))
;; jabber-ping:1 ends here

;; [[file:jabber.org::#process-ping][jabber-process-ping:1]]
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
;; jabber-process-ping:1 ends here

;; [[file:jabber.org::#pong][jabber-pong:1]]
(defun jabber-pong (jc xml-data)
  "Return pong as defined in XEP-0199.
Sender and Id are determined from the incoming packet passed in XML-DATA.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result" nil nil nil nil nil id)))
;; jabber-pong:1 ends here

;; [[file:jabber.org::#keepalive][jabber-keepalive:1]]
;;;###autoload
(defgroup jabber-keepalive nil
  "Keepalive functions try to detect lost connection"
  :group 'jabber)
;; jabber-keepalive:1 ends here

;; [[file:jabber.org::#keepalive-interval][jabber-keepalive-interval:1]]
(defcustom jabber-keepalive-interval 600
  "Interval in seconds between connection checks."
  :type 'integer
  :group 'jabber-keepalive)
;; jabber-keepalive-interval:1 ends here

;; [[file:jabber.org::#keepalive-timeout][jabber-keepalive-timeout:1]]
(defcustom jabber-keepalive-timeout 20
  "Seconds to wait for response from server."
  :type 'integer
  :group 'jabber-keepalive)
;; jabber-keepalive-timeout:1 ends here

;; [[file:jabber.org::#keepalive-timer][jabber-keepalive-timer:1]]
(defvar jabber-keepalive-timer nil
  "Timer object for keepalive function.")
;; jabber-keepalive-timer:1 ends here

;; [[file:jabber.org::#keepalive-timeout-timer][jabber-keepalive-timeout-timer:1]]
(defvar jabber-keepalive-timeout-timer nil
  "Timer object for keepalive timeout function.")
;; jabber-keepalive-timeout-timer:1 ends here

;; [[file:jabber.org::#keepalive-pending][jabber-keepalive-pending:1]]
(defvar jabber-keepalive-pending nil
  "List of outstanding keepalive connections.")
;; jabber-keepalive-pending:1 ends here

;; [[file:jabber.org::#keepalive-debug][jabber-keepalive-debug:1]]
(defvar jabber-keepalive-debug nil
  "Log keepalive traffic when non-nil.")
;; jabber-keepalive-debug:1 ends here

;; [[file:jabber.org::#keepalive-start][jabber-keepalive-start:1]]
;;;###autoload
(defun jabber-keepalive-start (&optional jc)
  "Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect it if it doesn't answer.  See variable `jabber-keepalive-interval'
and variable `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument."
  (interactive)

  (when jabber-keepalive-timer
    (jabber-keepalive-stop))

  (setq jabber-keepalive-timer
	(run-with-timer 5
			jabber-keepalive-interval
			'jabber-keepalive-do))
  (add-hook 'jabber-post-disconnect-hook 'jabber-keepalive-stop))
;; jabber-keepalive-start:1 ends here

;; [[file:jabber.org::#keepalive-stop][jabber-keepalive-stop:1]]
(defun jabber-keepalive-stop ()
  "Deactivate keepalive."
  (interactive)

  (when jabber-keepalive-timer
    (jabber-cancel-timer jabber-keepalive-timer)
    (setq jabber-keepalive-timer nil)))
;; jabber-keepalive-stop:1 ends here

;; [[file:jabber.org::#keepalive-do][jabber-keepalive-do:1]]
(defun jabber-keepalive-do ()
  (when jabber-keepalive-debug
    (message "%s: sending keepalive packet(s)" (current-time-string)))
  (setq jabber-keepalive-timeout-timer
	(run-with-timer jabber-keepalive-timeout
			nil
			'jabber-keepalive-timeout))
  (setq jabber-keepalive-pending jabber-connections)
  (dolist (c jabber-connections)
    ;; Whether we get an error or not is not interesting.
    ;; Getting a response at all is.
    (jabber-ping-send c nil 'jabber-keepalive-got-response nil nil)))
;; jabber-keepalive-do:1 ends here

;; [[file:jabber.org::#keepalive-got-response][jabber-keepalive-got-response:1]]
(defun jabber-keepalive-got-response (jc &rest args)
  (when jabber-keepalive-debug
    (message "%s: got keepalive response from %s"
	     (current-time-string)
	     (plist-get (fsm-get-state-data jc) :server)))
  (setq jabber-keepalive-pending (remq jc jabber-keepalive-pending))
  (when (and (null jabber-keepalive-pending) (timerp jabber-keepalive-timeout-timer))
    (jabber-cancel-timer jabber-keepalive-timeout-timer)
    (setq jabber-keepalive-timeout-timer nil)))
;; jabber-keepalive-got-response:1 ends here

;; [[file:jabber.org::#keepalive-timeout-1][jabber-keepalive-timeout:1]]
(defun jabber-keepalive-timeout ()
  (jabber-cancel-timer jabber-keepalive-timer)
  (setq jabber-keepalive-timer nil)

  (dolist (c jabber-keepalive-pending)
    (message "%s: keepalive timeout, connection to %s considered lost"
	     (current-time-string)
	     (plist-get (fsm-get-state-data c) :server))

    (run-hook-with-args 'jabber-lost-connection-hooks c)
    (jabber-disconnect-one c nil)))
;; jabber-keepalive-timeout:1 ends here

;; [[file:jabber.org::#whitespace-ping-interval][jabber-whitespace-ping-interval:1]]
(defcustom jabber-whitespace-ping-interval 30
  "Send a space character to the server with this interval, in seconds.

This is a traditional remedy for a number of problems: to keep NAT
boxes from considering the connection dead, to have the OS discover
earlier that the connection is lost, and to placate servers which rely
on the client doing this, e.g.  Openfire.

If you want to verify that the server is able to answer, see
`jabber-keepalive-start' for another mechanism."
  :type '(integer :tag "Interval in seconds")
  :group 'jabber-core)
;; jabber-whitespace-ping-interval:1 ends here

;; [[file:jabber.org::#whitespace-ping-timer][jabber-whitespace-ping-timer:1]]
(defvar jabber-whitespace-ping-timer nil
  "Timer object for whitespace pings.")
;; jabber-whitespace-ping-timer:1 ends here

;; [[file:jabber.org::#whitespace-ping-start][jabber-whitespace-ping-start:1]]
;;;###autoload
(defun jabber-whitespace-ping-start (&optional jc)
  "Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts."
  (interactive)

  (when jabber-whitespace-ping-timer
    (jabber-whitespace-ping-stop))

  (setq jabber-whitespace-ping-timer
	(run-with-timer 5
			jabber-whitespace-ping-interval
			'jabber-whitespace-ping-do))
  (add-hook 'jabber-post-disconnect-hook 'jabber-whitespace-ping-stop))
;; jabber-whitespace-ping-start:1 ends here

;; [[file:jabber.org::#whitespace-ping-stop][jabber-whitespace-ping-stop:1]]
(defun jabber-whitespace-ping-stop ()
  "Deactivate whitespace pings."
  (interactive)

  (when jabber-whitespace-ping-timer
    (jabber-cancel-timer jabber-whitespace-ping-timer)
    (setq jabber-whitespace-ping-timer nil)))
;; jabber-whitespace-ping-stop:1 ends here

;; [[file:jabber.org::#whitespace-ping-do][jabber-whitespace-ping-do:1]]
(defun jabber-whitespace-ping-do ()
  (dolist (c jabber-connections)
    (ignore-errors (jabber-send-string c " "))))
;; jabber-whitespace-ping-do:1 ends here

;; [[file:jabber.org::#feature-negotiation-()][Feature Negotiation ([[https://xmpp.org/extensions/xep-0020.html][XEP-0020]]):1]]
(jabber-disco-advertise-feature "http://jabber.org/protocol/feature-neg")
;; Feature Negotiation ([[https://xmpp.org/extensions/xep-0020.html][XEP-0020]]):1 ends here

;; [[file:jabber.org::#fn-parse][jabber-fn-parse:1]]
(defun jabber-fn-parse (xml-data type)
  "Parse a Feature Negotiation request, return alist representation.
XML-DATA should have one child element, <x/>, in the jabber:x:data
namespace.

TYPE is either 'request or 'response.

Returned alist has field name as key, and value is a list of offered
alternatives."
  (let ((x (car (jabber-xml-get-children xml-data 'x))))
    (unless (and x
		 (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data"))
      (jabber-signal-error "Modify" 'bad-request "Malformed Feature Negotiation"))

    (let (alist
	  (fields (jabber-xml-get-children x 'field)))
      (dolist (field fields)
	(let ((var (jabber-xml-get-attribute field 'var))
	      (value (car (jabber-xml-get-children field 'value)))
	      (options (jabber-xml-get-children field 'option)))
	  (setq alist (cons
		       (cons var
			     (cond
			      ((eq type 'request)
			       (mapcar #'(lambda (option)
					   (car (jabber-xml-node-children
						 (car (jabber-xml-get-children
						       option 'value)))))
				       options))
			      ((eq type 'response)
			       (jabber-xml-node-children value))
			      (t
			       (error "Incorrect Feature Negotiation type: %s" type))))
		       alist))))
      ;; return alist
      alist)))
;; jabber-fn-parse:1 ends here

;; [[file:jabber.org::#fn-encode][jabber-fn-encode:1]]
(defun jabber-fn-encode (alist type)
  "Transform a feature alist into an <x/> node int the jabber:x:data namespace.
Note that this is not the reverse of `jabber-fn-parse'.

TYPE is either 'request or 'response."
  (let ((requestp (eq type 'request)))
    `(x ((xmlns . "jabber:x:data")
	 (type . ,(if requestp "form" "submit")))
	 ,@(mapcar #'(lambda (field)
		       `(field
			 ((type . "list-single")
			  (var . ,(car field)))
			 ,@(if requestp
				(mapcar
				 #'(lambda (option)
				     `(option nil (value nil ,option)))
				 (cdr field))
			      (list `(value nil ,(cadr field))))))
		   alist))))
;; jabber-fn-encode:1 ends here

;; [[file:jabber.org::#fn-intersection][jabber-fn-intersection:1]]
(defun jabber-fn-intersection (mine theirs)
  "Find values acceptable to both parties.

MINE and THEIRS are alists, as returned by `jabber-fn-parse'.

An alist is returned, where the keys are the negotiated variables,
and the values are lists containing the preferred option.  If
negotiation is impossible, an error is signalled.  The errors are as
specified in XEP-0020, and not necessarily the ones of higher-level
protocols."

  (let ((vars (mapcar #'car mine))
	(their-vars (mapcar #'car theirs)))

    ;; are the same variables being negotiated?
    (sort vars 'string-lessp)
    (sort their-vars 'string-lessp)
    (let ((mine-but-not-theirs (cl-set-difference vars their-vars :test 'string=))
	  (theirs-but-not-mine (cl-set-difference their-vars vars :test 'string=)))
      (when mine-but-not-theirs
	(jabber-signal-error "Modify" 'not-acceptable (car mine-but-not-theirs)))
      (when theirs-but-not-mine
	(jabber-signal-error "Cancel" 'feature-not-implemented (car theirs-but-not-mine))))

      (let (alist)
	(dolist (var vars)
	  (let ((my-options (cdr (assoc var mine)))
		(their-options (cdr (assoc var theirs))))
	    (let ((common-options (cl-intersection my-options their-options :test 'string=)))
	      (if common-options
		  ;; we have a match; but which one to use?
		  ;; the first one will probably work
		  (setq alist
			(cons (list var (car common-options))
			      alist))
		;; no match
		(jabber-signal-error "Modify" 'not-acceptable var)))))
	alist)))
;; jabber-fn-intersection:1 ends here

;; [[file:jabber.org::#widget-display-various-kinds-of-forms][widget - display various kinds of forms:1]]
(require 'widget)
(require 'wid-edit)
;; widget - display various kinds of forms:1 ends here

;; [[file:jabber.org::#widget-alist][jabber-widget-alist:1]]
(defvar jabber-widget-alist nil
  "Alist of widgets currently used.")
;; jabber-widget-alist:1 ends here

;; [[file:jabber.org::#form-type][jabber-form-type:1]]
(defvar jabber-form-type nil
  "Type of form.
One of:
'x-data, jabber:x:data
'register, as used in jabber:iq:register and jabber:iq:search.")
;; jabber-form-type:1 ends here

;; [[file:jabber.org::#submit-to][jabber-submit-to:1]]
(defvar jabber-submit-to nil
  "JID of the entity to which form data is to be sent.")
;; jabber-submit-to:1 ends here

;; [[file:jabber.org::#submit-to][jabber-submit-to:2]]
(jabber-disco-advertise-feature "jabber:x:data")
;; jabber-submit-to:2 ends here

;; [[file:jabber.org::#submit-to][jabber-submit-to:3]]
(define-widget 'jid 'string
  "JID widget."
  :value-to-internal (lambda (widget value)
		       (let ((displayname (jabber-jid-rostername value)))
			 (if displayname
			     (format "%s <%s>" displayname value)
			   value)))
  :value-to-external (lambda (widget value)
		       (if (string-match "<\\([^>]+\\)>[ \t]*$" value)
			   (match-string 1 value)
			 value))
  :complete-function 'jid-complete)
;; jabber-submit-to:3 ends here

;; [[file:jabber.org::#jid-complete][jid-complete:1]]
(defun jid-complete ()
  "Perform completion on JID preceding point."
  (interactive)
  ;; mostly stolen from widget-color-complete
  (let* ((prefix (buffer-substring-no-properties (widget-field-start widget)
						 (point)))
	 (list (append (mapcar #'symbol-name *jabber-roster*)
		       (delq nil
			     (mapcar #'(lambda (item)
					 (when (jabber-jid-rostername item)
					   (format "%s <%s>" (jabber-jid-rostername item)
						   (symbol-name item))))
				     *jabber-roster*))))
	 (completion (try-completion prefix list)))
    (cond ((eq completion t)
	   (message "Exact match."))
	  ((null completion)
	   (error "Can't find completion for \"%s\"" prefix))
	  ((not (string-equal prefix completion))
	   (insert-and-inherit (substring completion (length prefix))))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list (all-completions prefix list nil)
				      prefix))
	   (message "Making completion list...done")))))
;; jid-complete:1 ends here

;; [[file:jabber.org::#init-widget-buffer][jabber-init-widget-buffer:1]]
(defun jabber-init-widget-buffer (submit-to)
  "Setup buffer-local variables for widgets."
  (make-local-variable 'jabber-widget-alist)
  (make-local-variable 'jabber-submit-to)
  (setq jabber-widget-alist nil)
  (setq jabber-submit-to submit-to)
  (setq buffer-read-only nil)
  ;; XXX: This is because data from other queries would otherwise be
  ;; appended to this buffer, which would fail since widget buffers
  ;; are read-only... or something like that.  Maybe there's a
  ;; better way.
  (rename-uniquely))
;; jabber-init-widget-buffer:1 ends here

;; [[file:jabber.org::#render-register-form][jabber-render-register-form:1]]
(defun jabber-render-register-form (query &optional default-username)
  "Display widgets from <query/> element in IQ register or search namespace.
Display widgets from <query/> element in jabber:iq:{register,search} namespace.
DEFAULT-USERNAME is the default value for the username field."
  (make-local-variable 'jabber-widget-alist)
  (setq jabber-widget-alist nil)
  (make-local-variable 'jabber-form-type)
  (setq jabber-form-type 'register)

  (if (jabber-xml-get-children query 'instructions)
      (widget-insert "Instructions: " (car (jabber-xml-node-children (car (jabber-xml-get-children query 'instructions)))) "\n"))
  (if (jabber-xml-get-children query 'registered)
      (widget-insert "You are already registered.  You can change your details here.\n"))
  (widget-insert "\n")

  (let ((possible-fields
	 ;; taken from XEP-0077
	 '((username . "Username")
	   (nick . "Nickname")
	   (password . "Password")
	   (name . "Full name")
	   (first . "First name")
	   (last . "Last name")
	   (email . "E-mail")
	   (address . "Address")
	   (city . "City")
	   (state . "State")
	   (zip . "Zip")
	   (phone . "Telephone")
	   (url . "Web page")
	   (date . "Birth date"))))
    (dolist (field (jabber-xml-node-children query))
      (let ((entry (assq (jabber-xml-node-name field) possible-fields)))
	(when entry
	  (widget-insert (cdr entry) "\t")
	  ;; Special case: when registering a new account, the default
	  ;; username is the one specified in jabber-username.  Things
	  ;; will break if the user changes that name, though...
	  (let ((default-value (or (when (eq (jabber-xml-node-name field) 'username)
				     default-username)
				   "")))
	    (setq jabber-widget-alist
		  (cons
		   (cons (car entry)
			 (widget-create 'editable-field
					:secret  (if (eq (car entry) 'password)
						     ?* nil)
					(or (car (jabber-xml-node-children
						  field)) default-value)))
		   jabber-widget-alist)))
	  (widget-insert "\n"))))))
;; jabber-render-register-form:1 ends here

;; [[file:jabber.org::#parse-register-form][jabber-parse-register-form:1]]
(defun jabber-parse-register-form ()
  "Return children of a <query/> tag containing information entered in the widgets of the current buffer."
  (mapcar
   (lambda (widget-cons)
     (list (car widget-cons)
	   nil
	   (widget-value (cdr widget-cons))))
   jabber-widget-alist))
;; jabber-parse-register-form:1 ends here

;; [[file:jabber.org::#render-xdata-form][jabber-render-xdata-form:1]]
(defun jabber-render-xdata-form (x &optional defaults)
  "Display widgets from <x/> element in jabber:x:data namespace.
DEFAULTS is an alist associating variable names with default values.
DEFAULTS takes precedence over values specified in the form."
  (make-local-variable 'jabber-widget-alist)
  (setq jabber-widget-alist nil)
  (make-local-variable 'jabber-form-type)
  (setq jabber-form-type 'xdata)

  (let ((title (car (jabber-xml-node-children (car (jabber-xml-get-children x 'title))))))
    (if (stringp title)
	(widget-insert (jabber-propertize title 'face 'jabber-title-medium) "\n\n")))
  (let ((instructions (car (jabber-xml-node-children (car (jabber-xml-get-children x 'instructions))))))
    (if (stringp instructions)
	(widget-insert "Instructions: " instructions "\n\n")))

  (dolist (field (jabber-xml-get-children x 'field))
    (let* ((var (jabber-xml-get-attribute field 'var))
	   (label (jabber-xml-get-attribute field 'label))
	   (type (jabber-xml-get-attribute field 'type))
	   (required (jabber-xml-get-children field 'required))
	   (values (jabber-xml-get-children field 'value))
	   (options (jabber-xml-get-children field 'option))
	   (desc (car (jabber-xml-get-children field 'desc)))
	   (default-value (assoc var defaults)))
      ;; "required" not implemented yet

      (cond
       ((string= type "fixed")
	(widget-insert (car (jabber-xml-node-children (car values)))))

       ((string= type "text-multi")
	(if (or label var)
	    (widget-insert (or label var) ":\n"))
	(push (cons (cons var type)
		    (widget-create 'text (or (cdr default-value)
					      (mapconcat #'(lambda (val)
							     (car (jabber-xml-node-children val)))
							 values "\n")
					     "")))
	      jabber-widget-alist))

       ((string= type "list-single")
	(if (or label var)
	    (widget-insert (or label var) ":\n"))
	(push (cons (cons var type)
		    (apply 'widget-create
			   'radio-button-choice
			   :value (or (cdr default-value)
				      (car (xml-node-children (car values))))
			   (mapcar (lambda (option)
				     `(item :tag ,(jabber-xml-get-attribute option 'label)
					    :value ,(car (jabber-xml-node-children (car (jabber-xml-get-children option 'value))))))
				   options)))
	      jabber-widget-alist))

       ((string= type "boolean")
	(push (cons (cons var type)
		    (widget-create 'checkbox
				   :tag (or label var)
				   :value (if default-value
					      (cdr default-value)
					    (not (null
						  (member (car (xml-node-children (car values))) '("1" "true")))))))
	      jabber-widget-alist)
	(if (or label var)
	    (widget-insert " " (or label var) "\n")))

       (t	; in particular including text-single and text-private
	(if (or label var)
	    (widget-insert (or label var) ": "))
	(setq jabber-widget-alist
	      (cons
	       (cons (cons var type)
		     (widget-create 'editable-field
				    :secret (if (string= type "text-private") ?* nil)
				    (or (cdr default-value)
					(car (jabber-xml-node-children (car values)))
					"")))
	       jabber-widget-alist))))
      (when (and desc (car (jabber-xml-node-children desc)))
	(widget-insert "\n" (car (jabber-xml-node-children desc))))
      (widget-insert "\n"))))
;; jabber-render-xdata-form:1 ends here

;; [[file:jabber.org::#parse-xdata-form][jabber-parse-xdata-form:1]]
(defun jabber-parse-xdata-form ()
  "Return an <x/> tag containing information entered in the widgets of the current buffer."
  `(x ((xmlns . "jabber:x:data")
       (type . "submit"))
      ,@(mapcar
	 (lambda (widget-cons)
	   (let ((values (jabber-xdata-value-convert (widget-value (cdr widget-cons)) (cdar widget-cons))))
	     ;; empty fields are not included
	     (when values
	       `(field ((var . ,(caar widget-cons)))
		       ,@(mapcar
			  (lambda (value)
			    (list 'value nil value))
			  values)))))
	 jabber-widget-alist)))
;; jabber-parse-xdata-form:1 ends here

;; [[file:jabber.org::#xdata-value-convert][jabber-xdata-value-convert:1]]
(defun jabber-xdata-value-convert (value type)
  "Convert VALUE from form used by widget library to form required by XEP-0004.
Return a list of strings, each of which to be included as cdata in a <value/> tag."
  (cond
   ((string= type "boolean")
    (if value (list "1") (list "0")))
   ((string= type "text-multi")
    (split-string value "[\n\r]"))
   (t					; in particular including text-single, text-private and list-single
    (if (zerop (length value))
	nil
      (list value)))))
;; jabber-xdata-value-convert:1 ends here

;; [[file:jabber.org::#render-xdata-search-results][jabber-render-xdata-search-results:1]]
(defun jabber-render-xdata-search-results (xdata)
  "Render search results in x:data form."

  (let ((title (car (jabber-xml-get-children xdata 'title))))
    (when title
      (insert (jabber-propertize (car (jabber-xml-node-children title)) 'face 'jabber-title-medium) "\n")))

  (if (jabber-xml-get-children xdata 'reported)
      (jabber-render-xdata-search-results-multi xdata)
    (jabber-render-xdata-search-results-single xdata)))
;; jabber-render-xdata-search-results:1 ends here

;; [[file:jabber.org::#render-xdata-search-results-multi][jabber-render-xdata-search-results-multi:1]]
(defun jabber-render-xdata-search-results-multi (xdata)
  "Render multi-record search results."
  (let (fields
	(jid-fields 0))
    (let ((reported (car (jabber-xml-get-children xdata 'reported)))
	  (column 0))
      (dolist (field (jabber-xml-get-children reported 'field))
	(let (width)
	  ;; Clever algorithm for estimating width based on field type goes here.
	  (setq width 20)

	  (setq fields
		(append
		 fields
		 (list (cons (jabber-xml-get-attribute field 'var)
			     (list 'label (jabber-xml-get-attribute field 'label)
				   'type (jabber-xml-get-attribute field 'type)
				   'column column)))))
	  (setq column (+ column width))
	  (if (string= (jabber-xml-get-attribute field 'type) "jid-single")
	      (setq jid-fields (1+ jid-fields))))))

    (dolist (field-cons fields)
      (indent-to (plist-get (cdr field-cons) 'column) 1)
      (insert (jabber-propertize (plist-get (cdr field-cons) 'label) 'face 'bold)))
    (insert "\n\n")

    ;; Now, the items
    (dolist (item (jabber-xml-get-children xdata 'item))

      (let ((start-of-line (point))
	    jid)

	;; The following code assumes that the order of the <field/>s in each
	;; <item/> is the same as in the <reported/> tag.
	(dolist (field (jabber-xml-get-children item 'field))
	  (let ((field-plist (cdr (assoc (jabber-xml-get-attribute field 'var) fields)))
		(value (car (jabber-xml-node-children (car (jabber-xml-get-children field 'value))))))

	    (indent-to (plist-get field-plist 'column) 1)

	    ;; Absent values are sometimes "", sometimes nil.  insert
	    ;; doesn't like nil.
	    (when value
	      ;; If there is only one JID field, let the whole row
	      ;; have the jabber-jid property.  If there are many JID
	      ;; fields, the string belonging to each field has that
	      ;; property.
	      (if (string= (plist-get field-plist 'type) "jid-single")
		  (if (not (eq jid-fields 1))
		      (insert (jabber-propertize value 'jabber-jid value))
		    (setq jid value)
		    (insert value))
		(insert value)))))

	(if jid
	    (put-text-property start-of-line (point)
			       'jabber-jid jid))
	(insert "\n")))))
;; jabber-render-xdata-search-results-multi:1 ends here

;; [[file:jabber.org::#render-xdata-search-results-single][jabber-render-xdata-search-results-single:1]]
(defun jabber-render-xdata-search-results-single (xdata)
  "Render single-record search results."
  (dolist (field (jabber-xml-get-children xdata 'field))
    (let ((label (jabber-xml-get-attribute field 'label))
	  (type (jabber-xml-get-attribute field 'type))
	  (values (mapcar #'(lambda (val)
			      (car (jabber-xml-node-children val)))
			  (jabber-xml-get-children field 'value))))
      ;; XXX: consider type
      (insert (jabber-propertize (concat label ": ") 'face 'bold))
      (indent-to 30)
      (insert (apply #'concat values) "\n"))))
;; jabber-render-xdata-search-results-single:1 ends here

;; [[file:jabber.org::#xdata-formtype][jabber-xdata-formtype:1]]
(defun jabber-xdata-formtype (x)
  "Return the form type of the xdata form in X, by XEP-0068.
Return nil if no form type is specified."
  (catch 'found-formtype
    (dolist (field (jabber-xml-get-children x 'field))
      (when (and (string= (jabber-xml-get-attribute field 'var) "FORM_TYPE")
		 (string= (jabber-xml-get-attribute field 'type) "hidden"))
	(throw 'found-formtype (car (jabber-xml-node-children
				     (car (jabber-xml-get-children field 'value)))))))))
;; jabber-xdata-formtype:1 ends here

;; [[file:jabber.org::#bookmarks][jabber-bookmarks:1]]
(defvar jabber-bookmarks (make-hash-table :test 'equal)
  "Mapping from full JIDs to bookmarks.
Bookmarks are what has been retrieved from the server, as list of
XML elements.  This is nil if bookmarks have not been retrieved,
and t if no bookmarks where found.")
;; jabber-bookmarks:1 ends here

;; [[file:jabber.org::#get-conference-data][jabber-get-conference-data:1]]
;;;###autoload
(defun jabber-get-conference-data (jc conference-jid cont &optional key)
  "Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache."
  (if (null cont)
      (let ((cache (jabber-get-bookmarks-from-cache jc)))
       (if (and cache (listp cache))
        (jabber-get-conference-data-internal
         cache conference-jid key)))
    (jabber-get-bookmarks
     jc
     (let ((conference-jid conference-jid)
		   (key key)
		   (cont cont))
       (lambda (jc result)
	 (let ((entry (jabber-get-conference-data-internal result conference-jid key)))
	   (funcall cont jc entry)))))))
;; jabber-get-conference-data:1 ends here

;; [[file:jabber.org::#get-conference-data-internal][jabber-get-conference-data-internal:1]]
(defun jabber-get-conference-data-internal (result conference-jid key)
  (let ((entry (dolist (node result)
		(when (and (eq (jabber-xml-node-name node) 'conference)
			   (string= (jabber-xml-get-attribute node 'jid) conference-jid))
		  (cl-return (jabber-parse-conference-bookmark node))))))
    (if key
	(plist-get entry key)
      entry)))
;; jabber-get-conference-data-internal:1 ends here

;; [[file:jabber.org::#parse-conference-bookmark][jabber-parse-conference-bookmark:1]]
;;;###autoload
(defun jabber-parse-conference-bookmark (node)
  "Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password."
  (when (eq (jabber-xml-node-name node) 'conference)
    (list :jid (jabber-xml-get-attribute node 'jid)
	  :name (jabber-xml-get-attribute node 'name)
	  :autojoin (member (jabber-xml-get-attribute node 'autojoin)
			    '("true" "1"))
	  :nick (car (jabber-xml-node-children
		      (car (jabber-xml-get-children node 'nick))))
	  :password (car (jabber-xml-node-children
			  (car (jabber-xml-get-children node 'password)))))))
;; jabber-parse-conference-bookmark:1 ends here

;; [[file:jabber.org::#get-bookmarks][jabber-get-bookmarks:1]]
;;;###autoload
(defun jabber-get-bookmarks (jc cont &optional refresh)
  "Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks."
  (let ((bookmarks (gethash (jabber-connection-bare-jid jc) jabber-bookmarks)))
    (if (and (not refresh) bookmarks)
	(run-with-timer 0 nil cont jc (when (listp bookmarks) bookmarks))
      (let* ((cont cont)
		     (callback (lambda (jc result) (jabber-get-bookmarks-1 jc result cont))))
	(jabber-private-get jc 'storage "storage:bookmarks"
			    callback callback)))))
;; jabber-get-bookmarks:1 ends here

;; [[file:jabber.org::#get-bookmarks-1][jabber-get-bookmarks-1:1]]
(defun jabber-get-bookmarks-1 (jc result cont)
  (let ((my-jid (jabber-connection-bare-jid jc))
	(value
	 (if (eq (jabber-xml-node-name result) 'storage)
	     (or (jabber-xml-node-children result) t)
	   t)))
    (puthash my-jid value jabber-bookmarks)
    (funcall cont jc (when (listp value) value))))
;; jabber-get-bookmarks-1:1 ends here

;; [[file:jabber.org::#get-bookmarks-from-cache][jabber-get-bookmarks-from-cache:1]]
;;;###autoload
(defun jabber-get-bookmarks-from-cache (jc)
  "Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil."
  (gethash (jabber-connection-bare-jid jc) jabber-bookmarks))
;; jabber-get-bookmarks-from-cache:1 ends here

;; [[file:jabber.org::#set-bookmarks][jabber-set-bookmarks:1]]
(defun jabber-set-bookmarks (jc bookmarks &optional callback)
  "Set bookmarks to BOOKMARKS, which is a list of XML elements.
If CALLBACK is non-nil, call it with JC and t or nil as arguments
on success or failure, respectively."
  (unless callback
    (setq callback #'ignore))
  (jabber-private-set
   jc
   `(storage ((xmlns . "storage:bookmarks"))
	     ,@bookmarks)
   callback t
   callback nil))
;; jabber-set-bookmarks:1 ends here

;; [[file:jabber.org::#edit-bookmarks][jabber-edit-bookmarks:1]]
;;;###autoload
(defun jabber-edit-bookmarks (jc)
  "Create a buffer for editing bookmarks interactively.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-get-bookmarks jc 'jabber-edit-bookmarks-1 t))
;; jabber-edit-bookmarks:1 ends here

;; [[file:jabber.org::#edit-bookmarks-1][jabber-edit-bookmarks-1:1]]
(defun jabber-edit-bookmarks-1 (jc bookmarks)
  (setq bookmarks
	(mapcar
	 (lambda (e)
	   (cl-case (jabber-xml-node-name e)
	     (url
	      (list 'url (or (jabber-xml-get-attribute e 'url) "")
		    (or (jabber-xml-get-attribute e 'name) "")))
	     (conference
	      (list 'conference
		    (or (jabber-xml-get-attribute e 'jid) "")
		    (or (jabber-xml-get-attribute e 'name) "")
		    (not (not (member (jabber-xml-get-attribute e 'autojoin)
				      '("true" "1"))))
		    (or (jabber-xml-path e '(nick "")) "")
		    (or (jabber-xml-path e '(password "")) "")))))
	 bookmarks))
  (setq bookmarks (delq nil bookmarks))
  (with-current-buffer (get-buffer-create "Edit bookmarks")
    (jabber-init-widget-buffer nil)
    (setq jabber-buffer-connection jc)

    (widget-insert (jabber-propertize (concat "Edit bookmarks for "
					      (jabber-connection-bare-jid jc))
				      'face 'jabber-title-large)
		   "\n\n")

    (when (or (bound-and-true-p jabber-muc-autojoin)
	      (bound-and-true-p jabber-muc-default-nicknames))
      (widget-insert "The variables `jabber-muc-autojoin' and/or `jabber-muc-default-nicknames'\n"
		     "contain values.  They are only available to jabber.el on this machine.\n"
		     "You may want to import them into your bookmarks, to make them available\n"
		     "to any client on any machine.\n")
      (widget-create 'push-button :notify 'jabber-bookmarks-import "Import values from variables")
      (widget-insert "\n\n"))

    (push (cons 'bookmarks
		(widget-create
		 '(repeat
		   :tag "Bookmarks"
		   (choice
		    (list :tag "Conference"
			  (const :format "" conference)
			  (string :tag "JID") ;XXX: jid widget type?
			  (string :tag "Name")
			  (checkbox :tag "Autojoin" :format "%[%v%] Autojoin?\n")
			  (string :tag "Nick")	      ;or nil?
			  (string :tag "Password")    ;or nil?
			  )
		    (list :tag "URL"
			  (const :format "" url)
			  (string :tag "URL")
			  (string :tag "Name"))))
		 :value bookmarks))
	  jabber-widget-alist)

    (widget-insert "\n")
    (widget-create 'push-button :notify 'jabber-bookmarks-submit "Submit")

    (widget-setup)
    (widget-minor-mode 1)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))
;; jabber-edit-bookmarks-1:1 ends here

;; [[file:jabber.org::#bookmarks-submit][jabber-bookmarks-submit:1]]
(defun jabber-bookmarks-submit (&rest ignore)
  (let ((bookmarks (widget-value (cdr (assq 'bookmarks jabber-widget-alist)))))
    (setq bookmarks
	  (mapcar
	   (lambda (entry)
	     (cl-case (car entry)
	       (url
		(cl-destructuring-bind (symbol url name) entry
		  `(url ((url . ,url)
			 (name . ,name)))))
	       (conference
		(cl-destructuring-bind (symbol jid name autojoin nick password)
		    entry
		  `(conference ((jid . ,jid)
				(name . ,name)
				(autojoin . ,(if autojoin
						 "1"
					       "0")))
			       ,@(unless (zerop (length nick))
				   `((nick () ,nick)))
			       ,@(unless (zerop (length password))
				   `((password () ,password))))))))
	   bookmarks))
    (remhash (jabber-connection-bare-jid jabber-buffer-connection) jabber-bookmarks)
    (jabber-private-set
     jabber-buffer-connection
     `(storage ((xmlns . "storage:bookmarks"))
	       ,@bookmarks)
     'jabber-report-success "Storing bookmarks"
     'jabber-report-success "Storing bookmarks")))
;; jabber-bookmarks-submit:1 ends here

;; [[file:jabber.org::#bookmarks-import][jabber-bookmarks-import:1]]
(defun jabber-bookmarks-import (&rest ignore)
  (let* ((value (widget-value (cdr (assq 'bookmarks jabber-widget-alist))))
	 (conferences (mapcar
		       'cdr
		       (cl-remove-if-not
			(lambda (entry)
			  (eq (car entry) 'conference))
			value))))
    (dolist (default-nickname jabber-muc-default-nicknames)
      (cl-destructuring-bind (muc-jid . nick) default-nickname
	(let ((entry (assoc muc-jid conferences)))
	  (if entry
	      (setf (cl-fourth entry) nick)
	    (setq entry (list muc-jid "" nil nick ""))
	    (push entry conferences)
	    (push (cons 'conference entry) value)))))
    (dolist (autojoin jabber-muc-autojoin)
      (let ((entry (assoc autojoin conferences)))
	(if entry
	    (setf (cl-third entry) t)
	  (setq entry (list autojoin "" t "" ""))
	  (push (cons 'conference entry) value))))
    (widget-value-set (cdr (assq 'bookmarks jabber-widget-alist)) value)
    (widget-setup)))
;; jabber-bookmarks-import:1 ends here

;; [[file:jabber.org::#private-get][jabber-private-get:1]]
;;;###autoload
(defun jabber-private-get (jc node-name namespace success-callback error-callback)
  "Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result."
  (jabber-send-iq jc nil "get"
		  `(query ((xmlns . "jabber:iq:private"))
			  (,node-name ((xmlns . ,namespace))))
		  #'jabber-private-get-1 success-callback
		  #'(lambda (jc xml-data error-callback)
		      (funcall error-callback jc xml-data))
		  error-callback))
;; jabber-private-get:1 ends here

;; [[file:jabber.org::#private-get-1][jabber-private-get-1:1]]
(defun jabber-private-get-1 (jc xml-data success-callback)
  (funcall success-callback jc
	   (car (jabber-xml-node-children
		 (jabber-iq-query xml-data)))))
;; jabber-private-get-1:1 ends here

;; [[file:jabber.org::#private-set][jabber-private-set:1]]
;;;###autoload
(defun jabber-private-set (jc fragment &optional
			      success-callback success-closure-data
			      error-callback error-closure-data)
  "Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

JC is the Jabber connection."
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . "jabber:iq:private"))
			  ,fragment)
		  success-callback success-closure-data
		  error-callback error-closure-data))
;; jabber-private-set:1 ends here

;; [[file:jabber.org::#muc-nick-coloring][muc-nick-coloring:1]]
;; we need hexrgb-hsv-to-hex:
(eval-and-compile
  (or (ignore-errors (require 'hexrgb))
      ;; jabber-fallback-lib/ from jabber/lisp/jabber-fallback-lib
      (ignore-errors
        (let* ((source    (or (locate-library "jabber")
                              load-file-name))
               (load-path (cons (expand-file-name
                                 "jabber-fallback-lib"
                                 (file-name-directory source))
                                load-path)))
          (require 'hexrgb)))
      (error
       "The hexrgb library was not found in `load-path' or jabber-fallback-lib/ directory")))
;; muc-nick-coloring:1 ends here

;; [[file:jabber.org::#muc-participant-colors][jabber-muc-participant-colors:1]]
(defcustom jabber-muc-participant-colors nil
  "Alist of used colors.
Format is (nick . color).  Color may be
in #RGB or textual (like red or blue) notation.  Colors will be
added in #RGB notation for unknown nicks."
  :type '(alist :key-type string :value-type color)
  :group 'jabber-chat)
;; jabber-muc-participant-colors:1 ends here

;; [[file:jabber.org::#muc-colorize-local][jabber-muc-colorize-local:1]]
(defcustom jabber-muc-colorize-local nil
  "Colorize MUC messages from you."
  :type 'boolean
  :group 'jabber-chat)
;; jabber-muc-colorize-local:1 ends here

;; [[file:jabber.org::#muc-colorize-foreign][jabber-muc-colorize-foreign:1]]
(defcustom jabber-muc-colorize-foreign nil
  "Colorize MUC messages not from you."
  :type 'boolean
  :group 'jabber-chat)
;; jabber-muc-colorize-foreign:1 ends here

;; [[file:jabber.org::#muc-nick-saturation][jabber-muc-nick-saturation:1]]
(defcustom jabber-muc-nick-saturation 1.0
  "Default saturation for nick coloring."
  :type 'float
  :group 'jabber-chat)
;; jabber-muc-nick-saturation:1 ends here

;; [[file:jabber.org::#muc-nick-value][jabber-muc-nick-value:1]]
(defcustom jabber-muc-nick-value 1.0
  "Default value for nick coloring."
  :type 'float
  :group 'jabber-chat)
;; jabber-muc-nick-value:1 ends here

;; [[file:jabber.org::#muc-nick-gen-color][jabber-muc-nick-gen-color:1]]
(defun jabber-muc-nick-gen-color (nick)
  "Return a good enough color from the available pool."
  (let ((hue (/ (mod (string-to-number (substring (md5 nick) 0 6) 16) 360) 360.0)))
    (hexrgb-hsv-to-hex hue jabber-muc-nick-saturation jabber-muc-nick-value)))
;; jabber-muc-nick-gen-color:1 ends here

;; [[file:jabber.org::#muc-nick-get-color][jabber-muc-nick-get-color:1]]
(defun jabber-muc-nick-get-color (nick)
  "Get NICKs color."
  (let ((color (cdr (assoc nick jabber-muc-participant-colors))))
    (if color
        color
      (progn
        (unless jabber-muc-participant-colors)
        (push (cons nick (jabber-muc-nick-gen-color nick)) jabber-muc-participant-colors)
        (cdr (assoc nick jabber-muc-participant-colors))))))
;; jabber-muc-nick-get-color:1 ends here

;; [[file:jabber.org::#multi-user-chat-(muc)-()][Multi-User Chat (MUC) ([[https://xmpp.org/extensions/xep-0045.html][XEP-0045]]):1]]
;; we need jabber-bookmarks for jabber-muc-autojoin (via
;; jabber-get-bookmarks and jabber-parse-conference-bookmark):
;; Multi-User Chat (MUC) ([[https://xmpp.org/extensions/xep-0045.html][XEP-0045]]):1 ends here

;; [[file:jabber.org::#*jabber-active-groupchats*][*jabber-active-groupchats*:1]]
;;;###autoload
(defvar *jabber-active-groupchats* nil
  "Alist of groupchats and nicknames.
Keys are strings, the bare JID of the room.
Values are strings.")
;; *jabber-active-groupchats*:1 ends here

;; [[file:jabber.org::#pending-groupchats][jabber-pending-groupchats:1]]
(defvar jabber-pending-groupchats (make-hash-table)
  "Hash table of groupchats and nicknames.
Keys are JID symbols; values are strings.
This table records the last nickname used to join the particular
chat room.  Items are thus never removed.")
;; jabber-pending-groupchats:1 ends here

;; [[file:jabber.org::#muc-participants][jabber-muc-participants:1]]
(defvar jabber-muc-participants nil
  "Alist of groupchats and participants.
Keys are strings, the bare JID of the room.
Values are lists of nickname strings.")
;; jabber-muc-participants:1 ends here

;; [[file:jabber.org::#group][jabber-group:1]]
(defvar jabber-group nil
  "The groupchat you are participating in.")
;; jabber-group:1 ends here

;; [[file:jabber.org::#muc-topic][jabber-muc-topic:1]]
(defvar jabber-muc-topic ""
  "The topic of the current MUC room.")
;; jabber-muc-topic:1 ends here

;; [[file:jabber.org::#role-history][jabber-role-history:1]]
(defvar jabber-role-history ()
  "Keeps track of previously used roles.")
;; jabber-role-history:1 ends here

;; [[file:jabber.org::#affiliation-history][jabber-affiliation-history:1]]
(defvar jabber-affiliation-history ()
  "Keeps track of previously used affiliations.")
;; jabber-affiliation-history:1 ends here

;; [[file:jabber.org::#muc-nickname-history][jabber-muc-nickname-history:1]]
(defvar jabber-muc-nickname-history ()
  "Keeps track of previously referred-to nicknames.")
;; jabber-muc-nickname-history:1 ends here

;; [[file:jabber.org::#muc-default-nicknames][jabber-muc-default-nicknames:1]]
(defcustom jabber-muc-default-nicknames nil
  "Default nickname for specific MUC rooms."
  :group 'jabber-chat
  :type '(repeat
	  (cons :format "%v"
		(string :tag "JID of room")
		(string :tag "Nickname"))))
;; jabber-muc-default-nicknames:1 ends here

;; [[file:jabber.org::#muc-autojoin][jabber-muc-autojoin:1]]
(defcustom jabber-muc-autojoin nil
  "List of MUC rooms to automatically join on connection.
This list is saved in your Emacs customizations.  You can also store
such a list on the Jabber server, where it is available to every
client; see `jabber-edit-bookmarks'."
  :group 'jabber-chat
  :type '(repeat (string :tag "JID of room")))
;; jabber-muc-autojoin:1 ends here

;; [[file:jabber.org::#muc-disable-disco-check][jabber-muc-disable-disco-check:1]]
(defcustom jabber-muc-disable-disco-check nil
  "If non-nil, disable checking disco#info of rooms before joining them.
Disco information can tell whether the room exists and whether it is
password protected, but some servers do not support it.  If you want
to join chat rooms on such servers, set this variable to t."
  :group 'jabber-chat
  :type 'boolean)
;; jabber-muc-disable-disco-check:1 ends here

;; [[file:jabber.org::#groupchat-buffer-format][jabber-groupchat-buffer-format:1]]
(defcustom jabber-groupchat-buffer-format "*-jabber-groupchat-%n-*"
  "The format specification for the name of groupchat buffers.

These fields are available (all are about the group you are chatting
in):

%n   Roster name of group, or JID if no nickname set
%b   Name of group from bookmarks or roster name or JID if none set
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)
;; jabber-groupchat-buffer-format:1 ends here

;; [[file:jabber.org::#groupchat-prompt-format][jabber-groupchat-prompt-format:1]]
(defcustom jabber-groupchat-prompt-format "[%t] %n> "
  "The format specification for lines in groupchat.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
%n, %u, %r
     Nickname in groupchat
%j   Full JID (room@server/nick)"
  :type 'string
  :group 'jabber-chat)
;; jabber-groupchat-prompt-format:1 ends here

;; [[file:jabber.org::#muc-header-line-format][jabber-muc-header-line-format:1]]
(defcustom jabber-muc-header-line-format
  '(" " (:eval (jabber-jid-displayname jabber-group))
    "\t" jabber-muc-topic)
  "The specification for the header line of MUC buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)
;; jabber-muc-header-line-format:1 ends here

;; [[file:jabber.org::#muc-private-buffer-format][jabber-muc-private-buffer-format:1]]
(defcustom jabber-muc-private-buffer-format "*-jabber-muc-priv-%g-%n-*"
  "The format specification for the buffer name for private MUC messages.

These fields are available:

%g   Roster name of group, or JID if no nickname set
%n   Nickname of the group member you're chatting with"
  :type 'string
  :group 'jabber-chat)
;; jabber-muc-private-buffer-format:1 ends here

;; [[file:jabber.org::#muc-private-foreign-prompt-format][jabber-muc-private-foreign-prompt-format:1]]
(defcustom jabber-muc-private-foreign-prompt-format "[%t] %g/%n> "
  "The format specification for lines others type in a private MUC buffer.

These fields are available:

%t  Time, formatted according to `jabber-chat-time-format'
%n  Nickname in room
%g  Short room name (either roster name or username part of JID)"
  :type 'string
  :group 'jabber-chat)
;; jabber-muc-private-foreign-prompt-format:1 ends here

;; [[file:jabber.org::#muc-print-names-format][jabber-muc-print-names-format:1]]
(defcustom jabber-muc-print-names-format "	%n	%a	%j\n"
  "The format specification for MUC list lines.

Fields available:

%n  Nickname in room
%a  Affiliation status
%j  Full JID (room@server/nick)"
  :type 'string
  :group 'jabber-chat)
;; jabber-muc-print-names-format:1 ends here

;; [[file:jabber.org::#muc-private-header-line-format][jabber-muc-private-header-line-format:1]]
(defcustom jabber-muc-private-header-line-format
  '(" " (:eval (jabber-jid-resource jabber-chatting-with))
    " in " (:eval (jabber-jid-displayname (jabber-jid-user jabber-chatting-with)))
    "\t" jabber-events-message
    "\t" jabber-chatstates-message)
  "The specification for the header line of private MUC chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)
;; jabber-muc-private-header-line-format:1 ends here

;; [[file:jabber.org::#muc-printers][jabber-muc-printers:1]]
;;;###autoload
(defvar jabber-muc-printers '()
  "List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")
;; jabber-muc-printers:1 ends here

;; [[file:jabber.org::#muc-printers][jabber-muc-printers:2]]
;;;###autoload
(defun jabber-muc-get-buffer (group)
  "Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-groupchat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname group))
                (cons ?b (jabber-jid-bookmarkname group))
		(cons ?j (jabber-jid-user group)))))
;; jabber-muc-printers:2 ends here

;; [[file:jabber.org::#muc-create-buffer][jabber-muc-create-buffer:1]]
(defun jabber-muc-create-buffer (jc group)
  "Prepare a buffer for chatroom GROUP.
This function is idempotent.

JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-muc-get-buffer group))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode jc #'jabber-chat-pp))
    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)

    (set (make-local-variable 'jabber-group) group)
    (make-local-variable 'jabber-muc-topic)
    (setq jabber-send-function 'jabber-muc-send)
    (setq header-line-format jabber-muc-header-line-format)
    (current-buffer)))
;; jabber-muc-create-buffer:1 ends here

;; [[file:jabber.org::#muc-private-get-buffer][jabber-muc-private-get-buffer:1]]
;;;###autoload
(defun jabber-muc-private-get-buffer (group nickname)
  "Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-muc-private-buffer-format
	       (list
		(cons ?g (jabber-jid-displayname group))
		(cons ?n nickname))))
;; jabber-muc-private-get-buffer:1 ends here

;; [[file:jabber.org::#muc-private-create-buffer][jabber-muc-private-create-buffer:1]]
(defun jabber-muc-private-create-buffer (jc group nickname)
  "Prepare a buffer for chatting with NICKNAME in GROUP.
This function is idempotent.

JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-muc-private-get-buffer group nickname))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode jc #'jabber-chat-pp))

    (set (make-local-variable 'jabber-chatting-with) (concat group "/" nickname))
    (setq jabber-send-function 'jabber-chat-send)
    (setq header-line-format jabber-muc-private-header-line-format)

    (current-buffer)))
;; jabber-muc-private-create-buffer:1 ends here

;; [[file:jabber.org::#muc-send][jabber-muc-send:1]]
(defun jabber-muc-send (jc body)
  "Send BODY to MUC room in current buffer.

JC is the Jabber connection."
  ;; There is no need to display the sent message in the buffer, as
  ;; we will get it back from the MUC server.
  (jabber-send-sexp jc
		    `(message
		      ((to . ,jabber-group)
		       (type . "groupchat"))
		      (body () ,body))))
;; jabber-muc-send:1 ends here

;; [[file:jabber.org::#muc-add-groupchat][jabber-muc-add-groupchat:1]]
(defun jabber-muc-add-groupchat (group nickname)
  "Remember participating in GROUP under NICKNAME."
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    (if whichgroup
	(setcdr whichgroup nickname)
      (add-to-list '*jabber-active-groupchats* (cons group nickname)))))
;; jabber-muc-add-groupchat:1 ends here

;; [[file:jabber.org::#muc-remove-groupchat][jabber-muc-remove-groupchat:1]]
(defun jabber-muc-remove-groupchat (group)
  "Remove GROUP from internal bookkeeping."
  (let ((whichgroup (assoc group *jabber-active-groupchats*))
	(whichparticipants (assoc group jabber-muc-participants)))
    (setq *jabber-active-groupchats*
	  (delq whichgroup *jabber-active-groupchats*))
    (setq jabber-muc-participants
	  (delq whichparticipants jabber-muc-participants))))
;; jabber-muc-remove-groupchat:1 ends here

;; [[file:jabber.org::#muc-connection-closed][jabber-muc-connection-closed:1]]
(defun jabber-muc-connection-closed (bare-jid)
  "Remove MUC data for BARE-JID.
Forget all information about rooms that had been entered with
this JID.  Suitable to call when the connection is closed."
  (dolist (room-entry jabber-muc-participants)
    (let* ((room (car room-entry))
	   (buffer (get-buffer (jabber-muc-get-buffer room))))
      (when (bufferp buffer)
	(with-current-buffer buffer
	  (when (string= bare-jid
			 (jabber-connection-bare-jid jabber-buffer-connection))
	    (setq *jabber-active-groupchats*
		  (cl-delete room *jabber-active-groupchats*
			   :key #'car :test #'string=))
	    (setq jabber-muc-participants
		  (delq room-entry jabber-muc-participants))))))))
;; jabber-muc-connection-closed:1 ends here

;; [[file:jabber.org::#muc-participant-plist][jabber-muc-participant-plist:1]]
(defun jabber-muc-participant-plist (group nickname)
  "Return plist associated with NICKNAME in GROUP.
Return nil if nothing known about that combination."
  (let ((whichparticipants (assoc group jabber-muc-participants)))
    (when whichparticipants
      (cdr (assoc nickname whichparticipants)))))
;; jabber-muc-participant-plist:1 ends here

;; [[file:jabber.org::#muc-modify-participant][jabber-muc-modify-participant:1]]
(defun jabber-muc-modify-participant (group nickname new-plist)
  "Assign properties in NEW-PLIST to NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    ;; either we have a list of participants already...
    (if participants
	(let ((participant (assoc nickname participants)))
	  ;; and maybe this participant is already in the list
	  (if participant
	      ;; if so, just update role, affiliation, etc.
	      (setf (cdr participant) new-plist)
	    (push (cons nickname new-plist) (cdr participants))))
      ;; or we don't
      (push (cons group (list (cons nickname new-plist))) jabber-muc-participants))))
;; jabber-muc-modify-participant:1 ends here

;; [[file:jabber.org::#muc-report-delta][jabber-muc-report-delta:1]]
(defun jabber-muc-report-delta (nickname old-plist new-plist reason actor)
  "Compare OLD-PLIST and NEW-PLIST, and return a string explaining the change.
Return nil if nothing noteworthy has happened.
NICKNAME is the user experiencing the change.  REASON and ACTOR, if non-nil,
are the corresponding presence fields.

This function is only concerned with presence stanzas resulting
in the user entering/staying in the room."
  ;; The keys in the plist are affiliation, role and jid.
  (when (plist-get new-plist 'jid)
    ;; nickname is only used for displaying, so we can modify it if we
    ;; want to.
    (setq nickname (concat nickname " <"
			   (jabber-jid-user (plist-get new-plist 'jid))
			   ">")))
  (cond
   ((null old-plist)
    ;; User enters the room
    (concat nickname " enters the room ("
	    (plist-get new-plist 'role)
	    (unless (string= (plist-get new-plist 'affiliation) "none")
	      (concat ", " (plist-get new-plist 'affiliation)))
	    ")"))

   ;; If affiliation changes, the role change is usually the logical
   ;; one, so don't report it separately.
   ((not (string= (plist-get old-plist 'affiliation)
		  (plist-get new-plist 'affiliation)))
    (let ((actor-reason (concat (when actor
				  (concat " by " actor))
				(when reason
				  (concat ": " reason))))
	  (from (plist-get old-plist 'affiliation))
	  (to (plist-get new-plist 'affiliation)))
      ;; There are many ways to express these transitions in English.
      ;; This one favors eloquence over regularity and consistency.
      (cond
       ;; Higher affiliation
       ((or (and (member from '("outcast" "none" "member"))
		 (member to '("admin" "owner")))
	    (and (string= from "admin") (string= to "owner")))
	(concat nickname " has been promoted to " to actor-reason))
       ;; Lower affiliation
       ((or (and (member from '("owner" "admin"))
		 (string= to "member"))
	    (and (string= from "owner") (string= to "admin")))
	(concat nickname " has been demoted to " to actor-reason))
       ;; Become member
       ((string= to "member")
	(concat nickname " has been granted membership" actor-reason))
       ;; Lose membership
       ((string= to "none")
	(concat nickname " has been deprived of membership" actor-reason)))))

   ;; Role changes
   ((not (string= (plist-get old-plist 'role)
		  (plist-get new-plist 'role)))
    (let ((actor-reason (concat (when actor
				  (concat " by " actor))
				(when reason
				  (concat ": " reason))))
	  (from (plist-get old-plist 'role))
	  (to (plist-get new-plist 'role)))
      ;; Possible roles are "none" (not in room, hence not of interest
      ;; in this function), "visitor" (no voice), "participant" (has
      ;; voice), and "moderator".
      (cond
       ((string= to "moderator")
	(concat nickname " has been granted moderator privileges" actor-reason))
       ((and (string= from "moderator")
	     (string= to "participant"))
	(concat nickname " had moderator privileges revoked" actor-reason))
       ((string= to "participant")
	(concat nickname " has been granted voice" actor-reason))
       ((string= to "visitor")
	(concat nickname " has been denied voice" actor-reason)))))))
;; jabber-muc-report-delta:1 ends here

;; [[file:jabber.org::#muc-remove-participant][jabber-muc-remove-participant:1]]
(defun jabber-muc-remove-participant (group nickname)
  "Forget everything about NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    (when participants
      (let ((participant (assoc nickname (cdr participants))))
	(setf (cdr participants) (delq participant (cdr participants)))))))
;; jabber-muc-remove-participant:1 ends here

;; [[file:jabber.org::#muc-argument-list][jabber-muc-argument-list:1]]
(defmacro jabber-muc-argument-list (&optional args)
  "Prepend connection and group name to ARGS.
If the current buffer is not an MUC buffer, signal an error.
This macro is meant for use as an argument to `interactive'."
  `(if (null jabber-group)
       (error "Not in MUC buffer")
     (nconc (list jabber-buffer-connection jabber-group) ,args)))
;; jabber-muc-argument-list:1 ends here

;; [[file:jabber.org::#muc-read-completing][jabber-muc-read-completing:1]]
(defun jabber-muc-read-completing (prompt &optional allow-not-joined)
  "Read the name of a joined chatroom, or use chatroom of current buffer if any.
If ALLOW-NOT-JOINED is provided and non-nil, permit choosing any
JID; only provide completion as a guide."
  (or jabber-group
      (jabber-read-jid-completing prompt
				  (if (null *jabber-active-groupchats*)
				      (error "You haven't joined any group")
				    (mapcar (lambda (x) (jabber-jid-symbol (car x)))
					    *jabber-active-groupchats*))
				  (not allow-not-joined)
				  jabber-group)))
;; jabber-muc-read-completing:1 ends here

;; [[file:jabber.org::#muc-read-nickname][jabber-muc-read-nickname:1]]
(defun jabber-muc-read-nickname (group prompt)
  "Read the nickname of a participant in GROUP."
  (let ((nicknames (cdr (assoc group jabber-muc-participants))))
    (unless nicknames
      (error "Unknown group: %s" group))
    (completing-read prompt nicknames nil t nil 'jabber-muc-nickname-history)))
;; jabber-muc-read-nickname:1 ends here

;; [[file:jabber.org::#muc-read-nickname][jabber-muc-read-nickname:2]]
(add-to-list 'jabber-jid-muc-menu
             (cons "Request vcard" 'jabber-muc-vcard-get))
;; jabber-muc-read-nickname:2 ends here

;; [[file:jabber.org::#muc-vcard-get][jabber-muc-vcard-get:1]]
;;;###autoload
(defun jabber-muc-vcard-get (jc group nickname)
  "Request vcard from chat with NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
    (let ((muc-name (format "%s/%s" group nickname)))
	(jabber-vcard-get jc muc-name)))
;; jabber-muc-vcard-get:1 ends here

;; [[file:jabber.org::#muc-instant-config][jabber-muc-instant-config:1]]
(defun jabber-muc-instant-config (jc group)
  "Accept default configuration for GROUP.
This can be used for a newly created room, as an alternative to
filling out the configuration form with `jabber-muc-get-config'.
Both of these methods unlock the room, so that other users can
enter it.

JC is the Jabber connection."
  (interactive (jabber-muc-argument-list))
  (jabber-send-iq jc group
		  "set"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  (x ((xmlns . "jabber:x:data") (type . "submit"))))
		  #'jabber-report-success "MUC instant configuration"
		  #'jabber-report-success "MUC instant configuration"))
;; jabber-muc-instant-config:1 ends here

;; [[file:jabber.org::#muc-instant-config][jabber-muc-instant-config:2]]
(add-to-list 'jabber-jid-muc-menu
   (cons "Configure groupchat" 'jabber-muc-get-config))
;; jabber-muc-instant-config:2 ends here

;; [[file:jabber.org::#muc-get-config][jabber-muc-get-config:1]]
(defun jabber-muc-get-config (jc group)
  "Ask for MUC configuration form.

JC is the Jabber connection."
  (interactive (jabber-muc-argument-list))
  (jabber-send-iq jc group
		  "get"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner")))
		  #'jabber-process-data #'jabber-muc-render-config
		  #'jabber-process-data "MUC configuration request failed"))
;; jabber-muc-get-config:1 ends here

;; [[file:jabber.org::#muc-get-config][jabber-muc-get-config:2]]
(defalias 'jabber-groupchat-get-config 'jabber-muc-get-config
  "Deprecated.  See `jabber-muc-get-config' instead.")
;; jabber-muc-get-config:2 ends here

;; [[file:jabber.org::#muc-render-config][jabber-muc-render-config:1]]
(defun jabber-muc-render-config (jc xml-data)
  "Render MUC configuration form.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let ((query (jabber-iq-query xml-data))
	xdata)
    (dolist (x (jabber-xml-get-children query 'x))
      (if (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	  (setq xdata x)))
    (if (not xdata)
	(insert "No configuration possible.\n")

    (jabber-init-widget-buffer (jabber-xml-get-attribute xml-data 'from))
    (setq jabber-buffer-connection jc)

    (jabber-render-xdata-form xdata)

    (widget-create 'push-button :notify #'jabber-muc-submit-config "Submit")
    (widget-insert "\t")
    (widget-create 'push-button :notify #'jabber-muc-cancel-config "Cancel")
    (widget-insert "\n")

    (widget-setup)
    (widget-minor-mode 1))))
;; jabber-muc-render-config:1 ends here

;; [[file:jabber.org::#muc-render-config][jabber-muc-render-config:2]]
(defalias 'jabber-groupchat-render-config 'jabber-muc-render-config
  "Deprecated.  See `jabber-muc-render-config' instead.")
;; jabber-muc-render-config:2 ends here

;; [[file:jabber.org::#muc-submit-config][jabber-muc-submit-config:1]]
(defun jabber-muc-submit-config (&rest ignore)
  "Submit MUC configuration form."

  (jabber-send-iq jabber-buffer-connection jabber-submit-to
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  ,(jabber-parse-xdata-form))
		  #'jabber-report-success "MUC configuration"
		  #'jabber-report-success "MUC configuration"))
;; jabber-muc-submit-config:1 ends here

;; [[file:jabber.org::#muc-submit-config][jabber-muc-submit-config:2]]
(defalias 'jabber-groupchat-submit-config 'jabber-muc-submit-config
  "Deprecated.  See `jabber-muc-submit-config' instead.")
;; jabber-muc-submit-config:2 ends here

;; [[file:jabber.org::#muc-cancel-config][jabber-muc-cancel-config:1]]
(defun jabber-muc-cancel-config (&rest ignore)
  "Cancel MUC configuration form."

  (jabber-send-iq jabber-buffer-connection jabber-submit-to
		  "set"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  (x ((xmlns . "jabber:x:data") (type . "cancel"))))
		  nil nil nil nil))
;; jabber-muc-cancel-config:1 ends here

;; [[file:jabber.org::#muc-cancel-config][jabber-muc-cancel-config:2]]
(defalias 'jabber-groupchat-cancel-config 'jabber-muc-cancel-config
  "Deprecated.  See `jabber-muc-cancel-config' instead.")
;; jabber-muc-cancel-config:2 ends here

;; [[file:jabber.org::#muc-cancel-config][jabber-muc-cancel-config:3]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Join groupchat" 'jabber-muc-join))
;; jabber-muc-cancel-config:3 ends here

;; [[file:jabber.org::#muc-join][jabber-muc-join:1]]
(defun jabber-muc-join (jc group nickname &optional popup)
  "Join a groupchat, or change nick.
In interactive calls, or if POPUP is non-nil, switch to the
groupchat buffer.

JC is the Jabber connection."
  (interactive
   (let ((account (jabber-read-account))
	 (group (jabber-read-jid-completing "group: ")))
     (list account group (jabber-muc-read-my-nickname account group) t)))

  ;; If the user is already in the room, we don't need as many checks.
  (if (or (assoc group *jabber-active-groupchats*)
	  ;; Or if the users asked us not to check disco info.
	  jabber-muc-disable-disco-check)
      (jabber-muc-join-3 jc group nickname nil popup)
    ;; Else, send a disco request to find out what we are connecting
    ;; to.
    (jabber-disco-get-info jc group nil #'jabber-muc-join-2
			   (list group nickname popup))))
;; jabber-muc-join:1 ends here

;; [[file:jabber.org::#muc-join][jabber-muc-join:2]]
(defalias 'jabber-groupchat-join 'jabber-muc-join
  "Deprecated.  Use `jabber-muc-join' instead.")
;; jabber-muc-join:2 ends here

;; [[file:jabber.org::#muc-join-2][jabber-muc-join-2:1]]
(defun jabber-muc-join-2 (jc closure result)
  (cl-destructuring-bind (group nickname popup) closure
    (let* ( ;; Either success...
	  (identities (car result))
	  (features (cadr result))
	  ;; ...or error
	  (condition (when (eq identities 'error) (jabber-error-condition result))))
      (cond
       ;; Maybe the room doesn't exist yet.
       ((eq condition 'item-not-found)
	(unless (or jabber-silent-mode
                    (y-or-n-p (format "%s doesn't exist.  Create it? "
                                      (jabber-jid-displayname group))))
	  (error "Non-existent groupchat")))

       ;; Maybe the room doesn't support disco.
       ((eq condition 'feature-not-implemented)
	t				;whatever... we will ignore it later
	)
       ;; Maybe another error occurred. Report it to user
       (condition
	(message "Couldn't query groupchat: %s" (jabber-parse-error result)))

       ;; Bad stanza? Without NS, for example
       ((and (eq identities 'error) (not condition))
        (message "Bad error stanza received")))

      ;; Continue only if it is really chat room.  If there was an
      ;; error, give the chat room the benefit of the doubt.  (Needed
      ;; for ejabberd's mod_irc, for example)
      (when (or condition
                (cl-find "conference" (if (sequencep identities) identities nil)
                      :key (lambda (i) (aref i 1))
                      :test #'string=))
        (let ((password
	     ;; Is the room password-protected?
	     (when (member "muc_passwordprotected" features)
	       (or
		(jabber-get-conference-data jc group nil :password)
		(read-passwd (format "Password for %s: " (jabber-jid-displayname group)))))))

	(jabber-muc-join-3 jc group nickname password popup))))))
;; jabber-muc-join-2:1 ends here

;; [[file:jabber.org::#muc-join-2][jabber-muc-join-2:2]]
(defalias 'jabber-groupchat-join-2 'jabber-muc-join-2
  "Deprecated.  See `jabber-muc-join-2' instead.")
;; jabber-muc-join-2:2 ends here

;; [[file:jabber.org::#muc-join-3][jabber-muc-join-3:1]]
(defun jabber-muc-join-3 (jc group nickname password popup)

  ;; Remember that this is a groupchat _before_ sending the stanza.
  ;; The response might come quicker than you think.

  (puthash (jabber-jid-symbol group) nickname jabber-pending-groupchats)

  (jabber-send-sexp jc
		    `(presence ((to . ,(format "%s/%s" group nickname)))
			       (x ((xmlns . "http://jabber.org/protocol/muc"))
				  ,@(when password
				      `((password () ,password))))
			       ,@(jabber-presence-children jc)))

  ;; There, stanza sent.  Now we just wait for the MUC service to
  ;; mirror the stanza.  This is handled in
  ;; `jabber-muc-process-presence', where a buffer will be created for
  ;; the room.

  ;; But if the user interactively asked to join, he/she probably
  ;; wants the buffer to pop up right now.
  (when popup
    (let ((buffer (jabber-muc-create-buffer jc group)))
      (switch-to-buffer buffer))))
;; jabber-muc-join-3:1 ends here

;; [[file:jabber.org::#muc-join-3][jabber-muc-join-3:2]]
(defalias 'jabber-groupchat-join-3 'jabber-muc-join-3
  "Deprecated.  See `jabber-muc-join-3' instead.")
;; jabber-muc-join-3:2 ends here

;; [[file:jabber.org::#muc-read-my-nickname][jabber-muc-read-my-nickname:1]]
(defun jabber-muc-read-my-nickname (jc group &optional default)
  "Read nickname for joining GROUP.
If DEFAULT is non-nil, return default nick without prompting.

JC is the Jabber connection."
  (let ((default-nickname (or
			   (jabber-get-conference-data jc group nil :nick)
			   (cdr (assoc group jabber-muc-default-nicknames))
			   (plist-get (fsm-get-state-data jc) :username))))
    (if default
        default-nickname
        (jabber-read-with-input-method (format "Nickname: (default %s) "
					   default-nickname)
				   nil nil default-nickname))))
;; jabber-muc-read-my-nickname:1 ends here

;; [[file:jabber.org::#muc-read-my-nickname][jabber-muc-read-my-nickname:2]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Change nickname" 'jabber-muc-nick))
;; jabber-muc-read-my-nickname:2 ends here

;; [[file:jabber.org::#muc-read-my-nickname][jabber-muc-read-my-nickname:3]]
(defalias 'jabber-muc-nick 'jabber-muc-join)
;; jabber-muc-read-my-nickname:3 ends here

;; [[file:jabber.org::#muc-read-my-nickname][jabber-muc-read-my-nickname:4]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Leave groupchat" 'jabber-muc-leave))
;; jabber-muc-read-my-nickname:4 ends here

;; [[file:jabber.org::#muc-leave][jabber-muc-leave:1]]
(defun jabber-muc-leave (jc group)
  "Leave a groupchat.

JC is the Jabber connection."
  (interactive (jabber-muc-argument-list))
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    ;; send unavailable presence to our own nick in room
    (jabber-send-sexp jc
		      `(presence ((to . ,(format "%s/%s" group (cdr whichgroup)))
				  (type . "unavailable"))))))
;; jabber-muc-leave:1 ends here

;; [[file:jabber.org::#muc-leave][jabber-muc-leave:2]]
(defalias 'jabber-groupchat-leave 'jabber-muc-leave
  "Deprecated.  Use `jabber-muc-leave' instead.")
;; jabber-muc-leave:2 ends here

;; [[file:jabber.org::#muc-leave][jabber-muc-leave:3]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "List participants" 'jabber-muc-names))
;; jabber-muc-leave:3 ends here

;; [[file:jabber.org::#muc-names][jabber-muc-names:1]]
(defun jabber-muc-names ()
  "Print names, affiliations, and roles of participants in current buffer."
  (interactive)
  (ewoc-enter-last jabber-chat-ewoc (list :notice
					  (jabber-muc-print-names
					   (cdr (assoc jabber-group jabber-muc-participants)))
					  :time (current-time))))
;; jabber-muc-names:1 ends here

;; [[file:jabber.org::#muc-format-names][jabber-muc-format-names:1]]
(defun jabber-muc-format-names (participant)
  "Format one participant name."
  (format-spec jabber-muc-print-names-format
               (list
                (cons ?n (car participant))
                (cons ?a (plist-get (cdr participant) 'affiliation))
                (cons ?j (or (plist-get (cdr participant) 'jid) "")))))
;; jabber-muc-format-names:1 ends here

;; [[file:jabber.org::#muc-print-names][jabber-muc-print-names:1]]
(defun jabber-muc-print-names (participants)
  "Format and return data in PARTICIPANTS."
  (let ((mlist) (plist) (vlist) (nlist))
    (mapcar (lambda (x)
              (let ((role (plist-get (cdr x) 'role)))
                (cond ((string= role "moderator")
                       (add-to-list 'mlist x))
                      ((string= role "participant")
                       (add-to-list 'plist x))
                      ((string= role "visitor")
                       (add-to-list 'vlist x))
                      ((string= role "none")
                       (add-to-list 'nlist x)))))
            participants)
    (concat
     (apply 'concat "\nModerators:\n" (mapcar 'jabber-muc-format-names mlist))
     (apply 'concat "\nParticipants:\n" (mapcar 'jabber-muc-format-names plist))
     (apply 'concat "\nVisitors:\n" (mapcar 'jabber-muc-format-names vlist))
     (apply 'concat "\nNones:\n" (mapcar 'jabber-muc-format-names nlist)))))
;; jabber-muc-print-names:1 ends here

;; [[file:jabber.org::#muc-print-names][jabber-muc-print-names:2]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Set topic" 'jabber-muc-set-topic))
;; jabber-muc-print-names:2 ends here

;; [[file:jabber.org::#muc-set-topic][jabber-muc-set-topic:1]]
(defun jabber-muc-set-topic (jc group topic)
  "Set topic of GROUP to TOPIC.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-read-with-input-method "New topic: " jabber-muc-topic))))
  (jabber-send-message jc group topic nil "groupchat"))
;; jabber-muc-set-topic:1 ends here

;; [[file:jabber.org::#muc-snarf-topic][jabber-muc-snarf-topic:1]]
(defun jabber-muc-snarf-topic (xml-data)
  "Record subject (topic) of the given <message/>, if any.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((new-topic (jabber-xml-path xml-data '(subject ""))))
    (when new-topic
      (setq jabber-muc-topic new-topic))))
;; jabber-muc-snarf-topic:1 ends here

;; [[file:jabber.org::#muc-snarf-topic][jabber-muc-snarf-topic:2]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Set role (kick, voice, op)" 'jabber-muc-set-role))
;; jabber-muc-snarf-topic:2 ends here

;; [[file:jabber.org::#muc-set-role][jabber-muc-set-role:1]]
(defun jabber-muc-set-role (jc group nickname role reason)
  "Set role of NICKNAME in GROUP to ROLE, specifying REASON.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (let ((nickname (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (list nickname
	    (completing-read "New role: " '(("none") ("visitor") ("participant") ("moderator")) nil t nil 'jabber-role-history)
	    (read-string "Reason: ")))))
  (unless (or (zerop (length nickname)) (zerop (length role)))
    (jabber-send-iq jc group "set"
		    `(query ((xmlns . "http://jabber.org/protocol/muc#admin"))
			    (item ((nick . ,nickname)
				   (role . ,role))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Role change"
		    'jabber-report-success "Role change")))
;; jabber-muc-set-role:1 ends here

;; [[file:jabber.org::#muc-set-role][jabber-muc-set-role:2]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Set affiliation (ban, member, admin)" 'jabber-muc-set-affiliation))
;; jabber-muc-set-role:2 ends here

;; [[file:jabber.org::#muc-set-affiliation][jabber-muc-set-affiliation:1]]
(defun jabber-muc-set-affiliation (jc group nickname-or-jid nickname-p affiliation reason)
  "Set affiliation of NICKNAME-OR-JID in GROUP to AFFILIATION.
If NICKNAME-P is non-nil, NICKNAME-OR-JID is a nickname in the
group, else it is a JID.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (let ((nickname-p (y-or-n-p "Specify user by room nickname? ")))
      (list
       (if nickname-p
	   (jabber-muc-read-nickname jabber-group "Nickname: ")
	 (jabber-read-jid-completing "User: "))
       nickname-p
       (completing-read "New affiliation: "
			'(("none") ("outcast") ("member") ("admin") ("owner")) nil t nil 'jabber-affiliation-history)
       (read-string "Reason: ")))))
  (let ((jid
	 (if nickname-p
	     (let ((participants (cdr (assoc group jabber-muc-participants))))
	       (unless participants
		 (error "Couldn't find group %s" group))
	       (let ((participant (cdr (assoc nickname-or-jid participants))))
		 (unless participant
		   (error "Couldn't find %s in group %s" nickname-or-jid group))
		 (or (plist-get participant 'jid)
		     (error "JID of %s in group %s is unknown" nickname-or-jid group))))
	   nickname-or-jid)))
    (jabber-send-iq jc group "set"
		    `(query ((xmlns . "http://jabber.org/protocol/muc#admin"))
			    (item ((jid . ,jid)
				   (affiliation . ,affiliation))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Affiliation change"
		    'jabber-report-success "Affiliation change")))
;; jabber-muc-set-affiliation:1 ends here

;; [[file:jabber.org::#muc-set-affiliation][jabber-muc-set-affiliation:2]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Invite someone to chatroom" 'jabber-muc-invite))
;; jabber-muc-set-affiliation:2 ends here

;; [[file:jabber.org::#muc-invite][jabber-muc-invite:1]]
(defun jabber-muc-invite (jc jid group reason)
  "Invite JID to GROUP, stating REASON.

JC is the Jabber connection."
  (interactive
   (list (jabber-read-account)
	 (jabber-read-jid-completing
          "Invite whom: "
          ;; The current room is _not_ a good default for whom to invite.
          (remq (jabber-jid-symbol jabber-group) (jabber-concat-rosters)))
	 (jabber-muc-read-completing "To group: ")
	 (jabber-read-with-input-method "Reason: ")))
  (jabber-send-sexp
   jc
   `(message ((to . ,group))
	     (x ((xmlns . "http://jabber.org/protocol/muc#user"))
		(invite ((to . ,jid))
			,(unless (zerop (length reason))
			   `(reason nil ,reason)))))))
;; jabber-muc-invite:1 ends here

;; [[file:jabber.org::#muc-invite][jabber-muc-invite:2]]
(add-to-list 'jabber-body-printers 'jabber-muc-print-invite)
;; jabber-muc-invite:2 ends here

;; [[file:jabber.org::#muc-print-invite][jabber-muc-print-invite:1]]
(defun jabber-muc-print-invite (xml-data who mode)
  "Print MUC invitation.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (dolist (x (jabber-xml-get-children xml-data 'x))
    (when (string= (jabber-xml-get-attribute x 'xmlns) "http://jabber.org/protocol/muc#user")
      (let ((invitation (car (jabber-xml-get-children x 'invite))))
	(when invitation
	  (when (eql mode :insert)
	    (let ((group (jabber-xml-get-attribute xml-data 'from))
		  (inviter (jabber-xml-get-attribute invitation 'from))
		  (reason (car (jabber-xml-node-children (car (jabber-xml-get-children invitation 'reason))))))
	      ;; XXX: password
	      (insert "You have been invited to MUC room " (jabber-jid-displayname group))
	      (when inviter
		(insert " by " (jabber-jid-displayname inviter)))
	      (insert ".")
	      (when reason
		(insert "  Reason: " reason))
	      (insert "\n\n")

	      (let ((action
		     `(lambda (&rest ignore) (interactive)
			(jabber-muc-join jabber-buffer-connection ,group
					       (jabber-muc-read-my-nickname jabber-buffer-connection ,group)))))
		(if (fboundp 'insert-button)
		    (insert-button "Accept"
				   'action action)
		  ;; Simple button replacement
		  (let ((keymap (make-keymap)))
		    (define-key keymap "\r" action)
		    (insert (jabber-propertize "Accept"
					       'keymap keymap
					       'face 'highlight))))

		(insert "\t")

		(let ((action
		       `(lambda (&rest ignore) (interactive)
			  (let ((reason
				 (jabber-read-with-input-method
				  "Reason: ")))
			    (jabber-send-sexp
			     jabber-buffer-connection
			     (list 'message
				   (list (cons 'to ,group))
				   (list 'x
					 (list (cons 'xmlns "http://jabber.org/protocol/muc#user"))
					 (list 'decline
					       (list (cons 'to ,inviter))
					       (unless (zerop (length reason))
						 (list 'reason nil reason))))))))))
		  (if (fboundp 'insert-button)
		      (insert-button "Decline"
				     'action action)
		    ;; Simple button replacement
		    (let ((keymap (make-keymap)))
		      (define-key keymap "\r" action)
		      (insert (jabber-propertize "Decline"
						 'keymap keymap
						 'face 'highlight))))))))
	  (cl-return t))))))
;; jabber-muc-print-invite:1 ends here

;; [[file:jabber.org::#muc-autojoin-1][jabber-muc-autojoin:1]]
(defun jabber-muc-autojoin (jc)
  "Join rooms specified in account bookmarks and global `jabber-muc-autojoin'.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (let ((nickname (plist-get (fsm-get-state-data jc) :username)))
    (when (bound-and-true-p jabber-muc-autojoin)
      (dolist (group jabber-muc-autojoin)
	(jabber-muc-join jc group (or
					 (cdr (assoc group jabber-muc-default-nicknames))
					 (plist-get (fsm-get-state-data jc) :username)))))
    (jabber-get-bookmarks
     jc
     (lambda (jc bookmarks)
       (dolist (bookmark bookmarks)
	 (setq bookmark (jabber-parse-conference-bookmark bookmark))
	 (when (and bookmark (plist-get bookmark :autojoin))
	   (jabber-muc-join jc (plist-get bookmark :jid)
				  (or (plist-get bookmark :nick)
				      (plist-get (fsm-get-state-data jc) :username)))))))))
;; jabber-muc-autojoin:1 ends here

;; [[file:jabber.org::#muc-message-p][jabber-muc-message-p:1]]
;;;###autoload
(defun jabber-muc-message-p (message)
  "Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites."
  ;; Public groupchat messages have type "groupchat" and are from
  ;; room@server/nick.  Public groupchat errors have type "error" and
  ;; are from room@server.
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (or
     (string= type "groupchat")
     (and (string= type "error")
	  (gethash (jabber-jid-symbol from) jabber-pending-groupchats))
     (jabber-xml-path message '(("http://jabber.org/protocol/muc#user" . "x") invite)))))
;; jabber-muc-message-p:1 ends here

;; [[file:jabber.org::#muc-sender-p][jabber-muc-sender-p:1]]
;;;###autoload
(defun jabber-muc-sender-p (jid)
  "Return non-nil if JID is a full JID of an MUC participant."
  (and (assoc (jabber-jid-user jid) *jabber-active-groupchats*)
       (jabber-jid-resource jid)))
;; jabber-muc-sender-p:1 ends here

;; [[file:jabber.org::#muc-private-message-p][jabber-muc-private-message-p:1]]
;;;###autoload
(defun jabber-muc-private-message-p (message)
  "Return non-nil if MESSAGE is a private message in a groupchat."
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (and
     (not (string= type "groupchat"))
     (jabber-muc-sender-p from))))
;; jabber-muc-private-message-p:1 ends here

;; [[file:jabber.org::#muc-private-message-p][jabber-muc-private-message-p:2]]
(add-to-list 'jabber-jid-muc-menu
	     (cons "Open private chat" 'jabber-muc-private))
;; jabber-muc-private-message-p:2 ends here

;; [[file:jabber.org::#muc-private][jabber-muc-private:1]]
(defun jabber-muc-private (jc group nickname)
  "Open private chat with NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (switch-to-buffer (jabber-muc-private-create-buffer jabber-buffer-connection group nickname)))
;; jabber-muc-private:1 ends here

;; [[file:jabber.org::#muc-presence-p][jabber-muc-presence-p:1]]
(defun jabber-muc-presence-p (presence)
  "Return non-nil if PRESENCE is presence from groupchat."
  (let ((from (jabber-xml-get-attribute presence 'from))
	(type (jabber-xml-get-attribute presence 'type))
	(muc-marker (cl-find-if
		     (lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
				   "http://jabber.org/protocol/muc#user"))
		     (jabber-xml-get-children presence 'x))))
    ;; This is MUC presence if it has an MUC-namespaced tag...
    (or muc-marker
	;; ...or if it is error presence from a room we tried to join.
	(and (string= type "error")
	     (gethash (jabber-jid-symbol from) jabber-pending-groupchats)))))
;; jabber-muc-presence-p:1 ends here

;; [[file:jabber.org::#muc-parse-affiliation][jabber-muc-parse-affiliation:1]]
(defun jabber-muc-parse-affiliation (x-muc)
  "Parse X-MUC in the muc#user namespace and return a plist.
Return nil if X-MUC is nil."
  ;; XXX: parse <actor/> and <reason/> tags?  or maybe elsewhere?
  (apply 'nconc (mapcar (lambda (prop) (list (car prop) (cdr prop)))
			(jabber-xml-node-attributes
			 (car (jabber-xml-get-children x-muc 'item))))))
;; jabber-muc-parse-affiliation:1 ends here

;; [[file:jabber.org::#muc-print-prompt][jabber-muc-print-prompt:1]]
(defun jabber-muc-print-prompt (xml-data &optional local dont-print-nick-p)
  "Print MUC prompt for message in XML-DATA."
  (let ((nick (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
	(timestamp (jabber-message-timestamp xml-data)))
    (if (stringp nick)
	(insert (jabber-propertize
		 (format-spec jabber-groupchat-prompt-format
			      (list
			       (cons ?t (format-time-string
					 (if timestamp
					     jabber-chat-delayed-time-format
					   jabber-chat-time-format)
					 timestamp))
			       (cons ?n (if dont-print-nick-p "" nick))
			       (cons ?u nick)
			       (cons ?r nick)
			       (cons ?j (concat jabber-group "/" nick))))
		 'face (if local        ;Message from you.
                           (if jabber-muc-colorize-local ;; If colorization enable...
                               ;; ...colorize nick
                               (list ':foreground (jabber-muc-nick-get-color nick))
                             ;; otherwise, use default face.
                             'jabber-chat-prompt-local)
                         ;; Message from other participant.
                         (if jabber-muc-colorize-foreign ;If colorization enable...
                             ;; ... colorize nick
                             (list ':foreground (jabber-muc-nick-get-color nick))
                           ;; otherwise, use default face.
                           'jabber-chat-prompt-foreign))
		 'help-echo (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " nick " in " jabber-group)))
      (jabber-muc-system-prompt))))
;; jabber-muc-print-prompt:1 ends here

;; [[file:jabber.org::#muc-private-print-prompt][jabber-muc-private-print-prompt:1]]
(defun jabber-muc-private-print-prompt (xml-data)
  "Print prompt for private MUC message in XML-DATA."
  (let ((nick (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
	(group (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
	(timestamp (jabber-message-timestamp xml-data)))
    (insert (jabber-propertize
	     (format-spec jabber-muc-private-foreign-prompt-format
			  (list
			   (cons ?t (format-time-string
				     (if timestamp
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n nick)
			   (cons ?g (or (jabber-jid-rostername group)
					(jabber-jid-username group)))))
	     'face 'jabber-chat-prompt-foreign
	     'help-echo (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " nick " in " jabber-group)))))
;; jabber-muc-private-print-prompt:1 ends here

;; [[file:jabber.org::#muc-system-prompt][jabber-muc-system-prompt:1]]
(defun jabber-muc-system-prompt (&rest ignore)
  "Print system prompt for MUC."
  (insert (jabber-propertize
	   (format-spec jabber-groupchat-prompt-format
			(list
			 (cons ?t (format-time-string jabber-chat-time-format))
			 (cons ?n "")
			 (cons ?u "")
			 (cons ?r "")
			 (cons ?j jabber-group)))
	   'face 'jabber-chat-prompt-system
	   'help-echo (format-time-string "System message on %Y-%m-%d %H:%M:%S"))))
;; jabber-muc-system-prompt:1 ends here

;; [[file:jabber.org::#muc-system-prompt][jabber-muc-system-prompt:2]]
(add-to-list 'jabber-message-chain 'jabber-muc-process-message)
;; jabber-muc-system-prompt:2 ends here

;; [[file:jabber.org::#muc-process-message][jabber-muc-process-message:1]]
(defun jabber-muc-process-message (jc xml-data)
  "If XML-DATA is a groupchat message, handle it as such.

JC is the Jabber connection."
  (when (jabber-muc-message-p xml-data)
    (defvar printers nil)
    (let* ((from (jabber-xml-get-attribute xml-data 'from))
	   (group (jabber-jid-user from))
	   (nick (jabber-jid-resource from))
	   (error-p (jabber-xml-get-children xml-data 'error))
	   (type (cond
		  (error-p :muc-error)
		  ((string= nick (cdr (assoc group *jabber-active-groupchats*)))
		   :muc-local)
		  (t :muc-foreign)))
	   (body-text (car (jabber-xml-node-children
			   (car (jabber-xml-get-children
				 xml-data 'body)))))

	   (printers (append jabber-muc-printers jabber-chat-printers)))

      (with-current-buffer (jabber-muc-create-buffer jc group)
	(jabber-muc-snarf-topic xml-data)
	;; Call alert hooks only when something is output
	(when (or error-p
		  (run-hook-with-args-until-success 'printers xml-data type :printp))
	  (jabber-maybe-print-rare-time
	   (ewoc-enter-last jabber-chat-ewoc (list type xml-data :time (current-time))))

	  ;; ...except if the message is part of history, in which
	  ;; case we don't want an alert.
	  (let ((children-namespaces (mapcar (lambda (x) (when (listp x) (jabber-xml-get-attribute x 'xmlns)))
					     (jabber-xml-node-children xml-data))))
	    (unless (or (member "urn:xmpp:delay" children-namespaces)
			(member "jabber:x:delay" children-namespaces))
	      (dolist (hook '(jabber-muc-hooks jabber-alert-muc-hooks))
		(run-hook-with-args hook
				    nick group (current-buffer) body-text
				    (funcall jabber-alert-muc-function
					     nick group (current-buffer) body-text))))))))))
;; jabber-muc-process-message:1 ends here

;; [[file:jabber.org::#muc-process-presence][jabber-muc-process-presence:1]]
(defun jabber-muc-process-presence (jc presence)
  (let* ((from (jabber-xml-get-attribute presence 'from))
	 (type (jabber-xml-get-attribute presence 'type))
	 (x-muc (cl-find-if
		 (lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
			       "http://jabber.org/protocol/muc#user"))
		 (jabber-xml-get-children presence 'x)))
	 (group (jabber-jid-user from))
	 (nickname (jabber-jid-resource from))
	 (symbol (jabber-jid-symbol from))
	 (our-nickname (gethash symbol jabber-pending-groupchats))
	 (item (car (jabber-xml-get-children x-muc 'item)))
	 (actor (jabber-xml-get-attribute (car (jabber-xml-get-children item 'actor)) 'jid))
	 (reason (car (jabber-xml-node-children (car (jabber-xml-get-children item 'reason)))))
	 (error-node (car (jabber-xml-get-children presence 'error)))
	 (status-codes (if error-node
			   (list (jabber-xml-get-attribute error-node 'code))
			 (mapcar
			  (lambda (status-element)
			    (jabber-xml-get-attribute status-element 'code))
			  (jabber-xml-get-children x-muc 'status)))))
    ;; handle leaving a room
    (cond
     ((or (string= type "unavailable") (string= type "error"))
      ;; error from room itself? or are we leaving?
      (if (or (null nickname)
	      (member "110" status-codes)
	      (string= nickname our-nickname))
	  ;; Assume that an error means that we were thrown out of the
	  ;; room...
	  (let* ((leavingp t)
		 (message (cond
			   ((string= type "error")
			    (cond
			     ;; ...except for certain cases.
			     ((or (member "406" status-codes)
				  (member "409" status-codes))
			      (setq leavingp nil)
			      (concat "Nickname change not allowed"
				      (when error-node
					(concat ": " (jabber-parse-error error-node)))))
			     (t
			      (concat "Error entering room"
				      (when error-node
					(concat ": " (jabber-parse-error error-node)))))))
			   ((member "301" status-codes)
			    (concat "You have been banned"
				    (when actor (concat " by " actor))
				    (when reason (concat " - '" reason "'"))))
			   ((member "307" status-codes)
			    (concat "You have been kicked"
				    (when actor (concat " by " actor))
				    (when reason (concat " - '" reason "'"))))
			   (t
			    "You have left the chatroom"))))
	    (when leavingp
	      (jabber-muc-remove-groupchat group))
	    ;; If there is no buffer for this groupchat, don't bother
	    ;; creating one just to tell that user left the room.
	    (let ((buffer (get-buffer (jabber-muc-get-buffer group))))
	      (if buffer
		  (with-current-buffer buffer
		    (jabber-maybe-print-rare-time
		     (ewoc-enter-last jabber-chat-ewoc
				      (list (if (string= type "error")
						:muc-error
					      :muc-notice)
					    message
					    :time (current-time)))))
		(message "%s: %s" (jabber-jid-displayname group) message))))
	;; or someone else?
	(let* ((plist (jabber-muc-participant-plist group nickname))
	       (jid (plist-get plist 'jid))
	       (name (concat nickname
			     (when jid
			       (concat " <"
				       (jabber-jid-user jid)
				       ">")))))
	  (jabber-muc-remove-participant group nickname)
	  (with-current-buffer (jabber-muc-create-buffer jc group)
	    (jabber-maybe-print-rare-time
	     (ewoc-enter-last
	      jabber-chat-ewoc
	      (list :muc-notice
		    (cond
		     ((member "301" status-codes)
		      (concat name " has been banned"
			      (when actor (concat " by " actor))
			      (when reason (concat " - '" reason "'"))))
		     ((member "307" status-codes)
		      (concat name " has been kicked"
			      (when actor (concat " by " actor))
			      (when reason (concat " - '" reason "'"))))
		     ((member "303" status-codes)
		      (concat name " changes nickname to "
			      (jabber-xml-get-attribute item 'nick)))
		     (t
		      (concat name " has left the chatroom")))
		    :time (current-time))))))))
     (t
      ;; someone is entering

      (when (or (member "110" status-codes) (string= nickname our-nickname))
	;; This is us.  We just succeeded in entering the room.
	;;
	;; The MUC server is supposed to send a 110 code whenever this
	;; is our presence ("self-presence"), but at least one
	;; (ejabberd's mod_irc) doesn't, so check the nickname as well.
	;;
	;; This check might give incorrect results if the server
	;; changed our nickname to avoid collision with an existing
	;; participant, but even in this case the window where we have
	;; incorrect information should be very small, as we should be
	;; getting our own 110+210 presence shortly.
	(let ((whichgroup (assoc group *jabber-active-groupchats*)))
	  (if whichgroup
	      (setcdr whichgroup nickname)
	    (add-to-list '*jabber-active-groupchats* (cons group nickname))))
	;; The server may have changed our nick.  Record the new one.
	(puthash symbol nickname jabber-pending-groupchats))

      ;; Whoever enters, we create a buffer (if it didn't already
      ;; exist), and print a notice.  This is where autojoined MUC
      ;; rooms have buffers created for them.  We also remember some
      ;; metadata.
      (let ((old-plist (jabber-muc-participant-plist group nickname))
	    (new-plist (jabber-muc-parse-affiliation x-muc)))
	(jabber-muc-modify-participant group nickname new-plist)
	(let ((report (jabber-muc-report-delta nickname old-plist new-plist
					       reason actor)))
	  (when report
	    (with-current-buffer (jabber-muc-create-buffer jc group)
	      (jabber-maybe-print-rare-time
	       (ewoc-enter-last
		jabber-chat-ewoc
		(list :muc-notice report
		      :time (current-time))))
	      ;; Did the server change our nick?
	      (when (member "210" status-codes)
		(ewoc-enter-last
		 jabber-chat-ewoc
		 (list :muc-notice
		       (concat "Your nick was changed to " nickname " by the server")
		       :time (current-time))))
	      ;; Was this room just created?  If so, it's a locked
	      ;; room.  Notify the user.
	      (when (member "201" status-codes)
		(ewoc-enter-last
		 jabber-chat-ewoc
		 (list :muc-notice
		       (with-temp-buffer
			 (insert "This room was just created, and is locked to other participants.\n"
				 "To unlock it, ")
			 (insert-text-button
			  "configure the room"
			  'action (apply-partially 'call-interactively 'jabber-muc-get-config))
			 (insert " or ")
			 (insert-text-button
			  "accept the default configuration"
			  'action (apply-partially 'call-interactively 'jabber-muc-instant-config))
			 (insert ".")
			 (buffer-string))
		       :time (current-time))))))))))))
;; jabber-muc-process-presence:1 ends here

;; [[file:jabber.org::#muc-completion-delimiter][jabber-muc-completion-delimiter:1]]
(defcustom jabber-muc-completion-delimiter ": "
  "String to add to end of completion line."
  :type 'string
  :group 'jabber-chat)
;; jabber-muc-completion-delimiter:1 ends here

;; [[file:jabber.org::#muc-looks-personaling-symbols][jabber-muc-looks-personaling-symbols:1]]
(defcustom jabber-muc-looks-personaling-symbols '("," ":" ">")
  "Symbols for personaling messages."
  :type '(repeat string)
  :group 'jabber-chat)
;; jabber-muc-looks-personaling-symbols:1 ends here

;; [[file:jabber.org::#muc-personal-message-bonus][jabber-muc-personal-message-bonus:1]]
(defcustom jabber-muc-personal-message-bonus (* 60 20)
  "Bonus for personal message, in seconds."
  :type 'integer
  :group 'jabber-chat)
;; jabber-muc-personal-message-bonus:1 ends here

;; [[file:jabber.org::#muc-all-string][jabber-muc-all-string:1]]
(defcustom jabber-muc-all-string "all"
  "String meaning all conference members (to insert in completion).
Note that \":\" or alike not needed (it appended in other string)"
  :type 'string
  :group 'jabber-chat)

;;; History:
;;

;;; Code:

(require 'hippie-exp)
;; jabber-muc-all-string:1 ends here

;; [[file:jabber.org::#*jabber-muc-participant-last-speaking*][*jabber-muc-participant-last-speaking*:1]]
(defvar *jabber-muc-participant-last-speaking* nil
  "Global alist in form (group . ((member . time-of-last-speaking) ...) ...).")
;; *jabber-muc-participant-last-speaking*:1 ends here

;; [[file:jabber.org::#my-nick][jabber-my-nick:1]]
(defun jabber-my-nick (&optional group)
  "Return my jabber nick in GROUP."
  (let ((room (or group jabber-group)))
    (cdr (or (assoc room *jabber-active-groupchats*)
             (assoc room jabber-muc-default-nicknames)))))
;; jabber-my-nick:1 ends here

;; [[file:jabber.org::#muc-looks-like-personal-p][jabber-muc-looks-like-personal-p:1]]
;;;###autoload
(defun jabber-muc-looks-like-personal-p (message &optional group)
  "Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look."
  (if message (string-match (concat
		 "^"
		 (jabber-my-nick group)
		 (regexp-opt jabber-muc-looks-personaling-symbols))
		message)
    nil))
;; jabber-muc-looks-like-personal-p:1 ends here

;; [[file:jabber.org::#muc-nicknames][jabber-muc-nicknames:1]]
(defun jabber-muc-nicknames ()
  "List of conference participants, excluding self, or nil if we not in conference."
  (cl-delete-if '(lambda (nick)
		 (string= nick (jabber-my-nick)))
	     (append (mapcar 'car (cdr (assoc jabber-group jabber-muc-participants))) (list jabber-muc-all-string))))
;; jabber-muc-nicknames:1 ends here

;; [[file:jabber.org::#muc-participant-update-activity][jabber-muc-participant-update-activity:1]]
(defun jabber-muc-participant-update-activity (group nick time)
  "Update NICK's time of last speaking in GROUP to TIME."
  (let* ((room (assoc group *jabber-muc-participant-last-speaking*))
         (room-activity (cdr room))
         (entry (assoc nick room-activity))
         (old-time (or (cdr entry) 0)))
    (when (> time old-time)
      ;; don't use put-alist for speed
      (progn
        (if entry (setcdr entry time)
          (setq room-activity
                (cons (cons nick time) room-activity)))
        (if room (setcdr room room-activity)
          (setq *jabber-muc-participant-last-speaking*
                (cons (cons group room-activity)
                      *jabber-muc-participant-last-speaking*)))))))
;; jabber-muc-participant-update-activity:1 ends here

;; [[file:jabber.org::#muc-track-message-time][jabber-muc-track-message-time:1]]
(defun jabber-muc-track-message-time (nick group buffer text &optional title)
  "Tracks time of NICK's last speaking in GROUP."
  (when nick
    (let ((time (float-time)))
      (jabber-muc-participant-update-activity
       group
       nick
       (if (jabber-muc-looks-like-personal-p text group)
	   (+ time jabber-muc-personal-message-bonus)
	 time)))))
;; jabber-muc-track-message-time:1 ends here

;; [[file:jabber.org::#sort-nicks][jabber-sort-nicks:1]]
(defun jabber-sort-nicks (nicks group)
  "Return list of NICKS in GROUP, sorted."
  (cl-letf* ((times (cdr (assoc group *jabber-muc-participant-last-speaking*)))
	     ((symbol-function 'fetch-time) (lambda (nick) (or (assoc nick times)
							       (cons nick 0))))
	     ((symbol-function 'cmp) (lambda (nt1 nt2)
				       (let ((t1 (cdr nt1))
					     (t2 (cdr nt2)))
					 (if (and (zerop t1) (zerop t2))
					     (string< (car nt1)
						      (car nt2))
					   (> t1 t2))))))
    (mapcar #'car (sort (mapcar #'fetch-time nicks) #'cmp))))
;; jabber-sort-nicks:1 ends here

;; [[file:jabber.org::#muc-beginning-of-line][jabber-muc-beginning-of-line:1]]
(defun jabber-muc-beginning-of-line ()
  "Return position of line begining."
  (save-excursion
    (if (looking-back jabber-muc-completion-delimiter)
        (backward-char (+ (length jabber-muc-completion-delimiter) 1)))
    (skip-syntax-backward "^-")
    (point)))
;; jabber-muc-beginning-of-line:1 ends here

;; [[file:jabber.org::#muc-beginning-of-line][jabber-muc-beginning-of-line:2]]
;;; One big hack:
(defun jabber-muc-completion-delete-last-tried ()
  "Delete last tried competion variand from line."
  (let ((last-tried (car he-tried-table)))
    (when last-tried
      (goto-char he-string-beg)
      (delete-char (length last-tried))
      (ignore-errors (delete-char (length jabber-muc-completion-delimiter))))))
;; jabber-muc-beginning-of-line:2 ends here

;; [[file:jabber.org::#try-expand-jabber-muc][try-expand-jabber-muc:1]]
(defun try-expand-jabber-muc (old)
  "Try to expand target nick in MUC according to last speaking time.
OLD is last tried nickname."
  (unless jabber-chatting-with
    (unless old
      (let ((nicknames (jabber-muc-nicknames)))
        (he-init-string (jabber-muc-beginning-of-line) (point))
        (setq he-expand-list
              (jabber-sort-nicks (all-completions he-search-string
                                            (mapcar 'list nicknames))
                           jabber-group))))

    (setq he-expand-list
	  (cl-delete-if '(lambda (x)
		           (he-string-member x he-tried-table))
		        he-expand-list))
    (if (null he-expand-list)
        (progn
          (when old
            ;; here and later : its hack to workaround
            ;; he-substitute-string work which cant substitute empty
            ;; lines
            (if (string= he-search-string "")
                (jabber-muc-completion-delete-last-tried)
              (he-reset-string)))
          ())
      (let ((subst (if (eq (line-beginning-position) (jabber-muc-beginning-of-line))
                       (concat (car he-expand-list) jabber-muc-completion-delimiter)
                     (car he-expand-list))))
        (if (not (string= he-search-string ""))
            (he-substitute-string subst)
          (jabber-muc-completion-delete-last-tried)
          (progn
            (insert subst)
            (if (looking-back (concat "^" (regexp-quote (car he-expand-list))))
                (unless (looking-back (concat "^" (regexp-quote (car he-expand-list)) jabber-muc-completion-delimiter))
                  (insert jabber-muc-completion-delimiter))))))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))
;; try-expand-jabber-muc:1 ends here

;; [[file:jabber.org::#try-expand-jabber-muc][try-expand-jabber-muc:2]]
(add-hook 'jabber-muc-hooks 'jabber-muc-track-message-time)
(fset 'jabber-muc-completion (make-hippie-expand-function '(try-expand-jabber-muc)))
(define-key jabber-chat-mode-map [?\t] 'jabber-muc-completion)
;; try-expand-jabber-muc:2 ends here

;; [[file:jabber.org::#get-register][jabber-get-register:1]]
(add-to-list 'jabber-jid-service-menu
	     (cons "Register with service" 'jabber-get-register))
(defun jabber-get-register (jc to)
  "Send IQ get request in namespace \"jabber:iq:register\".

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Register with: ")))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:register")))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Registration"))
;; jabber-get-register:1 ends here

;; [[file:jabber.org::#process-register-or-search][jabber-process-register-or-search:1]]
(defun jabber-process-register-or-search (jc xml-data)
  "Display results from jabber:iq:{register,search} query as a form.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let ((query (jabber-iq-query xml-data))
	(have-xdata nil)
	(type (cond
	       ((string= (jabber-iq-xmlns xml-data) "jabber:iq:register")
		'register)
	       ((string= (jabber-iq-xmlns xml-data) "jabber:iq:search")
		'search)
	       (t
		(error "Namespace %s not handled by jabber-process-register-or-search" (jabber-iq-xmlns xml-data)))))
	(register-account
	 (plist-get (fsm-get-state-data jc) :registerp))
	(username
	 (plist-get (fsm-get-state-data jc) :username))
	(server
	 (plist-get (fsm-get-state-data jc) :server)))

    (cond
     ((eq type 'register)
      ;; If there is no `from' attribute, we are registering with the server
      (jabber-init-widget-buffer (or (jabber-xml-get-attribute xml-data 'from)
				     server)))

     ((eq type 'search)
      ;; no such thing here
      (jabber-init-widget-buffer (jabber-xml-get-attribute xml-data 'from))))

    (setq jabber-buffer-connection jc)

    (widget-insert (if (eq type 'register) "Register with " "Search ") jabber-submit-to "\n\n")

    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq have-xdata t)
	;; If the registration form obeys XEP-0068, we know
	;; for sure how to put a default username in it.
	(jabber-render-xdata-form x
				  (if (and register-account
					   (string= (jabber-xdata-formtype x) "jabber:iq:register"))
				      (list (cons "username" username))
				    nil))))
    (if (not have-xdata)
	(jabber-render-register-form query
				     (when register-account
				       username)))

    (widget-create 'push-button :notify (if (eq type 'register)
					    #'jabber-submit-register
					  #'jabber-submit-search) "Submit")
    (when (eq type 'register)
      (widget-insert "\t")
      (widget-create 'push-button :notify #'jabber-remove-register "Cancel registration"))
    (widget-insert "\n")
    (widget-setup)
    (widget-minor-mode 1)))
;; jabber-process-register-or-search:1 ends here

;; [[file:jabber.org::#submit-register][jabber-submit-register:1]]
(defun jabber-submit-register (&rest ignore)
  "Submit registration input.  See `jabber-process-register-or-search'."

  (let* ((registerp (plist-get (fsm-get-state-data jabber-buffer-connection) :registerp))
	 (handler (if registerp
		      #'jabber-process-register-secondtime
		    #'jabber-report-success))
	 (text (concat "Registration with " jabber-submit-to)))
    (jabber-send-iq jabber-buffer-connection jabber-submit-to
		    "set"

		    (cond
		     ((eq jabber-form-type 'register)
		      `(query ((xmlns . "jabber:iq:register"))
			      ,@(jabber-parse-register-form)))
		     ((eq jabber-form-type 'xdata)
		      `(query ((xmlns . "jabber:iq:register"))
			      ,(jabber-parse-xdata-form)))
		     (t
		      (error "Unknown form type: %s" jabber-form-type)))
		    handler (if registerp 'success text)
		    handler (if registerp 'failure text)))

  (message "Registration sent"))
;; jabber-submit-register:1 ends here

;; [[file:jabber.org::#process-register-secondtime][jabber-process-register-secondtime:1]]
(defun jabber-process-register-secondtime (jc xml-data closure-data)
  "Receive registration success or failure.
CLOSURE-DATA is either 'success or 'error.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (cond
   ((eq closure-data 'success)
    (message "Registration successful.  You may now connect to the server."))
   (t
    (jabber-report-success jc xml-data "Account registration")))
  (sit-for 3)
    (jabber-disconnect-one jc))
;; jabber-process-register-secondtime:1 ends here

;; [[file:jabber.org::#remove-register][jabber-remove-register:1]]
(defun jabber-remove-register (&rest ignore)
  "Cancel registration.  See `jabber-process-register-or-search'."

  (if (or jabber-silent-mode (yes-or-no-p (concat "Are you sure that you want to cancel your registration to " jabber-submit-to "? ")))
      (jabber-send-iq jabber-buffer-connection jabber-submit-to
		      "set"
		      '(query ((xmlns . "jabber:iq:register"))
			      (remove))
		      #'jabber-report-success "Unregistration"
		      #'jabber-report-success "Unregistration")))
;; jabber-remove-register:1 ends here

;; [[file:jabber.org::#get-search][jabber-get-search:1]]
(add-to-list 'jabber-jid-service-menu
	     (cons "Search directory" 'jabber-get-search))
(defun jabber-get-search (jc to)
  "Send IQ get request in namespace \"jabber:iq:search\".

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Search what database: ")))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:search")))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Search field retrieval"))
;; jabber-get-search:1 ends here

;; [[file:jabber.org::#submit-search][jabber-submit-search:1]]
(defun jabber-submit-search (&rest ignore)
  "Submit search.  See `jabber-process-register-or-search'."

  (let ((text (concat "Search at " jabber-submit-to)))
    (jabber-send-iq jabber-buffer-connection jabber-submit-to
		    "set"

		    (cond
		     ((eq jabber-form-type 'register)
		      `(query ((xmlns . "jabber:iq:search"))
			      ,@(jabber-parse-register-form)))
		     ((eq jabber-form-type 'xdata)
		      `(query ((xmlns . "jabber:iq:search"))
			      ,(jabber-parse-xdata-form)))
		     (t
		      (error "Unknown form type: %s" jabber-form-type)))
		    #'jabber-process-data #'jabber-process-search-result
		    #'jabber-report-success text))

  (message "Search sent"))
;; jabber-submit-search:1 ends here

;; [[file:jabber.org::#process-search-result][jabber-process-search-result:1]]
(defun jabber-process-search-result (jc xml-data)
  "Receive and display search results.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  ;; This function assumes that all search results come in one packet,
  ;; which is not necessarily the case.
  (let ((query (jabber-iq-query xml-data))
	(have-xdata nil)
	xdata fields (jid-fields 0))

    ;; First, check for results in jabber:x:data form.
    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq have-xdata t)
	(setq xdata x)))

    (if have-xdata
	(jabber-render-xdata-search-results xdata)

      (insert (jabber-propertize "Search results" 'face 'jabber-title-medium) "\n")

      (setq fields '((first . (label "First name" column 0))
		     (last . (label "Last name" column 15))
		     (nick . (label "Nickname" column 30))
		     (jid . (label "JID" column 45))
		     (email . (label "E-mail" column 65))))
      (setq jid-fields 1)

      (dolist (field-cons fields)
	(indent-to (plist-get (cdr field-cons) 'column) 1)
	(insert (jabber-propertize (plist-get (cdr field-cons) 'label) 'face 'bold)))
      (insert "\n\n")

      ;; Now, the items
      (dolist (item (jabber-xml-get-children query 'item))
	(let ((start-of-line (point))
	      jid)

	    (dolist (field-cons fields)
	      (let ((field-plist (cdr field-cons))
		    (value (if (eq (car field-cons) 'jid)
			       (setq jid (jabber-xml-get-attribute item 'jid))
			     (car (jabber-xml-node-children (car (jabber-xml-get-children item (car field-cons))))))))
		(indent-to (plist-get field-plist 'column) 1)
		(if value (insert value))))

	    (if jid
		(put-text-property start-of-line (point)
				   'jabber-jid jid))
	    (insert "\n"))))))
;; jabber-process-search-result:1 ends here

;; [[file:jabber.org::#get-browse][jabber-get-browse:1]]
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
;; jabber-get-browse:1 ends here

;; [[file:jabber.org::#process-browse][jabber-process-browse:1]]
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
;; jabber-process-browse:1 ends here

;; [[file:jabber.org::#software-version-()][Software Version ([[https://xmpp.org/extensions/xep-0092.html][XEP-0092]]):1]]
(require 'jabber-ourversion)
;; Software Version ([[https://xmpp.org/extensions/xep-0092.html][XEP-0092]]):1 ends here

;; [[file:jabber.org::#version-show][jabber-version-show:1]]
(defcustom jabber-version-show t
  "Show our client version to others.  Acts on loading."
  :type 'boolean
  :group 'jabber)
;; jabber-version-show:1 ends here

;; [[file:jabber.org::#get-version][jabber-get-version:1]]
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
;; jabber-get-version:1 ends here

;; [[file:jabber.org::#process-version][jabber-process-version:1]]
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
;; jabber-process-version:1 ends here

;; [[file:jabber.org::#process-version][jabber-process-version:2]]
(if jabber-version-show
    (and
     (add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:version" 'jabber-return-version))
     (jabber-disco-advertise-feature "jabber:iq:version")))
;; jabber-process-version:2 ends here

;; [[file:jabber.org::#return-version][jabber-return-version:1]]
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
;; jabber-return-version:1 ends here

;; [[file:jabber.org::#ahc-sessionid][jabber-ahc-sessionid:1]]
(defvar jabber-ahc-sessionid nil
  "Session ID of Ad-Hoc Command session.")
;; jabber-ahc-sessionid:1 ends here

;; [[file:jabber.org::#ahc-node][jabber-ahc-node:1]]
(defvar jabber-ahc-node nil
  "Node to send commands to.")
;; jabber-ahc-node:1 ends here

;; [[file:jabber.org::#ahc-commands][jabber-ahc-commands:1]]
(defvar jabber-ahc-commands nil
  "Commands provided.

This is an alist, where the keys are node names as strings (which
means that they must not conflict).  The values are plists having
following properties:

acl     - function taking connection object and JID of requester,
	  returning non-nil for access allowed.  No function means
          open for everyone.
name	- name of command
func	- function taking connection object and entire IQ stanza as
          arguments and returning a <command/> node

Use the function `jabber-ahc-add' to add a command to this list.")
;; jabber-ahc-commands:1 ends here

;; [[file:jabber.org::#server][server:1]]
(add-to-list 'jabber-disco-info-nodes
	     (list "http://jabber.org/protocol/commands"
		   '((identity ((category . "automation")
				(type . "command-list")
				(name . "Ad-Hoc Command list")))
		     (feature ((var . "http://jabber.org/protocol/commands")))
		     (feature ((var . "http://jabber.org/protocol/disco#items")))
		     (feature
		      ((var . "http://jabber.org/protocol/disco#info"))))))
;; server:1 ends here

;; [[file:jabber.org::#ahc-add][jabber-ahc-add:1]]
(defun jabber-ahc-add (node name func acl)
  "Add a command to internal lists.
NODE is the node name to be used.  It must be unique.
NAME is the natural-language name of the command.
FUNC is a function taking the entire IQ stanza as single argument when
this command is invoked, and returns a <command/> node.
ACL is a function taking JID as single argument, returning non-nil for
access allowed.  nil means open for everyone."
  (add-to-list 'jabber-ahc-commands (cons node (list 'name name
						     'func func
						     'acl acl)))
  (add-to-list 'jabber-disco-info-nodes
	       (list node `((identity ((category . "automation")
				       (type . "command-node")
				       (name . ,name)))
			    (feature ((var . "http://jabber.org/protocol/commands")))
			    (feature ((var . "http://jabber.org/protocol/disco#info")))
			    (feature ((var . "jabber:x:data")))))))
;; jabber-ahc-add:1 ends here

;; [[file:jabber.org::#ahc-disco-items][jabber-ahc-disco-items:1]]
(jabber-disco-advertise-feature "http://jabber.org/protocol/commands")
(add-to-list 'jabber-disco-items-nodes
	     (list "http://jabber.org/protocol/commands" #'jabber-ahc-disco-items nil))
(defun jabber-ahc-disco-items (jc xml-data)
  "Return commands in response to disco#items request.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((jid (jabber-xml-get-attribute xml-data 'from)))
    (mapcar (function
	     (lambda (command)
	       (let ((node (car command))
		     (plist (cdr command)))
		 (let ((acl (plist-get plist 'acl))
		       (name (plist-get plist 'name))
		       (func (plist-get plist 'func)))
		   (when (or (not (functionp acl))
			     (funcall acl jc jid))
		     `(item ((name . ,name)
			     (jid . ,(jabber-connection-jid jc))
			     (node . ,node))))))))
	    jabber-ahc-commands)))
;; jabber-ahc-disco-items:1 ends here

;; [[file:jabber.org::#ahc-process][jabber-ahc-process:1]]
(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/commands" 'jabber-ahc-process))
(defun jabber-ahc-process (jc xml-data)

  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data) 'node)))
    ;; find command
    (let* ((plist (cdr (assoc node jabber-ahc-commands)))
	   (acl (plist-get plist 'acl))
	   (func (plist-get plist 'func)))
      (if plist
	  ;; found
	  (if (or (not (functionp acl))
		  (funcall acl jc to))
	      ;; access control passed
	      (jabber-send-iq jc to "result"
			      (funcall func jc xml-data)
			      nil nil nil nil id)
	    ;; ...or failed
	    (jabber-signal-error "Cancel" 'not-allowed))
	;; No such node
	(jabber-signal-error "Cancel" 'item-not-found)))))
;; jabber-ahc-process:1 ends here

;; [[file:jabber.org::#ahc-get-list][jabber-ahc-get-list:1]]
(add-to-list 'jabber-jid-service-menu
	     (cons "Request command list" 'jabber-ahc-get-list))
(defun jabber-ahc-get-list (jc to)
  "Request list of ad-hoc commands.

See XEP-0050.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Request command list from: " nil nil nil nil nil)))
  (jabber-get-disco-items jc to "http://jabber.org/protocol/commands"))
;; jabber-ahc-get-list:1 ends here

;; [[file:jabber.org::#ahc-execute-command][jabber-ahc-execute-command:1]]
(add-to-list 'jabber-jid-service-menu
	     (cons "Execute command" 'jabber-ahc-execute-command))
(defun jabber-ahc-execute-command (jc to node)
  "Execute ad-hoc command.

See XEP-0050.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Execute command of: " nil nil nil nil nil)
		     (jabber-read-node "Node of command: ")))
  (jabber-send-iq jc to
		  "set"
		  `(command ((xmlns . "http://jabber.org/protocol/commands")
			     (node . ,node)
			     (action . "execute")))
		  #'jabber-process-data #'jabber-ahc-display
		  #'jabber-process-data "Command execution failed"))
;; jabber-ahc-execute-command:1 ends here

;; [[file:jabber.org::#ahc-display][jabber-ahc-display:1]]
(defun jabber-ahc-display (jc xml-data)
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (node (jabber-xml-get-attribute query 'node))
	 (notes (jabber-xml-get-children query 'note))
	 (sessionid (jabber-xml-get-attribute query 'sessionid))
	 (status (jabber-xml-get-attribute query 'status))
	 (actions (car (jabber-xml-get-children query 'actions)))
	 xdata
	 (inhibit-read-only t))

    (make-local-variable 'jabber-ahc-sessionid)
    (setq jabber-ahc-sessionid sessionid)
    (make-local-variable 'jabber-ahc-node)
    (setq jabber-ahc-node node)
    (make-local-variable 'jabber-buffer-connection)
    (setq jabber-buffer-connection jc)

    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq xdata x)))

    (cond
     ((string= status "executing")
      (insert "Executing command\n\n"))
     ((string= status "completed")
      (insert "Command completed\n\n"))
     ((string= status "canceled")
      (insert "Command canceled\n\n")))

    (dolist (note notes)
      (let ((note-type (jabber-xml-get-attribute note 'type)))
	(cond
	 ((string= note-type "warn")
	  (insert "Warning: "))
	 ((string= note-type "error")
	  (insert "Error: ")))
	(insert (car (jabber-xml-node-children note)) "\n")))
    (insert "\n")

    (when xdata
      (jabber-init-widget-buffer from)

      (let ((formtype (jabber-xml-get-attribute xdata 'type)))
	(if (string= formtype "result")
	    (jabber-render-xdata-search-results xdata)
	  (jabber-render-xdata-form xdata)

	  (when (string= status "executing")
	    (let ((button-titles
		   (cond
		    ((null actions)
		     '(complete cancel))
		    (t
		     (let ((children (mapcar #'jabber-xml-node-name (jabber-xml-node-children actions)))
			   (default-action (jabber-xml-get-attribute actions 'execute)))
		       (if (or (null default-action) (memq (intern default-action) children))
			   children
			 (cons (intern default-action) children)))))))
	      (dolist (button-title button-titles)
		(widget-create 'push-button :notify `(lambda (&rest ignore) (jabber-ahc-submit (quote ,button-title))) (symbol-name button-title))
		(widget-insert "\t")))
	    (widget-insert "\n"))))

      (widget-setup)
      (widget-minor-mode 1))))
;; jabber-ahc-display:1 ends here

;; [[file:jabber.org::#ahc-submit][jabber-ahc-submit:1]]
(defun jabber-ahc-submit (action)
  "Submit Ad-Hoc Command."

  (jabber-send-iq jabber-buffer-connection jabber-submit-to
		  "set"
		  `(command ((xmlns . "http://jabber.org/protocol/commands")
			     (sessionid . ,jabber-ahc-sessionid)
			     (node . ,jabber-ahc-node)
			     (action . ,(symbol-name action)))
			    ,(if (and (not (eq action 'cancel))
				      (eq jabber-form-type 'xdata))
				 (jabber-parse-xdata-form)))

		  #'jabber-process-data #'jabber-ahc-display
		  #'jabber-process-data "Command execution failed"))
;; jabber-ahc-submit:1 ends here

;; [[file:jabber.org::#ahc-presence-node][jabber-ahc-presence-node:1]]
(defconst jabber-ahc-presence-node "http://jabber.org/protocol/rc#set-status"
  "Node used by function `jabber-ahc-presence'.")
;; jabber-ahc-presence-node:1 ends here

;; [[file:jabber.org::#ahc-presence-node][jabber-ahc-presence-node:2]]
(jabber-ahc-add jabber-ahc-presence-node "Set presence" 'jabber-ahc-presence
		'jabber-my-jid-p)
;; jabber-ahc-presence-node:2 ends here

;; [[file:jabber.org::#ahc-presence][jabber-ahc-presence:1]]
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
;; jabber-ahc-presence:1 ends here

;; [[file:jabber.org::#mode-line][jabber-mode-line:1]]
(defgroup jabber-mode-line nil
  "Display Jabber status in mode line"
  :group 'jabber)
;; jabber-mode-line:1 ends here

;; [[file:jabber.org::#mode-line-compact][jabber-mode-line-compact:1]]
(defcustom jabber-mode-line-compact t
  "Count contacts in fewer categories for compact view."
  :group 'jabber-mode-line
  :type 'boolean)
;; jabber-mode-line-compact:1 ends here

;; [[file:jabber.org::#mode-line-string][jabber-mode-line-string:1]]
(defvar jabber-mode-line-string nil)
;; jabber-mode-line-string:1 ends here

;; [[file:jabber.org::#mode-line-presence][jabber-mode-line-presence:1]]
(defvar jabber-mode-line-presence nil)
;; jabber-mode-line-presence:1 ends here

;; [[file:jabber.org::#mode-line-contacts][jabber-mode-line-contacts:1]]
(defvar jabber-mode-line-contacts nil)
;; jabber-mode-line-contacts:1 ends here

;; [[file:jabber.org::#mode-line-contacts][jabber-mode-line-contacts:2]]
(defadvice jabber-send-presence (after jsp-update-mode-line
				       (show status priority))
  (jabber-mode-line-presence-update))
;; jabber-mode-line-contacts:2 ends here

;; [[file:jabber.org::#mode-line-presence-update][jabber-mode-line-presence-update:1]]
(defun jabber-mode-line-presence-update ()
  (setq jabber-mode-line-presence (if (and jabber-connections (not *jabber-disconnecting*))
				      (cdr (assoc *jabber-current-show* jabber-presence-strings))
				    "Offline")))
;; jabber-mode-line-presence-update:1 ends here

;; [[file:jabber.org::#mode-line-count-contacts][jabber-mode-line-count-contacts:1]]
(defun jabber-mode-line-count-contacts (&rest ignore)
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
;; jabber-mode-line-count-contacts:1 ends here

;; [[file:jabber.org::#mode-line-mode][jabber-mode-line-mode:1]]
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
;; jabber-mode-line-mode:1 ends here

;; [[file:jabber.org::#watch-alist][jabber-watch-alist:1]]
(defcustom jabber-watch-alist nil
  "Alist of buddies for which an extra notification should be sent
when they come online, with comment strings as values."
  ;; XXX: change symbol to jid-symbol or something, and update
  ;; documentation
  :type '(alist :key-type symbol :value-type string)
  :group 'jabber-watch)
;; jabber-watch-alist:1 ends here

;; [[file:jabber.org::#presence-watch][jabber-presence-watch:1]]
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
;; jabber-presence-watch:1 ends here

;; [[file:jabber.org::#watch-add][jabber-watch-add:1]]
(defun jabber-watch-add (buddy &optional comment)
  (interactive (list (jabber-read-jid-completing "Add buddy to watch list: ")
		     (read-string "Comment: ")))
  (unless (memq 'jabber-presence-watch jabber-presence-hooks)
    (error "The jabber-presence-watch function is not in jabber-presence-hooks"))
  (add-to-list 'jabber-watch-alist (cons
				    (jabber-jid-symbol buddy)
				    (and (not (zerop (length comment)))
					 comment))))
;; jabber-watch-add:1 ends here

;; [[file:jabber.org::#watch-remove][jabber-watch-remove:1]]
(defun jabber-watch-remove (buddy)
  (interactive
   (list (jabber-read-jid-completing "Remove buddy from watch list: "
				     (or (mapcar 'car jabber-watch-alist)
					 (error "Watch list is empty"))
				     t)))
  (setq jabber-watch-alist
        (delq (assq (jabber-jid-symbol buddy) jabber-watch-alist)
	      jabber-watch-alist)))
;; jabber-watch-remove:1 ends here

;; [[file:jabber.org::#activity][jabber-activity:1]]
(defgroup jabber-activity nil
  "Activity tracking options."
  :group 'jabber)
;; jabber-activity:1 ends here

;; [[file:jabber.org::#activity-make-string][jabber-activity-make-string:1]]
(defcustom jabber-activity-make-string 'jabber-activity-make-string-default
  "Function to call to show a string in the modeline.
Function to call, for making the string to put in the mode
line.  The default function returns the nick of the user."
  :set #'(lambda (var val)
	   (custom-set-default var val)
	   (when (and (featurep 'jabber-activity)
		      (fboundp 'jabber-activity-make-name-alist))
	     (jabber-activity-make-name-alist)
	     (jabber-activity-mode-line-update)))
  :type 'function
  :group 'jabber-activity)
;; jabber-activity-make-string:1 ends here

;; [[file:jabber.org::#activity-shorten-minimum][jabber-activity-shorten-minimum:1]]
(defcustom jabber-activity-shorten-minimum 1
  "Length of the strings returned by `jabber-activity-make-strings-shorten'.
All strings returned by `jabber-activity-make-strings-shorten' will be
at least this long, when possible."
  :group 'jabber-activity
  :type 'number)
;; jabber-activity-shorten-minimum:1 ends here

;; [[file:jabber.org::#activity-make-strings][jabber-activity-make-strings:1]]
(defcustom jabber-activity-make-strings 'jabber-activity-make-strings-default
  "Function which should return an alist of JID -> string given a list of JIDs."
  :set #'(lambda (var val)
	   (custom-set-default var val)
	   (when (and (featurep 'jabber-activity)
		      (fboundp 'jabber-activity-make-name-alist))
	     (jabber-activity-make-name-alist)
	     (jabber-activity-mode-line-update)))
  :type '(choice (function-item :tag "Keep strings"
				:value jabber-activity-make-strings-default)
		 (function-item :tag "Shorten strings"
				:value jabber-activity-make-strings-shorten)
		 (function :tag "Other function"))
  :group 'jabber-activity)
;; jabber-activity-make-strings:1 ends here

;; [[file:jabber.org::#activity-count-title][jabber-activity-count-in-title:1]]
(defcustom jabber-activity-count-in-title nil
  "If non-nil, display number of active JIDs in frame title."
  :type 'boolean
  :group 'jabber-activity
  :set #'(lambda (var val)
	   (custom-set-default var val)
	   (when (and (featurep 'jabber-activity)
		      (bound-and-true-p jabber-activity-mode))
	     (jabber-activity-mode -1)
	     (jabber-activity-mode 1))))
;; jabber-activity-count-in-title:1 ends here

;; [[file:jabber.org::#activity-count-title-format][jabber-activity-count-in-title-format:1]]
(defcustom jabber-activity-count-in-title-format
  '(jabber-activity-jids ("[" jabber-activity-count-string "] "))
  "Format string used for displaying activity in frame titles.
Same syntax as `mode-line-format'."
  :type 'sexp
  :group 'jabber-activity
  :set #'(lambda (var val)
	   (if (not (and (featurep 'jabber-activity) (bound-and-true-p jabber-activity-mode)))
	       (custom-set-default var val)
	     (jabber-activity-mode -1)
	     (custom-set-default var val)
	     (jabber-activity-mode 1))))
;; jabber-activity-count-in-title-format:1 ends here

;; [[file:jabber.org::#activity-show-p][jabber-activity-show-p:1]]
(defcustom jabber-activity-show-p 'jabber-activity-show-p-default
  "Function that checks if the given JID should be shown on the mode line.
Predicate function to call to check if the given JID should be
shown in the mode line or not."
  :type 'function
  :group 'jabber-activity)
;; jabber-activity-show-p:1 ends here

;; [[file:jabber.org::#activity-query-unread][jabber-activity-query-unread:1]]
(defcustom jabber-activity-query-unread t
  "Cancel Emacs killing when there are unread messages?
Query the user as to whether killing Emacs should be cancelled when
there are unread messages which otherwise would be lost."
  :type 'boolean
  :group 'jabber-activity)
;; jabber-activity-query-unread:1 ends here

;; [[file:jabber.org::#activity-banned][jabber-activity-banned:1]]
(defcustom jabber-activity-banned nil
  "List of regexps of banned JID."
  :type '(repeat string)
  :group 'jabber-activity)
;; jabber-activity-banned:1 ends here

;; [[file:jabber.org::#activity-face][jabber-activity-face:1]]
(defface jabber-activity-face
  '((t (:foreground "red" :weight bold)))
  "The face for displaying jabber-activity-string in the mode line."
  :group 'jabber-activity)
;; jabber-activity-face:1 ends here

;; [[file:jabber.org::#activity-personal-face][jabber-activity-personal-face:1]]
(defface jabber-activity-personal-face
  '((t (:foreground "blue" :weight bold)))
  "The face for displaying personal jabber-activity-string in the mode line."
  :group 'jabber-activity)
;; jabber-activity-personal-face:1 ends here

;; [[file:jabber.org::#activity-jids][jabber-activity-jids:1]]
(defvar jabber-activity-jids nil
  "A list of JIDs which have caused activity.")
;; jabber-activity-jids:1 ends here

;; [[file:jabber.org::#activity-personal-jids][jabber-activity-personal-jids:1]]
(defvar jabber-activity-personal-jids nil
  "Subset of `jabber-activity-jids' for JIDs with \"personal\" activity.")
;; jabber-activity-personal-jids:1 ends here

;; [[file:jabber.org::#activity-name-alist][jabber-activity-name-alist:1]]
(defvar jabber-activity-name-alist nil
  "Alist of mode line names for bare JIDs.")
;; jabber-activity-name-alist:1 ends here

;; [[file:jabber.org::#activity-mode-string][jabber-activity-mode-string:1]]
(defvar jabber-activity-mode-string ""
  "The mode string for jabber activity.")
;; jabber-activity-mode-string:1 ends here

;; [[file:jabber.org::#activity-count-string][jabber-activity-count-string:1]]
(defvar jabber-activity-count-string "0"
  "Number of active JIDs as a string.")
;; jabber-activity-count-string:1 ends here

;; [[file:jabber.org::#activity-update-hook][jabber-activity-update-hook:1]]
(defvar jabber-activity-update-hook nil
  "Hook called when `jabber-activity-jids' changes.
It is called after `jabber-activity-mode-string' and
`jabber-activity-count-string' are updated.")

;; Protect this variable from being set in Local variables etc.
(put 'jabber-activity-mode-string 'risky-local-variable t)
(put 'jabber-activity-count-string 'risky-local-variable t)
;; jabber-activity-update-hook:1 ends here

;; [[file:jabber.org::#activity-make-string-default][jabber-activity-make-string-default:1]]
(defun jabber-activity-make-string-default (jid)
  "Return the nick of the JID.
If no nick is available, return
the user name part of the JID.  In private MUC conversations,
return the user's nickname."
  (if (jabber-muc-sender-p jid)
      (jabber-jid-resource jid)
    (let ((nick (jabber-jid-displayname jid))
	  (user (jabber-jid-user jid))
	  (username (jabber-jid-username jid)))
      (if (and username (string= nick user))
	  username
	nick))))
;; jabber-activity-make-string-default:1 ends here

;; [[file:jabber.org::#activity-make-strings-default][jabber-activity-make-strings-default:1]]
(defun jabber-activity-make-strings-default (jids)
  "Apply `jabber-activity-make-string' on JIDS."
  (mapcar #'(lambda (jid) (cons jid (funcall jabber-activity-make-string jid)))
	  jids))
;; jabber-activity-make-strings-default:1 ends here

;; [[file:jabber.org::#activity-common-prefix][jabber-activity-common-prefix:1]]
(defun jabber-activity-common-prefix (s1 s2)
  "Return length of common prefix string shared by S1 and S2."
  (let ((len (min (length s1) (length s2))))
    (or (dotimes (i len)
	  (when (not (eq (aref s1 i) (aref s2 i)))
	    (cl-return i)))
	;; Substrings, equal, nil, or empty ("")
	len)))
;; jabber-activity-common-prefix:1 ends here

;; [[file:jabber.org::#activity-make-strings-shorten][jabber-activity-make-strings-shorten:1]]
(defun jabber-activity-make-strings-shorten (jids)
  "Return an alist of (JID . short-names).
Return an alist of JID -> names acquired by running
`jabber-activity-make-string' on JIDS, and then shortening the names
as much as possible such that all strings still are unique and at
least `jabber-activity-shorten-minimum' long."
  (let ((alist
	 (sort (mapcar
		#'(lambda (x) (cons x (funcall jabber-activity-make-string x)))
		jids)
	       #'(lambda (x y) (string-lessp (cdr x) (cdr y))))))
    (cl-loop for ((prev-jid . prev) (cur-jid . cur) (next-jid . next))
	  on (cons nil alist)
	  until (null cur)
	  collect
	  (cons
	   cur-jid
	   (substring
	    cur
	    0 (min (length cur)
		  (max jabber-activity-shorten-minimum
		       (1+ (jabber-activity-common-prefix cur prev))
		       (1+ (jabber-activity-common-prefix cur next)))))))))
;; jabber-activity-make-strings-shorten:1 ends here

;; [[file:jabber.org::#activity-find-buffer-name][jabber-activity-find-buffer-name:1]]
(defun jabber-activity-find-buffer-name (jid)
  "Find the name of the buffer that messages from JID would use."
  (or (and (jabber-jid-resource jid)
	   (get-buffer (jabber-muc-private-get-buffer
			(jabber-jid-user jid)
			(jabber-jid-resource jid))))
      (get-buffer (jabber-chat-get-buffer jid))
      (get-buffer (jabber-muc-get-buffer jid))))
;; jabber-activity-find-buffer-name:1 ends here

;; [[file:jabber.org::#activity-show-p-default][jabber-activity-show-p-default:1]]
(defun jabber-activity-show-p-default (jid)
  "Return t only if there is an invisible buffer for JID.
And, JID is not in `jabber-activity-banned'."
  (let ((buffer (jabber-activity-find-buffer-name jid)))
    (and (buffer-live-p buffer)
	 (not (get-buffer-window buffer 'visible))
         (not (dolist (entry jabber-activity-banned)
                (when (string-match entry jid)
                  (cl-return t)))))))
;; jabber-activity-show-p-default:1 ends here

;; [[file:jabber.org::#activity-make-name-alist][jabber-activity-make-name-alist:1]]
(defun jabber-activity-make-name-alist ()
  "Rebuild `jabber-activity-name-alist' based on currently known JIDs."
  (let ((jids (or (mapcar #'car jabber-activity-name-alist)
		  (mapcar #'symbol-name *jabber-roster*))))
    (setq jabber-activity-name-alist
	  (funcall jabber-activity-make-strings jids))))
;; jabber-activity-make-name-alist:1 ends here

;; [[file:jabber.org::#activity-lookup-name][jabber-activity-lookup-name:1]]
(defun jabber-activity-lookup-name (jid)
  "Lookup name in `jabber-activity-name-alist' and return (jid . string).
Lookup name in `jabber-activity-name-alist', creates an entry
if needed, and returns a (jid . string) pair suitable for the mode line"
  (let ((elm (assoc jid jabber-activity-name-alist)))
    (if elm
	elm
      (progn
	;; Remake alist with the new JID
	(setq jabber-activity-name-alist
	      (funcall jabber-activity-make-strings
		       (cons jid (mapcar #'car jabber-activity-name-alist))))
	(jabber-activity-lookup-name jid)))))
;; jabber-activity-lookup-name:1 ends here

;; [[file:jabber.org::#activity-mode-line-update][jabber-activity-mode-line-update:1]]
(defun jabber-activity-mode-line-update ()
  "Update the string shown in the mode line using `jabber-activity-make-string'.
Update the string shown in the mode line using `jabber-activity-make-string'
on JIDs where `jabber-activity-show-p'.  Optional not-nil GROUP mean that
message come from MUC.
Optional TEXT used with one-to-one or MUC chats and may be used to identify
personal MUC message.
Optional PRESENCE mean personal presence request or alert."
  (setq jabber-activity-mode-string
  	(if jabber-activity-jids
	    (mapconcat
	     (lambda (x)
	       (let ((jump-to-jid (car x)))
		 (jabber-propertize
		  (cdr x)
		  'face (if (member jump-to-jid jabber-activity-personal-jids)
			    'jabber-activity-personal-face
			  'jabber-activity-face)
		  ;; XXX: XEmacs doesn't have make-mode-line-mouse-map.
		  ;; Is there another way to make this work?
		  'local-map (when (fboundp 'make-mode-line-mouse-map)
			       (make-mode-line-mouse-map
				'mouse-1 `(lambda ()
					    (interactive "@")
					    (jabber-activity-switch-to
					     ,(car x)))))
		  'help-echo (concat "Jump to "
				     (jabber-jid-displayname (car x))
				     "'s buffer"))))
	     (mapcar #'jabber-activity-lookup-name
		     jabber-activity-jids)
	     ",")
	  ""))
  (setq jabber-activity-count-string
	(number-to-string (length jabber-activity-jids)))
  (force-mode-line-update 'all)
  (run-hooks 'jabber-activity-update-hook))
;; jabber-activity-mode-line-update:1 ends here

;; [[file:jabber.org::#activity-clean][jabber-activity-clean:1]]
(defun jabber-activity-clean ()
  "Remove JIDs where `jabber-activity-show-p' no longer is true."
  (setq jabber-activity-jids (cl-delete-if-not jabber-activity-show-p
					    jabber-activity-jids))
  (setq jabber-activity-personal-jids
	(cl-delete-if-not jabber-activity-show-p
		       jabber-activity-personal-jids))
  (jabber-activity-mode-line-update))
;; jabber-activity-clean:1 ends here

;; [[file:jabber.org::#activity-add][jabber-activity-add:1]]
(defun jabber-activity-add (from buffer text proposed-alert)
  "Add a JID to mode line when `jabber-activity-show-p'."
  (when (funcall jabber-activity-show-p from)
    (add-to-list 'jabber-activity-jids from)
    (add-to-list 'jabber-activity-personal-jids from)
    (jabber-activity-mode-line-update)))
;; jabber-activity-add:1 ends here

;; [[file:jabber.org::#activity-add-muc][jabber-activity-add-muc:1]]
(defun jabber-activity-add-muc (nick group buffer text proposed-alert)
  "Add a JID to mode line when `jabber-activity-show-p'."
  (when (funcall jabber-activity-show-p group)
    (add-to-list 'jabber-activity-jids group)
    (when (jabber-muc-looks-like-personal-p text group)
      (add-to-list 'jabber-activity-personal-jids group))
    (jabber-activity-mode-line-update)))
;; jabber-activity-add-muc:1 ends here

;; [[file:jabber.org::#activity-presence][jabber-activity-presence:1]]
(defun jabber-activity-presence (who oldstatus newstatus statustext proposed-alert)
  "Add a JID to mode line on subscription requests."
  (when (string= newstatus "subscribe")
    (add-to-list 'jabber-activity-jids (symbol-name who))
    (add-to-list 'jabber-activity-personal-jids (symbol-name who))
    (jabber-activity-mode-line-update)))
;; jabber-activity-presence:1 ends here

;; [[file:jabber.org::#activity-kill-hook][jabber-activity-kill-hook:1]]
(defun jabber-activity-kill-hook ()
  "Query the user if is sure to kill Emacs when there are unread messages.
Query the user as to whether killing Emacs should be cancelled
when there are unread messages which otherwise would be lost, if
`jabber-activity-query-unread' is t"
  (if (and jabber-activity-jids
	   jabber-activity-query-unread)
      (or jabber-silent-mode (yes-or-no-p
       "You have unread Jabber messages, are you sure you want to quit?"))
    t))
;; jabber-activity-kill-hook:1 ends here

;; [[file:jabber.org::#activity-last-buffer][jabber-activity-last-buffer:1]]
(defvar jabber-activity-last-buffer nil
  "Last non-Jabber buffer used.")
;; jabber-activity-last-buffer:1 ends here

;; [[file:jabber.org::#activity-switch-to][jabber-activity-switch-to:1]]
(defun jabber-activity-switch-to (&optional jid-param)
  "If JID-PARAM is provided, switch to that buffer.
If JID-PARAM is nil and
there has been activity in another buffer, switch to that buffer.  If no such
buffer exists, switch back to the last non Jabber chat buffer used."
    (interactive)
    (if (or jid-param jabber-activity-jids)
        (let ((jid (or jid-param (car jabber-activity-jids))))
          (unless (eq major-mode 'jabber-chat-mode)
            (setq jabber-activity-last-buffer (current-buffer)))
          (switch-to-buffer (jabber-activity-find-buffer-name jid))
          (jabber-activity-clean))
      (if (eq major-mode 'jabber-chat-mode)
	  ;; Switch back to the buffer used last
	  (when (buffer-live-p jabber-activity-last-buffer)
	    (switch-to-buffer jabber-activity-last-buffer))
	(message "No new activity"))))
;; jabber-activity-switch-to:1 ends here

;; [[file:jabber.org::#activity-idle-timer][jabber-activity-idle-timer:1]]
(defvar jabber-activity-idle-timer nil "Idle timer used for activity cleaning.")
;; jabber-activity-idle-timer:1 ends here

;; [[file:jabber.org::#activity-mode][jabber-activity-mode:1]]
;;;###autoload
(define-minor-mode jabber-activity-mode
  "Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive."
  :global t
  :group 'jabber-activity
  :init-value t
  (if jabber-activity-mode
      (progn
	;; XEmacs compatibilty hack from erc-track
	(if (featurep 'xemacs)
	    (defadvice switch-to-buffer (after jabber-activity-update (&rest args) activate)
	      (jabber-activity-clean))
	  (add-hook 'window-configuration-change-hook
		    'jabber-activity-clean))
	(add-hook 'jabber-message-hooks
		  'jabber-activity-add)
	(add-hook 'jabber-muc-hooks
		  'jabber-activity-add-muc)
	(add-hook 'jabber-presence-hooks
		  'jabber-activity-presence)
        (setq jabber-activity-idle-timer (run-with-idle-timer 2 t 'jabber-activity-clean))
	;; XXX: reactivate
	;; (add-hook 'jabber-post-connect-hooks
;; 		  'jabber-activity-make-name-alist)
	(add-to-list 'kill-emacs-query-functions
		     'jabber-activity-kill-hook)
	(add-to-list 'global-mode-string
		     '(t jabber-activity-mode-string))
	(when jabber-activity-count-in-title
	  ;; Be careful not to override specific meanings of the
	  ;; existing title format.  In particular, if the car is
	  ;; a symbol, we can't just add our stuff at the beginning.
	  ;; If the car is "", we should be safe.
	  ;;
	  ;; In my experience, sometimes the activity count gets
	  ;; included twice in the title.  I'm not sure exactly why,
	  ;; but it would be nice to replace the code below with
	  ;; something cleaner.
	  (if (equal (car-safe frame-title-format) "")
	      (add-to-list 'frame-title-format
			   jabber-activity-count-in-title-format)
	    (setq frame-title-format (list ""
					   jabber-activity-count-in-title-format
					   frame-title-format)))
	  (if (equal (car-safe icon-title-format) "")
	      (add-to-list 'icon-title-format
			 jabber-activity-count-in-title-format)
	    (setq icon-title-format (list ""
					    jabber-activity-count-in-title-format
					    icon-title-format)))))
    (progn
      (if (featurep 'xemacs)
	  (ad-disable-advice 'switch-to-buffer 'after 'jabber-activity-update)
	(remove-hook 'window-configuration-change-hook
		     'jabber-activity-remove-visible))
      (remove-hook 'jabber-message-hooks
		   'jabber-activity-add)
      (remove-hook 'jabber-muc-hooks
		   'jabber-activity-add-muc)
      (remove-hook 'jabber-presence-hooks
		   'jabber-activity-presence)
      (ignore-errors (cancel-timer jabber-activity-idle-timer))
      ;; XXX: reactivate
;;       (remove-hook 'jabber-post-connect-hooks
;; 		   'jabber-activity-make-name-alist)
      (setq global-mode-string (delete '(t jabber-activity-mode-string)
				       global-mode-string))
      (when (listp frame-title-format)
	(setq frame-title-format
	      (delete jabber-activity-count-in-title-format
		      frame-title-format)))
      (when (listp icon-title-format)
	(setq icon-title-format
	      (delete jabber-activity-count-in-title-format
		      icon-title-format))))))

;; XXX: define-minor-mode should probably do this for us, but it doesn't.
(if jabber-activity-mode (jabber-activity-mode 1))
;; jabber-activity-mode:1 ends here

;; [[file:jabber.org::#events][jabber-events:1]]
(defgroup jabber-events nil
  "Message events and notifications."
  :group 'jabber)
;; jabber-events:1 ends here

;; [[file:jabber.org::#events-request-these][jabber-events-request-these:1]]
(defcustom jabber-events-request-these '(offline
					 delivered
					 displayed
					 composing)
  "Request these kinds of event notifications from others."
  :type '(set (const :tag "Delivered to offline storage" offline)
	      (const :tag "Delivered to user's client" delivered)
	      (const :tag "Displayed to user" displayed)
	      (const :tag "User is typing a reply" composing))
  :group 'jabber-events)
;; jabber-events-request-these:1 ends here

;; [[file:jabber.org::#events-composing-p][jabber-events-composing-p:1]]
(defvar jabber-events-composing-p nil
  "Is the other person composing a message?")
(make-variable-buffer-local 'jabber-events-composing-p)
;; jabber-events-composing-p:1 ends here

;; [[file:jabber.org::#events-arrived][jabber-events-arrived:1]]
(defvar jabber-events-arrived nil
  "In what way has the message reached the recipient?
Possible values are nil (no information available), offline
\(queued for delivery when recipient is online), delivered
\(message has reached the client) and displayed (user is
probably reading the message).")
(make-variable-buffer-local 'jabber-events-arrived)
;; jabber-events-arrived:1 ends here

;; [[file:jabber.org::#events-message][jabber-events-message:1]]
(defvar jabber-events-message ""
  "Human-readable presentation of event information.")
(make-variable-buffer-local 'jabber-events-message)
;; jabber-events-message:1 ends here

;; [[file:jabber.org::#events-update-message][jabber-events-update-message:1]]
(defun jabber-events-update-message ()
  (setq jabber-events-message
	(concat (cdr (assq jabber-events-arrived
			   '((offline . "In offline storage")
			     (delivered . "Delivered")
			     (displayed . "Displayed"))))
		(when jabber-events-composing-p
		  " (typing a message)"))))
;; jabber-events-update-message:1 ends here

;; [[file:jabber.org::#events-when-sending][jabber-events-when-sending:1]]
(add-hook 'jabber-chat-send-hooks 'jabber-events-when-sending)
(defun jabber-events-when-sending (text id)
  (setq jabber-events-arrived nil)
  (jabber-events-update-message)
  `((x ((xmlns . "jabber:x:event"))
       ,@(mapcar #'list jabber-events-request-these))))
;; jabber-events-when-sending:1 ends here

;; [[file:jabber.org::#events-confirm-delivered][jabber-events-confirm-delivered:1]]
(defcustom jabber-events-confirm-delivered t
  "Send delivery confirmation if requested?"
  :group 'jabber-events
  :type 'boolean)
;; jabber-events-confirm-delivered:1 ends here

;; [[file:jabber.org::#events-confirm-displayed][jabber-events-confirm-displayed:1]]
(defcustom jabber-events-confirm-displayed t
  "Send display confirmation if requested?"
  :group 'jabber-events
  :type 'boolean)
;; jabber-events-confirm-displayed:1 ends here

;; [[file:jabber.org::#events-confirm-composing][jabber-events-confirm-composing:1]]
(defcustom jabber-events-confirm-composing t
  "Send notifications about typing a reply?"
  :group 'jabber-events
  :type 'boolean)
;; jabber-events-confirm-composing:1 ends here

;; [[file:jabber.org::#events-requested][jabber-events-requested:1]]
(defvar jabber-events-requested ()
  "List of events requested.")
(make-variable-buffer-local 'jabber-events-requested)
;; jabber-events-requested:1 ends here

;; [[file:jabber.org::#events-last-id][jabber-events-last-id:1]]
(defvar jabber-events-last-id nil
  "Id of last message received, or nil if none.")
(make-variable-buffer-local 'jabber-events-last-id)
;; jabber-events-last-id:1 ends here

;; [[file:jabber.org::#events-delivery-confirmed][jabber-events-delivery-confirmed:1]]
(defvar jabber-events-delivery-confirmed nil
  "Has delivery confirmation been sent?")
(make-variable-buffer-local 'jabber-events-delivery-confirmed)
;; jabber-events-delivery-confirmed:1 ends here

;; [[file:jabber.org::#events-display-confirmed][jabber-events-display-confirmed:1]]
(defvar jabber-events-display-confirmed nil
  "Has display confirmation been sent?")
(make-variable-buffer-local 'jabber-events-display-confirmed)
;; jabber-events-display-confirmed:1 ends here

;; [[file:jabber.org::#events-composing-sent][jabber-events-composing-sent:1]]
(defvar jabber-events-composing-sent nil
  "Has composing notification been sent?
It can be sent and cancelled several times.")
;; jabber-events-composing-sent:1 ends here

;; [[file:jabber.org::#events-confirm-display][jabber-events-confirm-display:1]]
(add-hook 'window-configuration-change-hook
	  'jabber-events-confirm-display)
(defun jabber-events-confirm-display ()
  "Send display confirmation if appropriate.
That is, if user allows it, if the other user requested it,
and it hasn't been sent before."
  (walk-windows #'jabber-events-confirm-display-in-window))
;; jabber-events-confirm-display:1 ends here

;; [[file:jabber.org::#events-confirm-display-window][jabber-events-confirm-display-in-window:1]]
(defun jabber-events-confirm-display-in-window (window)
  (with-current-buffer (window-buffer window)
    (when (and jabber-events-confirm-displayed
	       (not jabber-events-display-confirmed)
	       (memq 'displayed jabber-events-requested)
	       ;; XXX: if jabber-events-requested is non-nil, how can
	       ;; jabber-chatting-with be nil?  See
	       ;; http://sourceforge.net/tracker/index.php?func=detail&aid=1872560&group_id=88346&atid=586350
	       jabber-chatting-with
	       ;; don't send to bare jids
	       (jabber-jid-resource jabber-chatting-with))
      (jabber-send-sexp
       jabber-buffer-connection
       `(message
	 ((to . ,jabber-chatting-with))
	 (x ((xmlns . "jabber:x:event"))
	    (displayed)
	    (id () ,jabber-events-last-id))))
      (setq jabber-events-display-confirmed t))))
;; jabber-events-confirm-display-in-window:1 ends here

;; [[file:jabber.org::#events-after-change][jabber-events-after-change:1]]
(defun jabber-events-after-change ()
  (let ((composing-now (not (= (point-max) jabber-point-insert))))
    (when (and jabber-events-confirm-composing
	       jabber-chatting-with
	       (not (eq composing-now jabber-events-composing-sent)))
      (jabber-send-sexp
       jabber-buffer-connection
       `(message
	 ((to . ,jabber-chatting-with))
	 (x ((xmlns . "jabber:x:event"))
	    ,@(if composing-now '((composing)) nil)
	    (id () ,jabber-events-last-id))))
      (setq jabber-events-composing-sent composing-now))))
;; jabber-events-after-change:1 ends here

;; [[file:jabber.org::#common][common:1]]
(add-to-list 'jabber-message-chain 'jabber-handle-incoming-message-events t)
;; common:1 ends here

;; [[file:jabber.org::#handle-incoming-message-events][jabber-handle-incoming-message-events:1]]
(defun jabber-handle-incoming-message-events (jc xml-data)
  (when (and (not (jabber-muc-message-p xml-data))
	     (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))))
    (with-current-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))
      (let ((x (cl-find "jabber:x:event"
		     (jabber-xml-get-children xml-data 'x)
		     :key #'(lambda (x) (jabber-xml-get-attribute x 'xmlns))
		     :test #'string=)))
	(cond
	 ;; If we get an error message, we shouldn't report any
	 ;; events, as the requests are mirrored from us.
	 ((string= (jabber-xml-get-attribute xml-data 'type) "error")
	  (remove-hook 'post-command-hook 'jabber-events-after-change t)
	  (setq jabber-events-requested nil))

	 ;; If there's a body, it's not an incoming message event.
	 ((jabber-xml-get-children xml-data 'body)
	  ;; User is done composing, obviously.
	  (setq jabber-events-composing-p nil)
	  (jabber-events-update-message)

	  ;; Reset variables
	  (setq jabber-events-display-confirmed nil)
	  (setq jabber-events-delivery-confirmed nil)

	  ;; User requests message events
	  (setq jabber-events-requested
		;; There might be empty strings in the XML data,
		;; which car chokes on.  Having nil values in
		;; the list won't hurt, therefore car-safe.
		(mapcar #'car-safe
			(jabber-xml-node-children x)))
	  (setq jabber-events-last-id (jabber-xml-get-attribute
				       xml-data 'id))

	  ;; Send notifications we already know about
	  (cl-flet ((send-notification
		  (type)
		  (jabber-send-sexp
		   jc
		   `(message
		     ((to . ,(jabber-xml-get-attribute xml-data 'from)))
		     (x ((xmlns . "jabber:x:event"))
			(,type)
			(id () ,jabber-events-last-id))))))
	    ;; Send delivery confirmation if appropriate
	    (when (and jabber-events-confirm-delivered
		       (memq 'delivered jabber-events-requested))
	      (send-notification 'delivered)
	      (setq jabber-events-delivery-confirmed t))

	    ;; Send display confirmation if appropriate
	    (when (and jabber-events-confirm-displayed
		       (get-buffer-window (current-buffer) 'visible)
		       (memq 'displayed jabber-events-requested))
	      (send-notification 'displayed)
	      (setq jabber-events-display-confirmed t))

	    ;; Set up hooks for composition notification
	    (when (and jabber-events-confirm-composing
		       (memq 'composing jabber-events-requested))
	      (add-hook 'post-command-hook 'jabber-events-after-change
			nil t))))
	 (t
	  ;; So it has no body.  If it's a message event,
	  ;; the <x/> node should be the only child of the
	  ;; message, and it should contain an <id/> node.
	  ;; We check the latter.
	  (when (and x (jabber-xml-get-children x 'id))
	    ;; Currently we don't care about the <id/> node.

	    ;; There's only one node except for the id.
	    (unless
		(dolist (possible-node '(offline delivered displayed))
		  (when (jabber-xml-get-children x possible-node)
		    (setq jabber-events-arrived possible-node)
		    (jabber-events-update-message)
		    (cl-return t)))
	      ;; Or maybe even zero, which is a negative composing node.
	      (setq jabber-events-composing-p
		    (not (null (jabber-xml-get-children x 'composing))))
	      (jabber-events-update-message)))))))))
;; jabber-handle-incoming-message-events:1 ends here

;; [[file:jabber.org::#chatstates][jabber-chatstates:1]]
(defgroup jabber-chatstates nil
  "Chat state notifications."
  :group 'jabber)
;; jabber-chatstates:1 ends here

;; [[file:jabber.org::#chatstates-xmlns][jabber-chatstates-xmlns:1]]
(defconst jabber-chatstates-xmlns "http://jabber.org/protocol/chatstates"
  "XML namespace for the chatstates feature.")
;; jabber-chatstates-xmlns:1 ends here

;; [[file:jabber.org::#chatstates-confirm][jabber-chatstates-confirm:1]]
(defcustom jabber-chatstates-confirm t
  "Send notifications about chat states?"
  :group 'jabber-chatstates
  :type 'boolean)
;; jabber-chatstates-confirm:1 ends here

;; [[file:jabber.org::#chatstates-requested][jabber-chatstates-requested:1]]
(defvar jabber-chatstates-requested 'first-time
  "Whether or not chat states notification was requested.
This is one of the following:
first-time - send state in first stanza, then switch to nil
t - send states
nil - don't send states")
(make-variable-buffer-local 'jabber-chatstates-requested)
;; jabber-chatstates-requested:1 ends here

;; [[file:jabber.org::#chatstates-last-state][jabber-chatstates-last-state:1]]
(defvar jabber-chatstates-last-state nil
  "The last seen chat state.")
(make-variable-buffer-local 'jabber-chatstates-last-state)
;; jabber-chatstates-last-state:1 ends here

;; [[file:jabber.org::#chatstates-message][jabber-chatstates-message:1]]
(defvar jabber-chatstates-message ""
  "Human-readable presentation of chat state information.")
(make-variable-buffer-local 'jabber-chatstates-message)
;; jabber-chatstates-message:1 ends here

;; [[file:jabber.org::#chatstates-update-message][jabber-chatstates-update-message:1]]
(defun jabber-chatstates-update-message ()
  (setq jabber-chatstates-message
        (if (and jabber-chatstates-last-state
                 (not (eq 'active jabber-chatstates-last-state)))
            (format " (%s)" (symbol-name jabber-chatstates-last-state))
          "")))
;; jabber-chatstates-update-message:1 ends here

;; [[file:jabber.org::#chatstates-when-sending][jabber-chatstates-when-sending:1]]
(add-hook 'jabber-chat-send-hooks 'jabber-chatstates-when-sending)
(defun jabber-chatstates-when-sending (text id)
  (jabber-chatstates-update-message)
  (jabber-chatstates-stop-timer)
  (when (and jabber-chatstates-confirm jabber-chatstates-requested)
    (when (eq jabber-chatstates-requested 'first-time)
      ;; don't send more notifications until we know that the other
      ;; side wants them.
      (setq jabber-chatstates-requested nil))
    (setq jabber-chatstates-composing-sent nil)
    `((active ((xmlns . ,jabber-chatstates-xmlns))))))
;; jabber-chatstates-when-sending:1 ends here

;; [[file:jabber.org::#chatstates-composing-sent][jabber-chatstates-composing-sent:1]]
(defvar jabber-chatstates-composing-sent nil
  "Has composing notification been sent?
It can be sent and cancelled several times.")
(make-variable-buffer-local 'jabber-chatstates-composing-sent)
;; jabber-chatstates-composing-sent:1 ends here

;; [[file:jabber.org::#chatstates-paused-timer][jabber-chatstates-paused-timer:1]]
(defvar jabber-chatstates-paused-timer nil
  "Timer that counts down from 'composing state to 'paused.")
(make-variable-buffer-local 'jabber-chatstates-paused-timer)
;; jabber-chatstates-paused-timer:1 ends here

;; [[file:jabber.org::#chatstates-stop-timer][jabber-chatstates-stop-timer:1]]
(defun jabber-chatstates-stop-timer ()
  "Stop the 'paused timer."
  (when jabber-chatstates-paused-timer
    (cancel-timer jabber-chatstates-paused-timer)))
;; jabber-chatstates-stop-timer:1 ends here

;; [[file:jabber.org::#chatstates-kick-timer][jabber-chatstates-kick-timer:1]]
(defun jabber-chatstates-kick-timer ()
  "Start (or restart) the 'paused timer as approriate."
  (jabber-chatstates-stop-timer)
  (setq jabber-chatstates-paused-timer
        (run-with-timer 5 nil 'jabber-chatstates-send-paused)))
;; jabber-chatstates-kick-timer:1 ends here

;; [[file:jabber.org::#chatstates-send-paused][jabber-chatstates-send-paused:1]]
(defun jabber-chatstates-send-paused ()
  "Send an 'paused state notification."
  (when (and jabber-chatstates-requested jabber-chatting-with)
    (setq jabber-chatstates-composing-sent nil)
    (jabber-send-sexp-if-connected
     jabber-buffer-connection
     `(message
       ((to . ,jabber-chatting-with)
        (type . "chat"))
       (paused ((xmlns . ,jabber-chatstates-xmlns)))))))
;; jabber-chatstates-send-paused:1 ends here

;; [[file:jabber.org::#chatstates-after-change][jabber-chatstates-after-change:1]]
(defun jabber-chatstates-after-change ()
  (let* ((composing-now (not (= (point-max) jabber-point-insert)))
         (state (if composing-now 'composing 'active)))
    (when (and jabber-chatstates-confirm
               jabber-chatting-with
	       jabber-chatstates-requested
               (not (eq composing-now jabber-chatstates-composing-sent)))
      (jabber-send-sexp-if-connected
       jabber-buffer-connection
       `(message
         ((to . ,jabber-chatting-with)
          (type . "chat"))
         (,state ((xmlns . ,jabber-chatstates-xmlns)))))
      (when (setq jabber-chatstates-composing-sent composing-now)
        (jabber-chatstates-kick-timer)))))
;; jabber-chatstates-after-change:1 ends here

;; [[file:jabber.org::#handle-incoming-message-chatstates][jabber-handle-incoming-message-chatstates:1]]
(defun jabber-handle-incoming-message-chatstates (jc xml-data)
  (when (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from)))
    (with-current-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))
      (cond
       ;; If we get an error message, we shouldn't report any
       ;; events, as the requests are mirrored from us.
       ((string= (jabber-xml-get-attribute xml-data 'type) "error")
        (remove-hook 'post-command-hook 'jabber-chatstates-after-change t)
        (setq jabber-chatstates-requested nil))

       (t
	(let ((state
	       (or
		(let ((node
		       (cl-find jabber-chatstates-xmlns
			     (jabber-xml-node-children xml-data)
			     :key #'(lambda (x) (jabber-xml-get-attribute x 'xmlns))
			     :test #'string=)))
		  (jabber-xml-node-name node))
		(let ((node
		       ;; XXX: this is how we interoperate with
		       ;; Google Talk.  We should really use a
		       ;; namespace-aware XML parser.
		       (cl-find jabber-chatstates-xmlns
			     (jabber-xml-node-children xml-data)
			     :key #'(lambda (x) (jabber-xml-get-attribute x 'xmlns:cha))
			     :test #'string=)))
		  (when node
		    ;; Strip the "cha:" prefix
		    (let ((name (symbol-name (jabber-xml-node-name node))))
		      (when (> (length name) 4)
			(intern (substring name 4)))))))))
	  ;; Set up hooks for composition notification
	  (when (and jabber-chatstates-confirm state)
	    (setq jabber-chatstates-requested t)
	    (add-hook 'post-command-hook 'jabber-chatstates-after-change nil t))

	  (setq jabber-chatstates-last-state state)
	  (jabber-chatstates-update-message)))))))

;; Add function last in chain, so a chat buffer is already created.
(add-to-list 'jabber-message-chain 'jabber-handle-incoming-message-chatstates t)
;; jabber-handle-incoming-message-chatstates:1 ends here

;; [[file:jabber.org::#handle-incoming-message-chatstates][jabber-handle-incoming-message-chatstates:2]]
(jabber-disco-advertise-feature "http://jabber.org/protocol/chatstates")
;; jabber-handle-incoming-message-chatstates:2 ends here

;; [[file:jabber.org::#generic-functions-avatars][Generic functions for avatars:1]]
(require 'mailcap)
;; Generic functions for avatars:1 ends here

;; [[file:jabber.org::#avatar][jabber-avatar:1]]
(defgroup jabber-avatar nil
  "Avatar related settings"
  :group 'jabber)
;; jabber-avatar:1 ends here

;; [[file:jabber.org::#avatar-cache-directory][jabber-avatar-cache-directory:1]]
(defcustom jabber-avatar-cache-directory
  (locate-user-emacs-file "jabber-avatar-cache" ".jabber-avatars")
  "Directory to use for cached avatars."
  :group 'jabber-avatar
  :type 'directory)
;; jabber-avatar-cache-directory:1 ends here

;; [[file:jabber.org::#avatar-verbose][jabber-avatar-verbose:1]]
(defcustom jabber-avatar-verbose nil
  "Display messages about irregularities with other people's avatars."
  :group 'jabber-avatar
  :type 'boolean)
;; jabber-avatar-verbose:1 ends here

;; [[file:jabber.org::#avatar-max-width][jabber-avatar-max-width:1]]
(defcustom jabber-avatar-max-width 96
  "Maximum width of avatars."
  :group 'jabber-avatar
  :type 'integer)
;; jabber-avatar-max-width:1 ends here

;; [[file:jabber.org::#avatar-max-height][jabber-avatar-max-height:1]]
(defcustom jabber-avatar-max-height 96
  "Maximum height of avatars."
  :group 'jabber-avatar
  :type 'integer)
;; jabber-avatar-max-height:1 ends here

;; [[file:jabber.org::#avatar][avatar:1]]
(cl-defstruct
    avatar sha1-sum mime-type url base64-data height width bytes)
;; avatar:1 ends here

;; [[file:jabber.org::#avatar-from-url][jabber-avatar-from-url:1]]
(defun jabber-avatar-from-url (url)
  "Construct an avatar structure from the given URL.
Retrieves the image to find info about it."
  (with-current-buffer (let ((coding-system-for-read 'binary))
			 (url-retrieve-synchronously url))
    (let* ((case-fold-search t)
	   (mime-type (ignore-errors
			(search-forward-regexp "^content-type:[ \t]*\\(.*\\)$")
			(match-string 1)))
	   (data (progn
		   (search-forward "\n\n")
		   (buffer-substring (point) (point-max)))))
      (prog1
	  (jabber-avatar-from-data data nil mime-type)
	(kill-buffer nil)))))
;; jabber-avatar-from-url:1 ends here

;; [[file:jabber.org::#avatar-from-file][jabber-avatar-from-file:1]]
(defun jabber-avatar-from-file (filename)
  "Construct an avatar structure from FILENAME."
  (require 'mailcap)
  (let ((data (with-temp-buffer
		(insert-file-contents-literally filename)
		(buffer-string)))
	(mime-type (when (string-match "\\.[^.]+$" filename)
		     (mailcap-extension-to-mime (match-string 0 filename)))))
    (jabber-avatar-from-data data nil mime-type)))
;; jabber-avatar-from-file:1 ends here

;; [[file:jabber.org::#avatar-from-base64-string][jabber-avatar-from-base64-string:1]]
(defun jabber-avatar-from-base64-string (base64-string &optional mime-type)
  "Construct an avatar stucture from BASE64-STRING.
If MIME-TYPE is not specified, try to find it from the image data."
  (jabber-avatar-from-data nil base64-string mime-type))
;; jabber-avatar-from-base64-string:1 ends here

;; [[file:jabber.org::#avatar-from-data][jabber-avatar-from-data:1]]
(defun jabber-avatar-from-data (raw-data base64-string &optional mime-type)
  "Construct an avatar structure from RAW-DATA and/or BASE64-STRING.
If either is not provided, it is computed.
If MIME-TYPE is not specified, try to find it from the image data."
  (let* ((data (or raw-data (base64-decode-string base64-string)))
	 (bytes (length data))
	 (sha1-sum (sha1 data))
	 (base64-data (or base64-string (base64-encode-string raw-data)))
	 (type (or mime-type
		   (cdr (assq (get :type (cdr (condition-case nil
						  (jabber-create-image data nil t)
						(error nil))))
			      '((png "image/png")
				(jpeg "image/jpeg")
				(gif "image/gif")))))))
    (jabber-avatar-compute-size
     (make-avatar :mime-type mime-type :sha1-sum sha1-sum :base64-data base64-data :bytes bytes))))

;; XXX: This function is based on an outdated version of XEP-0084.
;; (defun jabber-avatar-from-data-node (data-node)
;;   "Construct an avatar structure from the given <data/> node."
;;   (jabber-xml-let-attributes
;;    (content-type id bytes height width) data-node
;;    (let ((base64-data (car (jabber-xml-node-children data-node))))
;;      (make-avatar :mime-type content-type :sha1-sum id :bytes bytes
;; 		  :height height :width width :base64-data base64-data))))
;; jabber-avatar-from-data:1 ends here

;; [[file:jabber.org::#avatar-image][jabber-avatar-image:1]]
(defun jabber-avatar-image (avatar)
  "Create an image from AVATAR.
Return nil if images of this type are not supported."
  (condition-case nil
      (jabber-create-image (with-temp-buffer
		      (set-buffer-multibyte nil)
		      (insert (avatar-base64-data avatar))
		      (base64-decode-region (point-min) (point-max))
		      (buffer-string))
		    nil
		    t)
      (error nil)))
;; jabber-avatar-image:1 ends here

;; [[file:jabber.org::#avatar-compute-size][jabber-avatar-compute-size:1]]
(defun jabber-avatar-compute-size (avatar)
  "Compute and set the width and height fields of AVATAR.
Return AVATAR."
  ;; image-size only works when there is a window system.
  ;; But display-graphic-p doesn't exist on XEmacs...
  (let ((size (and (fboundp 'display-graphic-p)
		   (display-graphic-p)
		   (let ((image (jabber-avatar-image avatar)))
		     (and image
			  (image-size image t))))))
    (when size
      (setf (avatar-width avatar) (car size))
      (setf (avatar-height avatar) (cdr size)))
    avatar))
;; jabber-avatar-compute-size:1 ends here

;; [[file:jabber.org::#avatar-find-cached][jabber-avatar-find-cached:1]]
(defun jabber-avatar-find-cached (sha1-sum)
  "Return file name of cached image for avatar identified by SHA1-SUM.
If there is no cached image, return nil."
  (let ((filename (expand-file-name sha1-sum jabber-avatar-cache-directory)))
    (if (file-exists-p filename)
        filename
      nil)))
;; jabber-avatar-find-cached:1 ends here

;; [[file:jabber.org::#avatar-cache][jabber-avatar-cache:1]]
(defun jabber-avatar-cache (avatar)
  "Cache the AVATAR."
  (let* ((id (avatar-sha1-sum avatar))
	 (base64-data (avatar-base64-data avatar))
	 (mime-type (avatar-mime-type avatar))
	 (filename (expand-file-name id jabber-avatar-cache-directory)))
    (unless (file-directory-p jabber-avatar-cache-directory)
      (make-directory jabber-avatar-cache-directory t))

    (if (file-exists-p filename)
	(when jabber-avatar-verbose
	  (message "Caching avatar, but %s already exists" filename))
      (with-temp-buffer
	(let ((require-final-newline nil)
	      (coding-system-for-write 'binary))
	  (if (fboundp 'set-buffer-multibyte)
	      (set-buffer-multibyte nil))
	  (insert base64-data)
	  (base64-decode-region (point-min) (point-max))
	  (write-region (point-min) (point-max) filename nil 'silent))))))
;; jabber-avatar-cache:1 ends here

;; [[file:jabber.org::#avatar-set][jabber-avatar-set:1]]
;;;; Set avatar for contact
(defun jabber-avatar-set (jid avatar)
  "Set the avatar of JID to be AVATAR.
JID is a string containing a bare JID.
AVATAR may be one of:
 * An avatar structure.
 * The SHA1 sum of a cached avatar.
 * nil, meaning no avatar."
  ;; We want to optimize for the case of same avatar.
  ;; Loading an image is expensive, so do it lazily.
  (let ((jid-symbol (jabber-jid-symbol jid))
	image hash)
    (cond
     ((avatar-p avatar)
      (setq hash (avatar-sha1-sum avatar))
      (setq image (lambda () (jabber-avatar-image avatar))))
     ((stringp avatar)
      (setq hash avatar)
      (setq image (lambda ()
		    (condition-case nil
			(jabber-create-image (jabber-avatar-find-cached avatar))
		      (error nil)))))
     (t
      (setq hash nil)
      (setq image #'ignore)))

    (unless (string= hash (get jid-symbol 'avatar-hash))
      (put jid-symbol 'avatar (funcall image))
      (put jid-symbol 'avatar-hash hash)
      (jabber-presence-update-roster jid-symbol))))
;; jabber-avatar-set:1 ends here

;; [[file:jabber.org::#create-image][jabber-create-image:1]]
(defun jabber-create-image (file-or-data &optional type data-p)
  "Create image, scaled down to jabber-avatar-max-width/height.
If width/height exceeds either of those, and ImageMagick is
available."
  (let* ((image (create-image file-or-data type data-p))
         (size (image-size image t))
         (spec (cdr image)))
    (when (and (functionp 'imagemagick-types)
               (or (> (car size) jabber-avatar-max-width)
                   (> (cdr size) jabber-avatar-max-height)))
      (plist-put spec :type 'imagemagick)
      (plist-put spec :width jabber-avatar-max-width)
      (plist-put spec :height jabber-avatar-max-height))
    image))
;; jabber-create-image:1 ends here

;; [[file:jabber.org::#vcard-photo][jabber-vcard-photo:1]]
(defvar jabber-vcard-photo nil
  "The avatar structure for the photo in the vCard edit buffer.")
(make-variable-buffer-local 'jabber-vcard-photo)
;; jabber-vcard-photo:1 ends here

;; [[file:jabber.org::#vcard-parse][jabber-vcard-parse:1]]
(defun jabber-vcard-parse (vcard)
  "Parse the vCard XML structure given in VCARD.
The top node should be the `vCard' node."
  ;; Hm... stpeter has a <query/> as top node...
  ;;(unless (eq (jabber-xml-node-name vcard) 'vCard)
  ;;  (error "Invalid vCard"))
  (let (result)
    (dolist (verbatim-node '(FN NICKNAME BDAY JABBERID MAILER TZ
				 TITLE ROLE NOTE PRODID REV SORT-STRING
				 UID URL DESC))
      ;; There should only be one of each of these.  They are
      ;; used verbatim.
      (let ((node (car (jabber-xml-get-children vcard
						verbatim-node))))
	;; Some clients include the node, but without data
	(when (car (jabber-xml-node-children node))
	  (push (cons (jabber-xml-node-name node)
		      (car (jabber-xml-node-children node)))
		result))))

    ;; Name components
    (let ((node (car (jabber-xml-get-children vcard 'N))))
      ;; Subnodes are FAMILY, GIVEN, MIDDLE, PREFIX, SUFFIX
      (push (cons 'N
		  (let (name)
		    (dolist (subnode (jabber-xml-node-children node))
		      (when (and (memq (jabber-xml-node-name subnode)
				       '(FAMILY GIVEN MIDDLE PREFIX SUFFIX))
				 (not (zerop (length
					      (car (jabber-xml-node-children
						    subnode))))))
			(push (cons (jabber-xml-node-name subnode)
				    (car (jabber-xml-node-children
					  subnode)))
			      name)))
		    name))
	    result))

    ;; There can be several addresses
    (let (addresses)
      (dolist (adr (jabber-xml-get-children vcard 'ADR))
	;; Find address type(s)
	(let (types)
	  (dolist (possible-type '(HOME WORK POSTAL PARCEL DOM INTL PREF))
	    (when (jabber-xml-get-children adr possible-type)
	      (push possible-type types)))

	  (let (components)
	    (dolist (component (jabber-xml-node-children adr))
	      (when (and (memq (jabber-xml-node-name component)
			       '(POBOX EXTADD STREET LOCALITY REGION
				       PCODE CTRY))
			 (not (zerop (length
				      (car (jabber-xml-node-children
					    component))))))
		(push (cons (jabber-xml-node-name component)
			    (car (jabber-xml-node-children component)))
		      components)))

	    (push (cons types components) addresses))))

      (when addresses
	(push (cons 'ADR addresses) result)))

    ;; Likewise for phone numbers
    (let (phone-numbers)
      (dolist (tel (jabber-xml-get-children vcard 'TEL))
	;; Find phone type(s)
	(let ((number (car (jabber-xml-node-children
			    (car (jabber-xml-get-children tel 'NUMBER)))))
	      types)
	  ;; Some clients put no NUMBER node.  Avoid that.
	  (when number
	    (dolist (possible-type '(HOME WORK VOICE FAX PAGER MSG CELL
					  VIDEO BBS MODEM ISDN PCS PREF))
	      (when (jabber-xml-get-children tel possible-type)
		(push possible-type types)))

	    (push (cons types number) phone-numbers))))

      (when phone-numbers
	(push (cons 'TEL phone-numbers) result)))

    ;; And for e-mail addresses
    (let (e-mails)
      (dolist (email (jabber-xml-get-children vcard 'EMAIL))
	(let ((userid (car (jabber-xml-node-children
			    (car (jabber-xml-get-children email 'USERID)))))
	      types)
	  ;; Some clients put no USERID node.  Avoid that.
	  (when userid
	    (dolist (possible-type '(HOME WORK INTERNET PREF X400))
	      (when (jabber-xml-get-children email possible-type)
		(push possible-type types)))
	    (unless (or (memq 'INTERNET types)
			(memq 'X400 types))
	      (push 'INTERNET types))

    (push (cons types userid) e-mails))))

      (when e-mails
	(push (cons 'EMAIL e-mails) result)))

    ;; XEP-0153: vCard-based avatars
    (let ((photo-tag (car (jabber-xml-get-children vcard 'PHOTO))))
      (when photo-tag
	(let ((type (jabber-xml-path photo-tag '(TYPE "")))
	      (binval (jabber-xml-path photo-tag '(BINVAL ""))))
	  (when (and type binval)
	    (push (list 'PHOTO type binval) result)))))

    result))
;; jabber-vcard-parse:1 ends here

;; [[file:jabber.org::#vcard-reassemble][jabber-vcard-reassemble:1]]
(defun jabber-vcard-reassemble (parsed)
  "Create a vCard XML structure from PARSED."
  ;; Save photo in jabber-vcard-photo, to avoid excessive processing.
  (let ((photo (cdr (assq 'PHOTO parsed))))
    (cond
     ;; No photo
     ((null photo)
      (setq jabber-vcard-photo nil))
     ;; Existing photo
     ((listp photo)
      (setq jabber-vcard-photo
	    (jabber-avatar-from-base64-string
	     (nth 1 photo) (nth 0 photo))))
     ;; New photo from file
     (t
      (access-file photo "Avatar file not found")
      ;; Maximum allowed size is 8 kilobytes
      (when (> (nth 7 (file-attributes photo)) 8192)
	(error "Avatar bigger than 8 kilobytes"))
      (setq jabber-vcard-photo (jabber-avatar-from-file photo)))))

  `(vCard ((xmlns . "vcard-temp"))
	  ;; Put in simple fields
	  ,@(mapcar
	     (lambda (field)
	       (when (and (assq (car field) jabber-vcard-fields)
			  (not (zerop (length (cdr field)))))
		 (list (car field) nil (cdr field))))
	     parsed)
	  ;; Put in decomposited name
	  (N nil
	     ,@(mapcar
		(lambda (name-part)
		  (when (not (zerop (length (cdr name-part))))
		    (list (car name-part) nil (cdr name-part))))
		(cdr (assq 'N parsed))))
	  ;; Put in addresses
	  ,@(mapcar
	     (lambda (address)
	       (append '(ADR) '(())
		       (mapcar 'list (nth 0 address))
		       (mapcar (lambda (field)
				 (list (car field) nil (cdr field)))
			       (cdr address))))
	     (cdr (assq 'ADR parsed)))
	  ;; Put in phone numbers
	  ,@(mapcar
	     (lambda (phone)
	       (append '(TEL) '(())
		       (mapcar 'list (car phone))
		       (list (list 'NUMBER nil (cdr phone)))))
	     (cdr (assq 'TEL parsed)))
	  ;; Put in e-mail addresses
	  ,@(mapcar
	     (lambda (email)
	       (append '(EMAIL) '(())
		       (mapcar 'list (car email))
		       (list (list 'USERID nil (cdr email)))))
	     (cdr (assq 'EMAIL parsed)))
	  ;; Put in photo
	  ,@(when jabber-vcard-photo
	      `((PHOTO ()
		       (TYPE () ,(avatar-mime-type jabber-vcard-photo))
		       (BINVAL () ,(avatar-base64-data jabber-vcard-photo)))))))
;; jabber-vcard-reassemble:1 ends here

;; [[file:jabber.org::#vcard-reassemble][jabber-vcard-reassemble:2]]
(add-to-list 'jabber-jid-info-menu
	     (cons "Request vcard" 'jabber-vcard-get))
;; jabber-vcard-reassemble:2 ends here

;; [[file:jabber.org::#vcard-get][jabber-vcard-get:1]]
(defun jabber-vcard-get (jc jid)
  "Request vcard from JID.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Request vcard from: " nil nil nil 'bare-or-muc)))
  (jabber-send-iq jc jid
		  "get"
		  '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-process-data #'jabber-vcard-display
		  #'jabber-process-data "Vcard request failed"))
;; jabber-vcard-get:1 ends here

;; [[file:jabber.org::#vcard-edit][jabber-vcard-edit:1]]
(defun jabber-vcard-edit (jc)
  "Edit your own vcard.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc nil
		  "get"
		  '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-vcard-do-edit nil
		  #'jabber-report-success "Vcard request failed"))
;; jabber-vcard-edit:1 ends here

;; [[file:jabber.org::#vcard-fields][jabber-vcard-fields:1]]
(defconst jabber-vcard-fields '((FN . "Full name")
				(NICKNAME . "Nickname")
				(BDAY . "Birthday")
				(URL . "URL")
				(JABBERID . "JID")
				(MAILER . "User agent")
				(TZ . "Time zone")
				(TITLE . "Title")
				(ROLE . "Role")
				(REV . "Last changed")
				(DESC . "Description")
				(NOTE . "Note")))
;; jabber-vcard-fields:1 ends here

;; [[file:jabber.org::#vcard-name-fields][jabber-vcard-name-fields:1]]
(defconst jabber-vcard-name-fields '((PREFIX . "Prefix")
				     (GIVEN . "Given name")
				     (MIDDLE . "Middle name")
				     (FAMILY . "Family name")
				     (SUFFIX . "Suffix")))
;; jabber-vcard-name-fields:1 ends here

;; [[file:jabber.org::#vcard-phone-types][jabber-vcard-phone-types:1]]
(defconst jabber-vcard-phone-types '((HOME . "Home")
				     (WORK . "Work")
				     (VOICE . "Voice")
				     (FAX . "Fax")
				     (PAGER . "Pager")
				     (MSG . "Message")
				     (CELL . "Cell phone")
				     (VIDEO . "Video")
				     (BBS . "BBS")
				     (MODEM . "Modem")
				     (ISDN . "ISDN")
				     (PCS . "PCS")))
;; jabber-vcard-phone-types:1 ends here

;; [[file:jabber.org::#vcard-email-types][jabber-vcard-email-types:1]]
(defconst jabber-vcard-email-types '((HOME . "Home")
				     (WORK . "Work")
				     (INTERNET . "Internet")
				     (X400 . "X400")
				     (PREF . "Preferred")))
;; jabber-vcard-email-types:1 ends here

;; [[file:jabber.org::#vcard-address-types][jabber-vcard-address-types:1]]
(defconst jabber-vcard-address-types '((HOME . "Home")
				       (WORK . "Work")
				       (POSTAL . "Postal")
				       (PARCEL . "Parcel")
				       (DOM . "Domestic")
				       (INTL . "International")
				       (PREF . "Preferred")))
;; jabber-vcard-address-types:1 ends here

;; [[file:jabber.org::#vcard-address-fields][jabber-vcard-address-fields:1]]
(defconst jabber-vcard-address-fields '((POBOX . "Post box")
					(EXTADD . "Ext. address")
					(STREET . "Street")
					(LOCALITY . "Locality")
					(REGION . "Region")
					(PCODE . "Post code")
					(CTRY . "Country")))
;; jabber-vcard-address-fields:1 ends here

;; [[file:jabber.org::#vcard-display][jabber-vcard-display:1]]
(defun jabber-vcard-display (jc xml-data)
  "Display received vcard.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((parsed (jabber-vcard-parse (jabber-iq-query xml-data))))
    (dolist (simple-field jabber-vcard-fields)
      (let ((field (assq (car simple-field) parsed)))
	(when field
	  (insert (cdr simple-field))
	  (indent-to 20)
	  (insert (cdr field) "\n"))))

    (let ((names (cdr (assq 'N parsed))))
      (when names
	(insert "\n")
	(dolist (name-field jabber-vcard-name-fields)
	  (let ((field (assq (car name-field) names)))
	    (when field
	      (insert (cdr name-field))
	      (indent-to 20)
	      (insert (cdr field) "\n"))))))

    (let ((email-addresses (cdr (assq 'EMAIL parsed))))
      (when email-addresses
	(insert "\n")
	(insert (jabber-propertize "E-mail addresses:\n"
				   'face 'jabber-title-medium))
	(dolist (email email-addresses)
	  (insert (mapconcat (lambda (type)
			       (cdr (assq type jabber-vcard-email-types)))
			     (car email)
			     " "))
	  (insert ": " (cdr email) "\n"))))

    (let ((phone-numbers (cdr (assq 'TEL parsed))))
      (when phone-numbers
	(insert "\n")
	(insert (jabber-propertize "Phone numbers:\n"
				   'face 'jabber-title-medium))
	(dolist (number phone-numbers)
	  (insert (mapconcat (lambda (type)
			       (cdr (assq type jabber-vcard-phone-types)))
			     (car number)
			     " "))
	  (insert ": " (cdr number) "\n"))))

    (let ((addresses (cdr (assq 'ADR parsed))))
      (when addresses
	(insert "\n")
	(insert (jabber-propertize "Addresses:\n"
				   'face 'jabber-title-medium))
	(dolist (address addresses)
	  (insert (jabber-propertize
		   (mapconcat (lambda (type)
				(cdr (assq type jabber-vcard-address-types)))
			      (car address)
			      " ")
		   'face 'jabber-title-small))
	  (insert "\n")
	  (dolist (address-field jabber-vcard-address-fields)
	    (let ((field (assq (car address-field) address)))
	      (when field
		(insert (cdr address-field))
		(indent-to 20)
		(insert (cdr field) "\n")))))))

    ;; XEP-0153: vCard-based avatars
    (let ((photo-type (nth 1 (assq 'PHOTO parsed)))
	  (photo-binval (nth 2 (assq 'PHOTO parsed))))
      (when (and photo-type photo-binval)
	(condition-case nil
	    ;; ignore the type, let create-image figure it out.
	    (let ((image (jabber-create-image (base64-decode-string photo-binval) nil t)))
	      (insert-image image "[Photo]")
	      (insert "\n"))
	  (error (insert "Couldn't display photo\n")))))))
;; jabber-vcard-display:1 ends here

;; [[file:jabber.org::#vcard-do-edit][jabber-vcard-do-edit:1]]
(defun jabber-vcard-do-edit (jc xml-data closure-data)
  (let ((parsed (jabber-vcard-parse (jabber-iq-query xml-data)))
	start-position)
    (with-current-buffer (get-buffer-create "Edit vcard")
      (jabber-init-widget-buffer nil)

      (setq jabber-buffer-connection jc)

      (setq start-position (point))

      (dolist (simple-field jabber-vcard-fields)
	(widget-insert (cdr simple-field))
	(indent-to 15)
	(let ((default-value (cdr (assq (car simple-field) parsed))))
	  (push (cons (car simple-field)
		      (widget-create 'editable-field (or default-value "")))
		jabber-widget-alist)))

      (widget-insert "\n")
      (push (cons 'N
		  (widget-create
		   '(set :tag "Decomposited name"
			 (cons :tag "Prefix" :format "%t: %v" (const :format "" PREFIX) (string :format "%v"))
			 (cons :tag "Given name" :format "%t: %v" (const :format "" GIVEN) (string :format "%v"))
			 (cons :tag "Middle name" :format "%t: %v" (const :format "" MIDDLE) (string :format "%v"))
			 (cons :tag "Family name" :format "%t: %v" (const :format "" FAMILY) (string :format "%v"))
			 (cons :tag "Suffix" :format "%t: %v" (const :format "" SUFFIX) (string :format "%v")))
		   :value (cdr (assq 'N parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (push (cons 'ADR
		  (widget-create
		   '(repeat :tag "Postal addresses"
			    (cons
			     :tag "Address"
			     (set :tag "Type"
				  (const :tag "Home" HOME)
				  (const :tag "Work" WORK)
				  (const :tag "Postal" POSTAL)
				  (const :tag "Parcel" PARCEL)
				  (const :tag "Domestic" DOM)
				  (const :tag "International" INTL)
				  (const :tag "Preferred" PREF))
			     (set
			      :tag "Address"
			      (cons :tag "Post box" :format "%t: %v"
				    (const :format "" POBOX) (string :format "%v"))
			      (cons :tag "Ext. address" :format "%t: %v"
				    (const :format "" EXTADD) (string :format "%v"))
			      (cons :tag "Street" :format "%t: %v"
				    (const :format "" STREET) (string :format "%v"))
			      (cons :tag "Locality" :format "%t: %v"
				    (const :format "" LOCALITY) (string :format "%v"))
			      (cons :tag "Region" :format "%t: %v"
				    (const :format "" REGION) (string :format "%v"))
			      (cons :tag "Post code" :format "%t: %v"
				    (const :format "" PCODE) (string :format "%v"))
			      (cons :tag "Country" :format "%t: %v"
				    (const :format "" CTRY) (string :format "%v")))))
		   :value (cdr (assq 'ADR parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (push (cons 'TEL
		  (widget-create
		   '(repeat :tag "Phone numbers"
			    (cons :tag "Number"
				  (set :tag "Type"
				       (const :tag "Home" HOME)
				       (const :tag "Work" WORK)
				       (const :tag "Voice" VOICE)
				       (const :tag "Fax" FAX)
				       (const :tag "Pager" PAGER)
				       (const :tag "Message" MSG)
				       (const :tag "Cell phone" CELL)
				       (const :tag "Video" VIDEO)
				       (const :tag "BBS" BBS)
				       (const :tag "Modem" MODEM)
				       (const :tag "ISDN" ISDN)
				       (const :tag "PCS" PCS))
				  (string :tag "Number")))
		   :value (cdr (assq 'TEL parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (push (cons 'EMAIL
		  (widget-create
		   '(repeat :tag "E-mail addresses"
			    (cons :tag "Address"
				  (set :tag "Type"
				       (const :tag "Home" HOME)
				       (const :tag "Work" WORK)
				       (const :tag "Internet" INTERNET)
				       (const :tag "X400" X400)
				       (const :tag "Preferred" PREF))
				  (string :tag "Address")))
		   :value (cdr (assq 'EMAIL parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (widget-insert "Photo/avatar:\n")
      (let* ((photo (assq 'PHOTO parsed))
	     (avatar (when photo
		       (jabber-avatar-from-base64-string (nth 2 photo)
							 (nth 1 photo)))))
	(push (cons
	       'PHOTO
	       (widget-create
		`(radio-button-choice (const :tag "None" nil)
				      ,@(when photo
					  (list
					   `(const :tag
						   ,(concat
						     "Existing: "
						     (jabber-propertize " "
									'display (jabber-avatar-image avatar)))
						   ,(cdr photo))))
				      (file :must-match t :tag "From file"))
		:value (cdr photo)))
	      jabber-widget-alist))

      (widget-insert "\n")
      (widget-create 'push-button :notify #'jabber-vcard-submit "Submit")

      (widget-setup)
      (widget-minor-mode 1)
      (switch-to-buffer (current-buffer))
      (goto-char start-position))))
;; jabber-vcard-do-edit:1 ends here

;; [[file:jabber.org::#vcard-submit][jabber-vcard-submit:1]]
(defun jabber-vcard-submit (&rest ignore)
  (let ((to-publish (jabber-vcard-reassemble
		     (mapcar (lambda (entry)
			       (cons (car entry) (widget-value (cdr entry))))
			     jabber-widget-alist))))
    (jabber-send-iq jabber-buffer-connection nil
		    "set"
		    to-publish
		    #'jabber-report-success "Changing vCard"
		    #'jabber-report-success "Changing vCard")
    (when (bound-and-true-p jabber-vcard-avatars-publish)
      (jabber-vcard-avatars-update-current
       jabber-buffer-connection
       (and jabber-vcard-photo (avatar-sha1-sum jabber-vcard-photo))))))
;; jabber-vcard-submit:1 ends here

;; [[file:jabber.org::#vcard-based-avatars-()][vCard-Based Avatars ([[https://xmpp.org/extensions/xep-0153.html][XEP-0153]]):1]]
(defcustom jabber-vcard-avatars-retrieve (and (fboundp 'display-images-p)
					      (display-images-p))
  "Automatically download vCard avatars?"
  :group 'jabber-avatar
  :type 'boolean)
;; vCard-Based Avatars ([[https://xmpp.org/extensions/xep-0153.html][XEP-0153]]):1 ends here

;; [[file:jabber.org::#vcard-avatars-publish][jabber-vcard-avatars-publish:1]]
(defcustom jabber-vcard-avatars-publish t
  "Publish your vCard photo as avatar?"
  :group 'jabber-avatar
  :type 'boolean)
;; jabber-vcard-avatars-publish:1 ends here

;; [[file:jabber.org::#vcard-avatars-current-hash][jabber-vcard-avatars-current-hash:1]]
(defvar jabber-vcard-avatars-current-hash
  (make-hash-table :test 'equal)
  "For each connection, SHA1 hash of current avatar.
Keys are full JIDs.")
;; jabber-vcard-avatars-current-hash:1 ends here

;; [[file:jabber.org::#vcard-avatars-presence][jabber-vcard-avatars-presence:1]]
(add-to-list 'jabber-presence-chain 'jabber-vcard-avatars-presence)
(defun jabber-vcard-avatars-presence (jc xml-data)
  "Look for vCard avatar mark in <presence/> stanza.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  ;; Only look at ordinary presence
  (when (and jabber-vcard-avatars-retrieve
	     (null (jabber-xml-get-attribute xml-data 'type)))
    (let* ((from (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
	   (photo (jabber-xml-path xml-data '(("vcard-temp:x:update" . "x") photo)))
	   (sha1-hash (car (jabber-xml-node-children photo))))
      (cond
       ((null sha1-hash)
	;; User has removed avatar
	(jabber-avatar-set from nil))
       ((string= sha1-hash (get (jabber-jid-symbol from) 'avatar-hash))
	;; Same avatar as before; do nothing
	)
       ((jabber-avatar-find-cached sha1-hash)
	;; Avatar is cached
	(jabber-avatar-set from sha1-hash))
       (t
	;; Avatar is not cached; retrieve it
	(jabber-vcard-avatars-fetch jc from sha1-hash))))))
;; jabber-vcard-avatars-presence:1 ends here

;; [[file:jabber.org::#vcard-avatars-fetch][jabber-vcard-avatars-fetch:1]]
(defun jabber-vcard-avatars-fetch (jc who sha1-hash)
  "Fetch WHO's vCard, and extract avatar.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Fetch whose vCard avatar: ")
		     nil))
  (jabber-send-iq jc who "get" '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-vcard-avatars-vcard (cons who sha1-hash)
		  #'ignore nil))
;; jabber-vcard-avatars-fetch:1 ends here

;; [[file:jabber.org::#vcard-avatars-vcard][jabber-vcard-avatars-vcard:1]]
(defun jabber-vcard-avatars-vcard (jc iq closure)
  "Get the photo from the vCard, and set the avatar."
  (let ((from (car closure))
	(sha1-hash (cdr closure))
	(photo (assq 'PHOTO (jabber-vcard-parse (jabber-iq-query iq)))))
    (if photo
	(let ((avatar (jabber-avatar-from-base64-string (nth 2 photo)
							(nth 1 photo))))
	  (unless (or (null sha1-hash)
		      (string= sha1-hash (avatar-sha1-sum avatar)))
	    (when jabber-avatar-verbose
	      (message "%s's avatar should have SHA1 sum %s, but has %s"
		       (jabber-jid-displayname from)
		       sha1-hash
		       (avatar-sha1-sum avatar))))
	  (jabber-avatar-cache avatar)
	  (jabber-avatar-set from avatar))
      (jabber-avatar-set from nil))))
;; jabber-vcard-avatars-vcard:1 ends here

;; [[file:jabber.org::#vcard-avatars-find-current][jabber-vcard-avatars-find-current:1]]
(defun jabber-vcard-avatars-find-current (jc)
  "Request our own vCard, to find hash of avatar.

JC is the Jabber connection."
  (when jabber-vcard-avatars-publish
    (jabber-send-iq jc nil "get" '(vCard ((xmlns . "vcard-temp")))
		    #'jabber-vcard-avatars-find-current-1 t
		    #'jabber-vcard-avatars-find-current-1 nil)))
;; jabber-vcard-avatars-find-current:1 ends here

;; [[file:jabber.org::#vcard-avatars-find-current-1][jabber-vcard-avatars-find-current-1:1]]
(defun jabber-vcard-avatars-find-current-1 (jc xml-data success)
  (jabber-vcard-avatars-update-current
   jc
   (and success
	(let ((photo (assq 'PHOTO (jabber-vcard-parse (jabber-iq-query xml-data)))))
	  (when photo
	    (let ((avatar (jabber-avatar-from-base64-string (nth 2 photo)
							    (nth 1 photo))))
	      (avatar-sha1-sum avatar)))))))
;; jabber-vcard-avatars-find-current-1:1 ends here

;; [[file:jabber.org::#vcard-avatars-update-current][jabber-vcard-avatars-update-current:1]]
(defun jabber-vcard-avatars-update-current (jc new-hash)
  (let ((old-hash (gethash
		   (jabber-connection-bare-jid jc)
		   jabber-vcard-avatars-current-hash)))
    (when (not (string= old-hash new-hash))
      (puthash (jabber-connection-bare-jid jc)
	       new-hash jabber-vcard-avatars-current-hash)
      (jabber-send-current-presence jc))))
;; jabber-vcard-avatars-update-current:1 ends here

;; [[file:jabber.org::#vcard-avatars-presence-element][jabber-vcard-avatars-presence-element:1]]
(add-to-list 'jabber-presence-element-functions 'jabber-vcard-avatars-presence-element)
(defun jabber-vcard-avatars-presence-element (jc)
  (when jabber-vcard-avatars-publish
    (let ((hash (gethash
		 (jabber-connection-bare-jid jc)
		 jabber-vcard-avatars-current-hash)))
      (list
       `(x ((xmlns . "vcard-temp:x:update"))
	   ;; if "not yet ready to advertise image", don't.
	   ;; that is, we haven't yet checked what avatar we have.
	   ,(when hash
	      `(photo () ,hash)))))))
;; jabber-vcard-avatars-presence-element:1 ends here

;; [[file:jabber.org::#autoaway][autoaway:1]]
(require 'time-date)
;; autoaway:1 ends here

;; [[file:jabber.org::#autoaway][jabber-autoaway:1]]
(defgroup jabber-autoaway nil
  "Change status to away after idleness."
  :group 'jabber)
;; jabber-autoaway:1 ends here

;; [[file:jabber.org::#autoaway-methods][jabber-autoaway-methods:1]]
(defcustom jabber-autoaway-methods
  (if (fboundp 'jabber-autoaway-method)
      (list jabber-autoaway-method)
    (list 'jabber-current-idle-time
          'jabber-xprintidle-get-idle-time
          'jabber-termatime-get-idle-time))
  "Methods used to keep track of idleness.
This is a list of functions that takes no arguments, and returns the
number of seconds since the user was active, or nil on error."
  :group 'jabber-autoaway
  :options '(jabber-current-idle-time
             jabber-xprintidle-get-idle-time
             jabber-termatime-get-idle-time))
;; jabber-autoaway-methods:1 ends here

;; [[file:jabber.org::#autoaway-timeout][jabber-autoaway-timeout:1]]
(defcustom jabber-autoaway-timeout 5
  "Minutes of inactivity before changing status to away."
  :group 'jabber-autoaway
  :type 'number)
;; jabber-autoaway-timeout:1 ends here

;; [[file:jabber.org::#autoaway-xa-timeout][jabber-autoaway-xa-timeout:1]]
(defcustom jabber-autoaway-xa-timeout 10
  "Minutes of inactivity before changing status to xa.
Set to 0 to disable."
  :group 'jabber-autoaway
  :type 'number)
;; jabber-autoaway-xa-timeout:1 ends here

;; [[file:jabber.org::#autoaway-status][jabber-autoaway-status:1]]
(defcustom jabber-autoaway-status "Idle"
  "Status string for autoaway."
  :group 'jabber-autoaway
  :type 'string)
;; jabber-autoaway-status:1 ends here

;; [[file:jabber.org::#autoaway-xa-status][jabber-autoaway-xa-status:1]]
(defcustom jabber-autoaway-xa-status "Extended away"
  "Status string for autoaway in xa state."
  :group 'jabber-autoaway
  :type 'string)
;; jabber-autoaway-xa-status:1 ends here

;; [[file:jabber.org::#autoaway-priority][jabber-autoaway-priority:1]]
(defcustom jabber-autoaway-priority nil
  "Priority for autoaway.
If nil, don't change priority.  See the manual for more
information about priority."
  :group 'jabber-autoaway
  :type '(choice (const :tag "Don't change")
		 (integer :tag "Priority"))
  :link '(info-link "(jabber)Presence"))
;; jabber-autoaway-priority:1 ends here

;; [[file:jabber.org::#autoaway-xa-priority][jabber-autoaway-xa-priority:1]]
(defcustom jabber-autoaway-xa-priority nil
  "Priority for autoaway in xa state.
If nil, don't change priority.  See the manual for more
information about priority."
  :group 'jabber-autoaway
  :type '(choice (const :tag "Don't change")
		 (integer :tag "Priority"))
  :link '(info-link "(jabber)Presence"))
;; jabber-autoaway-xa-priority:1 ends here

;; [[file:jabber.org::#xprintidle-program][jabber-xprintidle-program:1]]
(defcustom jabber-xprintidle-program (executable-find "xprintidle")
  "Name of the xprintidle program."
  :group 'jabber-autoaway
  :type 'string)
;; jabber-xprintidle-program:1 ends here

;; [[file:jabber.org::#autoaway-verbose][jabber-autoaway-verbose:1]]
(defcustom jabber-autoaway-verbose nil
  "If nil, don't print autoaway status messages."
  :group 'jabber-autoaway
  :type 'boolean)
;; jabber-autoaway-verbose:1 ends here

;; [[file:jabber.org::#autoaway-timer][jabber-autoaway-timer:1]]
(defvar jabber-autoaway-timer nil)
;; jabber-autoaway-timer:1 ends here

;; [[file:jabber.org::#autoaway-last-idle-time][jabber-autoaway-last-idle-time:1]]
(defvar jabber-autoaway-last-idle-time nil
  "Seconds of idle time the last time we checked.
This is used to detect whether the user has become unidle.")
;; jabber-autoaway-last-idle-time:1 ends here

;; [[file:jabber.org::#autoaway-message][jabber-autoaway-message:1]]
(defun jabber-autoaway-message (&rest args)
  (when jabber-autoaway-verbose
    (apply #'message args)))
;; jabber-autoaway-message:1 ends here

;; [[file:jabber.org::#autoaway-start][jabber-autoaway-start:1]]
;;;###autoload
(defun jabber-autoaway-start (&optional ignored)
  "Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'."
  (interactive)
  (unless jabber-autoaway-timer
    (setq jabber-autoaway-timer
	  (run-with-timer (* jabber-autoaway-timeout 60) nil #'jabber-autoaway-timer))
    (jabber-autoaway-message "Autoaway timer started")))
;; jabber-autoaway-start:1 ends here

;; [[file:jabber.org::#autoaway-stop][jabber-autoaway-stop:1]]
(defun jabber-autoaway-stop ()
  "Stop autoaway timer."
  (interactive)
  (when jabber-autoaway-timer
    (jabber-cancel-timer jabber-autoaway-timer)
    (setq jabber-autoaway-timer nil)
    (jabber-autoaway-message "Autoaway timer stopped")))
;; jabber-autoaway-stop:1 ends here

;; [[file:jabber.org::#autoaway-get-idle-time][jabber-autoaway-get-idle-time:1]]
(defun jabber-autoaway-get-idle-time ()
  "Get idle time in seconds according to `jabber-autoaway-methods'.
Return nil on error."
  (car (sort (mapcar 'funcall jabber-autoaway-methods) (lambda (a b) (if a (if b (< a b) t) nil)))))
;; jabber-autoaway-get-idle-time:1 ends here

;; [[file:jabber.org::#autoaway-timer-1][jabber-autoaway-timer:1]]
(defun jabber-autoaway-timer ()
  ;; We use one-time timers, so reset the variable.
  (setq jabber-autoaway-timer nil)
  (let ((idle-time (jabber-autoaway-get-idle-time)))
    (when (numberp idle-time)
      ;; Has "idle timeout" passed?
      (if (> idle-time (* 60 jabber-autoaway-timeout))
	  ;; If so, mark ourselves idle.
	  (jabber-autoaway-set-idle)
	;; Else, start a timer for the remaining amount.
	(setq jabber-autoaway-timer
	      (run-with-timer (- (* 60 jabber-autoaway-timeout) idle-time)
			      nil #'jabber-autoaway-timer))))))
;; jabber-autoaway-timer:1 ends here

;; [[file:jabber.org::#autoaway-set-idle][jabber-autoaway-set-idle:1]]
(defun jabber-autoaway-set-idle (&optional xa)
  (jabber-autoaway-message "Autoaway triggered")
  ;; Send presence, unless the user has set a custom presence
  (unless (member *jabber-current-show* '("xa" "dnd"))
    (jabber-send-presence
     (if xa "xa" "away")
     (if (or (string= *jabber-current-status* jabber-default-status) (string= *jabber-current-status* jabber-autoaway-status)) (if xa jabber-autoaway-xa-status jabber-autoaway-status) *jabber-current-status*)
     (or (if xa jabber-autoaway-priority jabber-autoaway-xa-priority) *jabber-current-priority*)))

  (setq jabber-autoaway-last-idle-time (jabber-autoaway-get-idle-time))
  ;; Run unidle timer every 10 seconds (if xa specified, timer already running)
  (unless xa
    (setq jabber-autoaway-timer (run-with-timer 10 10
					      #'jabber-autoaway-maybe-unidle))))
;; jabber-autoaway-set-idle:1 ends here

;; [[file:jabber.org::#autoaway-maybe-unidle][jabber-autoaway-maybe-unidle:1]]
(defun jabber-autoaway-maybe-unidle ()
  (let ((idle-time (jabber-autoaway-get-idle-time)))
    (jabber-autoaway-message "Idle for %d seconds" idle-time)
    (if (member *jabber-current-show* '("xa" "away"))
        ;; As long as idle time increases monotonically, stay idle.
        (if (> idle-time jabber-autoaway-last-idle-time)
            (progn
              ;; Has "Xa timeout" passed?
              (if (and (> jabber-autoaway-xa-timeout 0) (> idle-time (* 60 jabber-autoaway-xa-timeout)))
                  ;; iIf so, mark ourselves xa.
                  (jabber-autoaway-set-idle t))
              (setq jabber-autoaway-last-idle-time idle-time))
          ;; But if it doesn't, go back to unidle state.
          (jabber-autoaway-message "Back to unidle")
          ;; But don't mess with the user's custom presence.
          (if (or (string= *jabber-current-status* jabber-autoaway-status) (string= *jabber-current-status* jabber-autoaway-xa-status))
              (jabber-send-default-presence)
            (progn
              (jabber-send-presence jabber-default-show *jabber-current-status* jabber-default-priority)
              (jabber-autoaway-message "%S /= %S - not resetting presence" *jabber-current-status* jabber-autoaway-status)))
          (jabber-autoaway-stop)
          (jabber-autoaway-start)))))
;; jabber-autoaway-maybe-unidle:1 ends here

;; [[file:jabber.org::#xprintidle-get-idle-time][jabber-xprintidle-get-idle-time:1]]
(defun jabber-xprintidle-get-idle-time ()
  "Get idle time through the xprintidle program."
  (when jabber-xprintidle-program
    (with-temp-buffer
      (when (zerop (call-process jabber-xprintidle-program
				 nil t))
	(/ (string-to-number (buffer-string)) 1000.0)))))
;; jabber-xprintidle-get-idle-time:1 ends here

;; [[file:jabber.org::#termatime-get-idle-time][jabber-termatime-get-idle-time:1]]
(defun jabber-termatime-get-idle-time ()
  "Get idle time through atime of terminal.
The method for finding the terminal only works on GNU/Linux."
  (let ((terminal (cond
		   ((file-exists-p "/proc/self/fd/0")
		    "/proc/self/fd/0")
		   (t
		    nil))))
    (when terminal
      (let* ((atime-of-tty (nth 4 (file-attributes terminal)))
	     (diff (time-to-seconds (time-since atime-of-tty))))
	(when (> diff 0)
	  diff)))))
;; jabber-termatime-get-idle-time:1 ends here

;; [[file:jabber.org::#current-idle-time][jabber-current-idle-time:1]]
(defun jabber-current-idle-time ()
  "Get idle time through `current-idle-time'.
`current-idle-time' was introduced in Emacs 22."
  (if (fboundp 'current-idle-time)
      (let ((idle-time (current-idle-time)))
        (if (null idle-time)
            0
          (float-time idle-time)))))
;; jabber-current-idle-time:1 ends here

;; [[file:jabber.org::#entity-time-()][Entity Time ([[https://xmpp.org/extensions/xep-0202.html][XEP-0202]]), Legacy Entity Time ([[https://xmpp.org/extensions/xep-0090.html][XEP-0090]]):1]]
(require 'time-date)
;; Entity Time ([[https://xmpp.org/extensions/xep-0202.html][XEP-0202]]), Legacy Entity Time ([[https://xmpp.org/extensions/xep-0090.html][XEP-0090]]):1 ends here

;; [[file:jabber.org::#entity-time-()][Entity Time ([[https://xmpp.org/extensions/xep-0202.html][XEP-0202]]), Legacy Entity Time ([[https://xmpp.org/extensions/xep-0090.html][XEP-0090]]):2]]
(add-to-list 'jabber-jid-info-menu (cons "Request time" 'jabber-get-time))
;; Entity Time ([[https://xmpp.org/extensions/xep-0202.html][XEP-0202]]), Legacy Entity Time ([[https://xmpp.org/extensions/xep-0090.html][XEP-0090]]):2 ends here

;; [[file:jabber.org::#get-time][jabber-get-time:1]]
(defun jabber-get-time (jc to)
  "Request time.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Request time of: "
                                                 nil nil nil 'full t)))

  (jabber-send-iq jc to "get"
                  '(time ((xmlns . "urn:xmpp:time")))
                  'jabber-silent-process-data 'jabber-process-time
                  'jabber-silent-process-data
                  (lambda (jc xml-data)
                    (let ((from (jabber-xml-get-attribute xml-data 'from)))
                      (jabber-get-legacy-time jc from)))))
;; jabber-get-time:1 ends here

;; [[file:jabber.org::#get-legacy-time][jabber-get-legacy-time:1]]
(defun jabber-get-legacy-time (jc to)
  "Request legacy time.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Request time of: "
                                                 nil nil nil 'full t)))

  (jabber-send-iq jc to
                  "get"
                  '(query ((xmlns . "jabber:iq:time")))
                  'jabber-silent-process-data 'jabber-process-legacy-time
                  'jabber-silent-process-data "Time request failed"))
;; jabber-get-legacy-time:1 ends here

;; [[file:jabber.org::#process-time][jabber-process-time:1]]
;; called by jabber-process-data
(defun jabber-process-time (jc xml-data)
  "Handle results from urn:xmpp:time requests.

JC is the Jabber Connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (time (or (car (jabber-xml-get-children xml-data 'time))
                   ;; adium response of qeury
                   (car (jabber-xml-get-children xml-data 'query))))
         (tzo (car (jabber-xml-node-children
                    (car (jabber-xml-get-children time 'tzo)))))
         (utc (car (jabber-xml-node-children
                    (car (jabber-xml-get-children time 'utc))))))
    (when (and utc tzo)
      (format "%s has time: %s %s"
              from (format-time-string "%Y-%m-%d %T" (jabber-parse-time utc)) tzo))))
;; jabber-process-time:1 ends here

;; [[file:jabber.org::#process-legacy-time][jabber-process-legacy-time:1]]
(defun jabber-process-legacy-time (jc xml-data)
  "Handle results from jabber:iq:time requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (query (jabber-iq-query xml-data))
         (display
          (car (jabber-xml-node-children
                (car (jabber-xml-get-children
                      query 'display)))))
         (utc
          (car (jabber-xml-node-children
                (car (jabber-xml-get-children
                      query 'utc)))))
         (tz
          (car (jabber-xml-node-children
                (car (jabber-xml-get-children
                      query 'tz))))))
    (format "%s has time: %s" from
           (cond
            (display display)
            (utc
             (concat
              (format-time-string "%Y-%m-%d %T" (jabber-parse-legacy-time utc))
              (when tz
                (concat " " tz))))))))
;; jabber-process-legacy-time:1 ends here

;; [[file:jabber.org::#get-last-online][jabber-get-last-online:1]]
(defun jabber-get-last-online (jc to)
  "Request time since a user was last online, or uptime of a component.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Get last online for: "
						 nil nil nil 'bare-or-muc)))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:last")))
		  #'jabber-silent-process-data #'jabber-process-last
		  #'jabber-silent-process-data "Last online request failed"))
;; jabber-get-last-online:1 ends here

;; [[file:jabber.org::#get-idle-time][jabber-get-idle-time:1]]
(defun jabber-get-idle-time (jc to)
  "Request idle time of user.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Get idle time for: "
						 nil nil nil 'full t)))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:last")))
		  #'jabber-silent-process-data #'jabber-process-last
		  #'jabber-silent-process-data "Idle time request failed"))
;; jabber-get-idle-time:1 ends here

;; [[file:jabber.org::#process-last][jabber-process-last:1]]
(defun jabber-process-last (jc xml-data)
  "Handle resultts from jabber:iq:last requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (seconds (jabber-xml-get-attribute query 'seconds))
	 (message (car (jabber-xml-node-children query))))
    (cond
     ((jabber-jid-resource from)
      ;; Full JID: idle time
      (format "%s idle for %s seconds" from seconds))
     ((jabber-jid-username from)
      ;; Bare JID with username: time since online
      (concat
       (format "%s last online %s seconds ago" from seconds)
       (let ((seconds (condition-case nil
                          (string-to-number seconds)
                        (error nil))))
         (when (numberp seconds)
	   (concat
	    " - that is, at "
	    (format-time-string "%Y-%m-%d %T"
				(time-subtract (current-time)
					       (seconds-to-time seconds)))
	    "\n")))))
     (t
      ;; Only hostname: uptime
      (format "%s uptime: %s seconds" from seconds)))))
;; jabber-process-last:1 ends here

;; [[file:jabber.org::#process-last][jabber-process-last:2]]
(add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:time" 'jabber-return-legacy-time))
(jabber-disco-advertise-feature "jabber:iq:time")
;; jabber-process-last:2 ends here

;; [[file:jabber.org::#return-legacy-time][jabber-return-legacy-time:1]]
(defun jabber-return-legacy-time (jc xml-data)
  "Return client time as defined in XEP-0090.
Sender and ID are determined from the incoming packet passed in XML-DATA.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result"
		    `(query ((xmlns . "jabber:iq:time"))
			    ;; what is ``human-readable'' format?
			    ;; the same way as formating using by tkabber
			    (display () ,(format-time-string "%a %b %d %H:%M:%S %Z %Y"))
			    (tz () ,(format-time-string "%Z"))
			    (utc () ,(jabber-encode-legacy-time nil)))
		    nil nil nil nil
		    id)))
;; jabber-return-legacy-time:1 ends here

;; [[file:jabber.org::#return-legacy-time][jabber-return-legacy-time:2]]
(add-to-list 'jabber-iq-get-xmlns-alist (cons "urn:xmpp:time" 'jabber-return-time))
(jabber-disco-advertise-feature "urn:xmpp:time")
;; jabber-return-legacy-time:2 ends here

;; [[file:jabber.org::#return-time][jabber-return-time:1]]
(defun jabber-return-time (jc xml-data)
  "Return client time as defined in XEP-0202.
Sender and ID are determined from the incoming packet passed in XML-DATA.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
        (id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result"
                    `(time ((xmlns . "urn:xmpp:time"))
                           (utc () ,(jabber-encode-time nil))
                           (tzo () ,(jabber-encode-timezone)))
                    nil nil nil nil
                    id)))
;; jabber-return-time:1 ends here

;; [[file:jabber.org::#return-time][jabber-return-time:2]]
(add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:last" 'jabber-return-last))
(jabber-disco-advertise-feature "jabber:iq:last")
;; jabber-return-time:2 ends here

;; [[file:jabber.org::#return-last][jabber-return-last:1]]
(defun jabber-return-last (jc xml-data)
  (let ((to (jabber-xml-get-attribute xml-data 'from))
        (id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result"
                    `(time ((xmlns . "jabber:iq:last")
			    ;; XEP-0012 specifies that this is an integer.
                            (seconds . ,(number-to-string
					 (floor (jabber-autoaway-get-idle-time))))))
                    nil nil nil nil
                    id)))
;; jabber-return-last:1 ends here

;; [[file:jabber.org::#log-lines-to-keep][jabber-log-lines-to-keep:1]]
(defvar jabber-log-lines-to-keep 1000
  "Maximum number of lines in chat buffer.")
;; jabber-log-lines-to-keep:1 ends here

;; [[file:jabber.org::#truncate-top][jabber-truncate-top:1]]
(defun jabber-truncate-top (buffer &optional ewoc)
  "Clean old history from a chat BUFFER.
Optional EWOC is ewoc-widget to work.  Default is `jabber-chat-ewoc'
`jabber-log-lines-to-keep' specifies the number of lines to
keep.

Note that this might interfer with
`jabber-chat-display-more-backlog': you ask for more history, you
get it, and then it just gets deleted."
  (interactive)
    (let* ((inhibit-read-only t)
           (work-ewoc (if ewoc ewoc jabber-chat-ewoc))
          (delete-before
           ;; go back one node, to make this function "idempotent"
           (ewoc-prev
            work-ewoc
            (ewoc-locate work-ewoc
                         (save-excursion
                           (set-buffer buffer)
                           (goto-char (point-max))
                           (forward-line (- jabber-log-lines-to-keep))
                           (point))))))
      (while delete-before
        (setq delete-before
              (prog1
                  (ewoc-prev work-ewoc delete-before)
                (ewoc-delete work-ewoc delete-before))))))
;; jabber-truncate-top:1 ends here

;; [[file:jabber.org::#truncate-muc][jabber-truncate-muc:1]]
(defun jabber-truncate-muc (nick group buffer text proposed-alert)
  "Clean old history from MUC buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep."
  (jabber-truncate-top buffer))
;; jabber-truncate-muc:1 ends here

;; [[file:jabber.org::#truncate-chat][jabber-truncate-chat:1]]
(defun jabber-truncate-chat (from buffer text proposed-alert)
  "Clean old history from chat buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep.

Note that this might interfer with
`jabber-chat-display-more-backlog': you ask for more history, you
get it, and then it just gets deleted."
  (jabber-truncate-top buffer))
;; jabber-truncate-chat:1 ends here

;; [[file:jabber.org::#carbon-success][jabber-carbon-success:1]]
(defun jabber-carbon-success (jc xml-data context)
  (when (equal "result" (jabber-xml-get-attribute xml-data 'type))
    (message "Carbons feature successfully enabled")))
;; jabber-carbon-success:1 ends here

;; [[file:jabber.org::#carbon-failure][jabber-carbon-failure:1]]
(defun jabber-carbon-failure (jc xml-data context)
  (message "Carbons feature could not be enabled: %S" xml-data))
;; jabber-carbon-failure:1 ends here

;; [[file:jabber.org::#enable-carbons][jabber-enable-carbons:1]]
(add-to-list 'jabber-jid-service-menu
             (cons "Enable Carbons" 'jabber-enable-carbons))
(defun jabber-enable-carbons (jc)
  "Send request to enable XEP-0280 Message Carbons.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc
                  nil
                  "set"
                  `(enable ((xmlns . "urn:xmpp:carbons:2")))
                  #'jabber-carbon-success "Carbons feature enablement"
                  #'jabber-carbon-failure "Carbons feature enablement"))
;; jabber-enable-carbons:1 ends here

;; [[file:jabber.org::#handling-incoming-events][Handling incoming events:1]]
;;;###autoload
(eval-after-load "jabber-disco"
  '(jabber-disco-advertise-feature "urn:xmpp:rtt:0"))
;; Handling incoming events:1 ends here

;; [[file:jabber.org::#rtt-ewoc-node][jabber-rtt-ewoc-node:1]]
(defvar jabber-rtt-ewoc-node nil)
(make-variable-buffer-local 'jabber-rtt-ewoc-node)
;; jabber-rtt-ewoc-node:1 ends here

;; [[file:jabber.org::#rtt-last-seq][jabber-rtt-last-seq:1]]
(defvar jabber-rtt-last-seq nil)
(make-variable-buffer-local 'jabber-rtt-last-seq)
;; jabber-rtt-last-seq:1 ends here

;; [[file:jabber.org::#rtt-message][jabber-rtt-message:1]]
(defvar jabber-rtt-message nil)
(make-variable-buffer-local 'jabber-rtt-message)
;; jabber-rtt-message:1 ends here

;; [[file:jabber.org::#rtt-pending-events][jabber-rtt-pending-events:1]]
(defvar jabber-rtt-pending-events nil)
(make-variable-buffer-local 'jabber-rtt-pending-events)
;; jabber-rtt-pending-events:1 ends here

;; [[file:jabber.org::#rtt-timer][jabber-rtt-timer:1]]
(defvar jabber-rtt-timer nil)
(make-variable-buffer-local 'jabber-rtt-timer)
;; jabber-rtt-timer:1 ends here

;; [[file:jabber.org::#rtt-handle-message][jabber-rtt-handle-message:1]]
;;;###autoload
(eval-after-load "jabber-core"
  '(add-to-list 'jabber-message-chain #'jabber-rtt-handle-message t))
;; jabber-rtt-handle-message:1 ends here

;; [[file:jabber.org::#rtt-handle-message][jabber-rtt-handle-message:2]]
;;;###autoload
(defun jabber-rtt-handle-message (jc xml-data)
  ;; We could support this for MUC as well, if useful.
  (when (and (not (jabber-muc-message-p xml-data))
	     (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))))
    (with-current-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))
      (let* ((rtt (jabber-xml-path xml-data '(("urn:xmpp:rtt:0" . "rtt"))))
	     (body (jabber-xml-path xml-data '(body)))
	     (seq (when rtt (jabber-xml-get-attribute rtt 'seq)))
	     (event (when rtt (or (jabber-xml-get-attribute rtt 'event) "edit")))
	     (actions (when rtt (jabber-xml-node-children rtt)))
	     (inhibit-read-only t))
	(cond
	 ((or body (string= event "cancel"))
	  ;; A <body/> element supersedes real time text.
	  (jabber-rtt--reset))
	 ((member event '("new" "reset"))
	  (jabber-rtt--reset)
	  (setq jabber-rtt-ewoc-node
		(ewoc-enter-last jabber-chat-ewoc (list :notice "[typing...]"))
		jabber-rtt-last-seq (string-to-number seq)
		jabber-rtt-message ""
		jabber-rtt-pending-events nil)
	  (jabber-rtt--enqueue-actions actions))
	 ((string= event "edit")
	  ;; TODO: check whether this works properly in 32-bit Emacs
	  (cond
	   ((and jabber-rtt-last-seq
		 (equal (1+ jabber-rtt-last-seq)
			(string-to-number seq)))
	    ;; We are in sync.
	    (setq jabber-rtt-last-seq (string-to-number seq))
	    (jabber-rtt--enqueue-actions actions))
	   (t
	    ;; TODO: show warning when not in sync
	    (message "out of sync! %s vs %s"
		     seq jabber-rtt-last-seq))))
	 ;; TODO: handle event="init"
	 )))))
;; jabber-rtt-handle-message:2 ends here

;; [[file:jabber.org::#rtt-reset][jabber-rtt--reset:1]]
(defun jabber-rtt--reset ()
  (when jabber-rtt-ewoc-node
    (ewoc-delete jabber-chat-ewoc jabber-rtt-ewoc-node))
  (when (timerp jabber-rtt-timer)
    (cancel-timer jabber-rtt-timer))
  (setq jabber-rtt-ewoc-node nil
	jabber-rtt-last-seq nil
	jabber-rtt-message nil
	jabber-rtt-pending-events nil
	jabber-rtt-timer nil))
;; jabber-rtt--reset:1 ends here

;; [[file:jabber.org::#rtt-enqueue-actions][jabber-rtt--enqueue-actions:1]]
(defun jabber-rtt--enqueue-actions (new-actions)
  (setq jabber-rtt-pending-events
	;; Ensure that the queue never contains more than 700 ms worth
	;; of wait events.
	(jabber-rtt--fix-waits (append jabber-rtt-pending-events new-actions)))
  (unless jabber-rtt-timer
    (jabber-rtt--process-actions (current-buffer))))
;; jabber-rtt--enqueue-actions:1 ends here

;; [[file:jabber.org::#rtt-process-actions][jabber-rtt--process-actions:1]]
(defun jabber-rtt--process-actions (buffer)
  (with-current-buffer buffer
    (setq jabber-rtt-timer nil)
    (catch 'wait
      (while jabber-rtt-pending-events
	(let ((action (pop jabber-rtt-pending-events)))
	  (cl-case (jabber-xml-node-name action)
	    ((t)
	     ;; insert text
	     (let* ((p (jabber-xml-get-attribute action 'p))
		    (position (if p (string-to-number p) (length jabber-rtt-message))))
	       (setq position (max position 0))
	       (setq position (min position (length jabber-rtt-message)))
	       (setf (substring jabber-rtt-message position position)
		     (car (jabber-xml-node-children action)))

	       (ewoc-set-data jabber-rtt-ewoc-node (list :notice (concat "[typing...] " jabber-rtt-message)))
	       (let ((inhibit-read-only t))
		 (ewoc-invalidate jabber-chat-ewoc jabber-rtt-ewoc-node))))
	    ((e)
	     ;; erase text
	     (let* ((p (jabber-xml-get-attribute action 'p))
		    (position (if p (string-to-number p) (length jabber-rtt-message)))
		    (n (jabber-xml-get-attribute action 'n))
		    (number (if n (string-to-number n) 1)))
	       (setq position (max position 0))
	       (setq position (min position (length jabber-rtt-message)))
	       (setq number (max number 0))
	       (setq number (min number position))
	       ;; Now erase the NUMBER characters before POSITION.
	       (setf (substring jabber-rtt-message (- position number) position)
		     "")

	       (ewoc-set-data jabber-rtt-ewoc-node (list :notice (concat "[typing...] " jabber-rtt-message)))
	       (let ((inhibit-read-only t))
		 (ewoc-invalidate jabber-chat-ewoc jabber-rtt-ewoc-node))))
	    ((w)
	     (setq jabber-rtt-timer
		   (run-with-timer
		    (/ (string-to-number (jabber-xml-get-attribute action 'n)) 1000.0)
		    nil
		    #'jabber-rtt--process-actions
		    buffer))
	     (throw 'wait nil))))))))
;; jabber-rtt--process-actions:1 ends here

;; [[file:jabber.org::#rtt-fix-waits][jabber-rtt--fix-waits:1]]
(defun jabber-rtt--fix-waits (actions)
  ;; Ensure that the sum of all wait events is no more than 700 ms.
  (let ((sum 0))
    (dolist (action actions)
      (when (eq (jabber-xml-node-name action) 'w)
	(let ((n (jabber-xml-get-attribute action 'n)))
	  (setq n (string-to-number n))
	  (when (>= n 0)
	    (setq sum (+ sum n))))))

    (if (<= sum 700)
	actions
      (let ((scale (/ 700.0 sum)))
	(mapcar
	 (lambda (action)
	   (if (eq (jabber-xml-node-name action) 'w)
	       (let ((n (jabber-xml-get-attribute action 'n)))
		 (setq n (string-to-number n))
		 (setq n (max n 0))
		 `(w ((n . ,(number-to-string (* scale n)))) nil))
	     action))
	 actions)))))
;; jabber-rtt--fix-waits:1 ends here

;; [[file:jabber.org::#rtt-send-timer][jabber-rtt-send-timer:1]]
(defvar jabber-rtt-send-timer nil)
(make-variable-buffer-local 'jabber-rtt-send-timer)
;; jabber-rtt-send-timer:1 ends here

;; [[file:jabber.org::#rtt-send-seq][jabber-rtt-send-seq:1]]
(defvar jabber-rtt-send-seq nil)
(make-variable-buffer-local 'jabber-rtt-send-seq)
;; jabber-rtt-send-seq:1 ends here

;; [[file:jabber.org::#rtt-outgoing-events][jabber-rtt-outgoing-events:1]]
(defvar jabber-rtt-outgoing-events nil)
(make-variable-buffer-local 'jabber-rtt-outgoing-events)
;; jabber-rtt-outgoing-events:1 ends here

;; [[file:jabber.org::#rtt-send-last-timestamp][jabber-rtt-send-last-timestamp:1]]
(defvar jabber-rtt-send-last-timestamp nil)
(make-variable-buffer-local 'jabber-rtt-send-last-timestamp)
;; jabber-rtt-send-last-timestamp:1 ends here

;; [[file:jabber.org::#rtt-send-mode][jabber-rtt-send-mode:1]]
;;;###autoload
(define-minor-mode jabber-rtt-send-mode
  "Show text to recipient as it is being typed.
This lets the recipient see every change made to the message up
until it's sent.  The recipient's client needs to implement
XEP-0301, In-Band Real Time Text."
  nil " Real-Time" nil
  (if (null jabber-rtt-send-mode)
      (progn
	(remove-hook 'after-change-functions #'jabber-rtt--queue-update t)
	(remove-hook 'jabber-chat-send-hooks #'jabber-rtt--message-sent t)
	(jabber-rtt--cancel-send))
    (unless (derived-mode-p 'jabber-chat-mode)
      (error "Real Time Text only makes sense in chat buffers"))
    (when (timerp jabber-rtt-send-timer)
      (cancel-timer jabber-rtt-send-timer))
    (setq jabber-rtt-send-timer nil
	  jabber-rtt-send-seq nil
	  jabber-rtt-outgoing-events nil
	  jabber-rtt-send-last-timestamp nil)
    (jabber-rtt--send-current-text nil)
    (add-hook 'after-change-functions #'jabber-rtt--queue-update nil t)
    (add-hook 'jabber-chat-send-hooks #'jabber-rtt--message-sent nil t)))
;; jabber-rtt-send-mode:1 ends here

;; [[file:jabber.org::#rtt-cancel-send][jabber-rtt--cancel-send:1]]
(defun jabber-rtt--cancel-send ()
  (when (timerp jabber-rtt-send-timer)
    (cancel-timer jabber-rtt-send-timer))
  (setq jabber-rtt-send-seq (1+ jabber-rtt-send-seq))
  (jabber-send-sexp jabber-buffer-connection
		    `(message ((to . ,jabber-chatting-with)
			       (type . "chat"))
			      (rtt ((xmlns . "urn:xmpp:rtt:0")
				    (seq . ,(number-to-string jabber-rtt-send-seq))
				    (event . "cancel"))
				   nil)))
  (setq jabber-rtt-send-timer nil
	jabber-rtt-send-seq nil
	jabber-rtt-outgoing-events nil
	jabber-rtt-send-last-timestamp nil))
;; jabber-rtt--cancel-send:1 ends here

;; [[file:jabber.org::#rtt-send-current-text][jabber-rtt--send-current-text:1]]
(defun jabber-rtt--send-current-text (resetp)
  (let ((text (buffer-substring-no-properties jabber-point-insert (point-max))))
    ;; This should give us enough room to avoid wrap-arounds, even
    ;; with just 28 bits...
    (setq jabber-rtt-send-seq (random 100000))
    (jabber-send-sexp jabber-buffer-connection
		      `(message ((to . ,jabber-chatting-with)
				 (type . "chat"))
				(rtt ((xmlns . "urn:xmpp:rtt:0")
				      (seq . ,(number-to-string jabber-rtt-send-seq))
				      (event . ,(if resetp "reset" "new")))
				     (t () ,text))))))
;; jabber-rtt--send-current-text:1 ends here

;; [[file:jabber.org::#rtt-queue-update][jabber-rtt--queue-update:1]]
(defun jabber-rtt--queue-update (beg end pre-change-length)
  (unless (or (< beg jabber-point-insert)
	      (< end jabber-point-insert))
    (let ((timestamp (current-time)))
      (when jabber-rtt-send-last-timestamp
	(let* ((time-difference (time-subtract timestamp jabber-rtt-send-last-timestamp))
	       (interval (truncate (* 1000 (float-time time-difference)))))
	  (when (and (> interval 0)
		     ;; Don't send too long intervals - this should have
		     ;; been sent by our timer already.
		     (< interval 1000))
	    (push `(w ((n . ,(number-to-string interval))) nil)
		  jabber-rtt-outgoing-events))))
      (setq jabber-rtt-send-last-timestamp timestamp))

    (when (> pre-change-length 0)
      ;; Some text was deleted.  Let's check if we can use a shorter
      ;; tag:
      (let ((at-end (= end (point-max)))
	    (erase-one (= pre-change-length 1)))
	(push `(e (
		   ,@(unless at-end
		       `((p . ,(number-to-string
				(+ beg
				   (- jabber-point-insert)
				   pre-change-length)))))
		   ,@(unless erase-one
		       `((n . ,(number-to-string pre-change-length))))))
	      jabber-rtt-outgoing-events)))

    (when (/= beg end)
      ;; Some text was inserted.
      (let ((text (buffer-substring-no-properties beg end))
	    (at-end (= end (point-max))))
	(push `(t (
		   ,@(unless at-end
		       `((p . ,(number-to-string (- beg jabber-point-insert))))))
		  ,text)
	      jabber-rtt-outgoing-events)))

    (when (null jabber-rtt-send-timer)
      (setq jabber-rtt-send-timer
	    (run-with-timer 0.7 nil #'jabber-rtt--send-queued-events (current-buffer))))))
;; jabber-rtt--queue-update:1 ends here

;; [[file:jabber.org::#rtt-send-queued-events][jabber-rtt--send-queued-events:1]]
(defun jabber-rtt--send-queued-events (buffer)
  (with-current-buffer buffer
    (setq jabber-rtt-send-timer nil)
    (when jabber-rtt-outgoing-events
      (let ((event (if jabber-rtt-send-seq "edit" "new")))
	(setq jabber-rtt-send-seq
	      (if jabber-rtt-send-seq
		  (1+ jabber-rtt-send-seq)
		(random 100000)))
	(jabber-send-sexp jabber-buffer-connection
			  `(message ((to . ,jabber-chatting-with)
				     (type . "chat"))
				    (rtt ((xmlns . "urn:xmpp:rtt:0")
					  (seq . ,(number-to-string jabber-rtt-send-seq))
					  (event . ,event))
					 ,@(nreverse jabber-rtt-outgoing-events))))
	(setq jabber-rtt-outgoing-events nil)))))
;; jabber-rtt--send-queued-events:1 ends here

;; [[file:jabber.org::#rtt-message-sent][jabber-rtt--message-sent:1]]
(defun jabber-rtt--message-sent (_text _id)
  ;; We're sending a <body/> element; reset our state
  (when (timerp jabber-rtt-send-timer)
    (cancel-timer jabber-rtt-send-timer))
  (setq jabber-rtt-send-timer nil
	jabber-rtt-send-seq nil
	jabber-rtt-outgoing-events nil
	jabber-rtt-send-last-timestamp nil))
;; jabber-rtt--message-sent:1 ends here

;; [[file:jabber.org::#jabber][Jabber:1]]
;;; load Unicode tables if this needed
(when (and (featurep 'xemacs) (not (emacs-version>= 21 5 5)))
    (require 'un-define))
;; Jabber:1 ends here

;; [[file:jabber.org::#1][jabber:1]]
;;; these customize fields should come first
(defgroup jabber nil "Jabber instant messaging"
  :group 'applications)
;; jabber:1 ends here

;; [[file:jabber.org::#account-list][jabber-account-list:1]]
;;;###autoload
(defcustom jabber-account-list nil
  "List of Jabber accounts.
Each element of the list is a cons cell describing a Jabber account,
where the car is a JID and the CDR is an alist.

JID is a full Jabber ID string (e.g.  foo@bar.tld).  You can also
specify the resource (e.g.  foo@bar.tld/emacs).
The following keys can be present in the alist:

  :password is a string to authenticate ourself against the server.
  It can be empty.  If you don't want to store your password in your
  Emacs configuration, try auth-source (info node `(auth)Top').

  :network-server is a string identifying the address to connect to,
  if it's different from the server part of the JID.

  :port is the port to use (default depends on connection type).

  :connection-type is a symbol.  Valid symbols are `starttls',
  `network' and `ssl'.

Only JID is mandatory.  The rest can be guessed at run-time.

Examples:

Two accounts without any special configuration:
\((\"foo@example.com\") (\"bar@example.net\"))

One disabled account with a non-standard port:
\((\"romeo@montague.net\" (:port . 5242) (:disabled . t)))

If you don't have SRV and STARTTLS capabilities in your Emacs,
configure a Google Talk account like this:
\((\"username@gmail.com\"
  (:network-server . \"talk.google.com\")
  (:connection-type . ssl)))"
  :type '(repeat
	  (cons :tag "Account information"
		(string :tag "JID")
		(set :format "%v"
		     (cons :format "%v"
			   (const :format "" :disabled)
			   (const :tag "Disabled" t))
		     (cons :format "%v"
			   (const :format "" :password)
			   (string :tag "Password"))
		     (cons :format "%v"
			   (const :format "" :network-server)
			   (string :tag "Network server"))
		     (cons :format "%v"
			   (const :format "" :port)
			   (integer :tag "Port" 5222))
		     (cons :format "%v"
			   (const :format "" :connection-type)
			   (choice :tag "Connection type"
				   ;; XXX: detect whether we have STARTTLS?  option
				   ;; for enforcing encryption?
				   (const :tag "STARTTLS" starttls)
				   (const :tag "Unencrypted" network)
				   (const :tag "Legacy SSL/TLS" ssl))))))
  :group 'jabber)
;; jabber-account-list:1 ends here

;; [[file:jabber.org::#default-show][jabber-default-show:1]]
(defcustom jabber-default-show ""
  "Default show state."
  :type '(choice (const :tag "Online" "")
		 (const :tag "Chatty" "chat")
		 (const :tag "Away" "away")
		 (const :tag "Extended away" "xa")
		 (const :tag "Do not disturb" "dnd"))
  :group 'jabber)
;; jabber-default-show:1 ends here

;; [[file:jabber.org::#default-status][jabber-default-status:1]]
(defcustom jabber-default-status ""
  "Default status string."
  :type 'string
  :group 'jabber)
;; jabber-default-status:1 ends here

;; [[file:jabber.org::#default-priority][jabber-default-priority:1]]
(defcustom jabber-default-priority 10
  "Default priority."
  :type 'integer
  :group 'jabber)
;; jabber-default-priority:1 ends here

;; [[file:jabber.org::#*jabber-current-status*][*jabber-current-status*:1]]
;;;###autoload
(defvar *jabber-current-status* nil
  "The users current presence status.")
;; *jabber-current-status*:1 ends here

;; [[file:jabber.org::#*jabber-current-show*][*jabber-current-show*:1]]
;;;###autoload
(defvar *jabber-current-show* nil
  "The users current presence show.")
;; *jabber-current-show*:1 ends here

;; [[file:jabber.org::#*jabber-current-priority*][*jabber-current-priority*:1]]
;;;###autoload
(defvar *jabber-current-priority* nil
  "The user's current priority.")
;; *jabber-current-priority*:1 ends here

;; [[file:jabber.org::#*jabber-status-history*][*jabber-status-history*:1]]
(defvar *jabber-status-history* nil
  "History of status messages.")
;; *jabber-status-history*:1 ends here

;; [[file:jabber.org::#faces][jabber-faces:1]]
(defgroup jabber-faces nil "Faces for displaying jabber instant messaging."
  :group 'jabber)
;; jabber-faces:1 ends here

;; [[file:jabber.org::#title-small][jabber-title-small:1]]
(defface jabber-title-small
  '((t (:weight bold :width semi-expanded :height 1.0 :inherit variable-pitch)))
  "Face for small titles."
  :group 'jabber-faces)
;; jabber-title-small:1 ends here

;; [[file:jabber.org::#title-medium][jabber-title-medium:1]]
(defface jabber-title-medium
  '((t (:weight bold :width expanded :height 2.0 :inherit variable-pitch)))
  "Face for medium titles."
  :group 'jabber-faces)
;; jabber-title-medium:1 ends here

;; [[file:jabber.org::#title-large][jabber-title-large:1]]
(defface jabber-title-large
  '((t (:weight bold :width ultra-expanded :height 3.0 :inherit variable-pitch)))
  "Face for large titles."
  :group 'jabber-faces)
;; jabber-title-large:1 ends here

;; [[file:jabber.org::#debug][jabber-debug:1]]
(defgroup jabber-debug nil "debugging options"
  :group 'jabber)
;; jabber-debug:1 ends here

;; [[file:jabber.org::#debug-log-xml][jabber-debug-log-xml:1]]
(defcustom jabber-debug-log-xml nil
  "Set to non-nil to log all XML i/o in *-jabber-console-JID-* buffer.
Set to string to also dump XML i/o in specified file."
  :type '(choice (const :tag "Do not dump XML i/o" nil)
                 (const :tag "Dump XML i/o in console" t)
                 (string :tag "Dump XML i/o in console and this file"))
  :group 'jabber-debug)
;; jabber-debug-log-xml:1 ends here

;; [[file:jabber.org::#debug-keep-process-buffers][jabber-debug-keep-process-buffers:1]]
(defcustom jabber-debug-keep-process-buffers nil
  "If nil, kill process buffers when the process dies.
Contents of process buffers might be useful for debugging."
  :type 'boolean
  :group 'jabber-debug)
;; jabber-debug-keep-process-buffers:1 ends here

;; [[file:jabber.org::#silent-mode][jabber-silent-mode:1]]
(defcustom jabber-silent-mode nil
  "If non-nil, do not ask for confirmation for some operations.  DANGEROUS!"
  :type 'boolean
  :group 'jabber)
;; jabber-silent-mode:1 ends here

;; [[file:jabber.org::#presence-faces][jabber-presence-faces:1]]
;;;###autoload
(defconst jabber-presence-faces
 '(("" . jabber-roster-user-online)
   ("away" . jabber-roster-user-away)
   ("xa" . jabber-roster-user-xa)
   ("dnd" . jabber-roster-user-dnd)
   ("chat" . jabber-roster-user-chatty)
   ("error" . jabber-roster-user-error)
   (nil . jabber-roster-user-offline))
 "Mapping from presence types to faces.")
;; jabber-presence-faces:1 ends here

;; [[file:jabber.org::#presence-strings][jabber-presence-strings:1]]
(defconst jabber-presence-strings
  `(("" . ,(jabber-propertize "Online" 'face 'jabber-roster-user-online))
    ("away" . ,(jabber-propertize "Away" 'face 'jabber-roster-user-away))
    ("xa" . ,(jabber-propertize "Extended Away" 'face 'jabber-roster-user-xa))
    ("dnd" . ,(jabber-propertize "Do not Disturb" 'face 'jabber-roster-user-dnd))
    ("chat" . ,(jabber-propertize "Chatty" 'face 'jabber-roster-user-chatty))
    ("error" . ,(jabber-propertize "Error" 'face 'jabber-roster-user-error))
    (nil . ,(jabber-propertize "Offline" 'face 'jabber-roster-user-offline)))
  "Mapping from presence types to readable, colorized strings.")
;; jabber-presence-strings:1 ends here

;; [[file:jabber.org::#customize][jabber-customize:1]]
;;;###autoload
(defun jabber-customize ()
  "Customize jabber options."
  (interactive)
  (customize-group 'jabber))
;; jabber-customize:1 ends here

;; [[file:jabber.org::#info][jabber-info:1]]
;;;###autoload
(defun jabber-info ()
  "Open jabber.el manual."
  (interactive)
  (info "jabber"))
;; jabber-info:1 ends here

;; [[file:jabber.org::#info][jabber-info:2]]
(provide 'jabber)

;;; jabber.el ends here
;; jabber-info:2 ends here
