;;; jabber.el --- a minimal jabber client  -*- lexical-binding: t; -*-

;; Author: Magnus Henoch <mange@freemail.hu>
;; Maintainer: wgreenhouse <wgreenhouse@tilde.club>
;; Keywords: comm
;; Homepage: https://codeberg.org/emacs-jabber/emacs-jabber
;; Package-Requires: ((hexrgb "21.0") (emacs "27.1") (fsm "0.1.0") (srv "0.1.0"))
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
;; jabber.el is an XMPP client for Emacs. XMPP (also known as
;; 'Jabber') is the IETF-standard federated instant messaging protocol
;; - see http://xmpp.org for more information.

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

;; [[file:jabber.org::#custom-variables][jabber-enable-legacy-features-p:1]]
  (defvar jabber-enable-legacy-features-p nil)
;; jabber-enable-legacy-features-p:1 ends here

;; [[file:jabber.org::#process-buffer][jabber-process-buffer:1]]
(defvar jabber-process-buffer " *-jabber-process-*"
  "The name of the process buffer.")
;; jabber-process-buffer:1 ends here

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

;;; these customize fields should come first
(defgroup jabber nil "Jabber instant messaging"
  :group 'applications)

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

(defcustom jabber-default-show ""
  "Default show state."
  :type '(choice (const :tag "Online" "")
		 (const :tag "Chatty" "chat")
		 (const :tag "Away" "away")
		 (const :tag "Extended away" "xa")
		 (const :tag "Do not disturb" "dnd"))
  :group 'jabber)

(defcustom jabber-default-status ""
  "Default status string."
  :type 'string
  :group 'jabber)

(defcustom jabber-default-priority 10
  "Default priority."
  :type 'integer
  :group 'jabber)

;;;###autoload
(defvar *jabber-current-status* nil
  "The users current presence status.")

;;;###autoload
(defvar *jabber-current-show* nil
  "The users current presence show.")

;;;###autoload
(defvar *jabber-current-priority* nil
  "The user's current priority.")

(defvar *jabber-status-history* nil
  "History of status messages.")

(defgroup jabber-faces nil "Faces for displaying jabber instant messaging."
  :group 'jabber)

(defface jabber-title-small
  '((t (:weight bold :width semi-expanded :height 1.0 :inherit variable-pitch)))
  "Face for small titles."
  :group 'jabber-faces)

(defface jabber-title-medium
  '((t (:weight bold :width expanded :height 2.0 :inherit variable-pitch)))
  "Face for medium titles."
  :group 'jabber-faces)

(defface jabber-title-large
  '((t (:weight bold :width ultra-expanded :height 3.0 :inherit variable-pitch)))
  "Face for large titles."
  :group 'jabber-faces)

(defgroup jabber-debug nil "debugging options"
  :group 'jabber)

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

(defconst jabber-presence-strings
  `(("" . ,(jabber-propertize "Online" 'face 'jabber-roster-user-online))
    ("away" . ,(jabber-propertize "Away" 'face 'jabber-roster-user-away))
    ("xa" . ,(jabber-propertize "Extended Away" 'face 'jabber-roster-user-xa))
    ("dnd" . ,(jabber-propertize "Do not Disturb" 'face 'jabber-roster-user-dnd))
    ("chat" . ,(jabber-propertize "Chatty" 'face 'jabber-roster-user-chatty))
    ("error" . ,(jabber-propertize "Error" 'face 'jabber-roster-user-error))
    (nil . ,(jabber-propertize "Offline" 'face 'jabber-roster-user-offline)))
  "Mapping from presence types to readable, colorized strings.")

;;;###autoload
(defun jabber-customize ()
  "Customize jabber options."
  (interactive)
  (customize-group 'jabber))

;;;###autoload
(defun jabber-info ()
  "Open jabber.el manual."
  (interactive)
  (info "jabber"))

(provide 'jabber)

;;; jabber.el ends here
