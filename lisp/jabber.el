;;; jabber.el --- A minimal Jabber client  -*- lexical-binding: t; -*-

;; Author: Magnus Henoch <mange@freemail.hu>
;; Maintainer: wgreenhouse <wgreenhouse@tilde.club>
;; Keywords: comm
;; Homepage: https://codeberg.org/emacs-jabber/emacs-jabber
;; Package-Requires: ((emacs "27.1") (fsm "0.2.0") (srv "0.2"))
;; Version: 0.9.0

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - Tom Berger - object@intelectronica.net

;; SSL - Support, mostly inspired by Gnus
;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni

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

;;; Commentary:
;; jabber.el is an XMPP client for Emacs. XMPP (also known as
;; 'Jabber') is the IETF-standard federated instant messaging protocol
;; - see http://xmpp.org for more information.

;;; History:
;;

;;; Code:

(require 'cl-lib)
(require 'goto-addr)

;; These are variables shared with more than one section. For
;; instance, `jabber-process-buffer' is used in jabber-core.el but also in
;; jabber-conn.el.

;; Placing these variable definitions before using them avoid
;; byte-compile warnings. Moreover, it is common practice to define
;; variables before its usage.

(defvar jabber-enable-legacy-features-p nil)

;; This was originally defined in jabber-core.el
(defvar jabber-process-buffer " *-jabber-process-*"
  "The name of the process buffer.")

;; Shared between jabber-core.el and jabber-alert.el

(defvar jabber-xml-data)

(defcustom jabber-debug-keep-process-buffers nil
  "If nil, kill process buffers when the process dies.
Contents of process buffers might be useful for debugging."
  :type 'boolean
  :group 'jabber-debug)

(defcustom jabber-silent-mode nil
  "If non-nil, do not ask for confirmation for some operations.  DANGEROUS!"
  :type 'boolean
  :group 'jabber)

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

;;; guess internal dependencies!
(require 'jabber-util)
(require 'jabber-menu)
(require 'jabber-xml)
(require 'jabber-conn)
(require 'jabber-core)
(require 'jabber-logon)
(require 'jabber-roster)
(require 'jabber-presence)
(require 'jabber-alert)
(require 'jabber-chat)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-widget)
(require 'jabber-register)
(require 'jabber-search)
(require 'jabber-browse)
(require 'jabber-muc)
(require 'jabber-muc-nick-completion)
(require 'jabber-version)
(require 'jabber-ahc-presence)
(require 'jabber-modeline)
(require 'jabber-watch)
(require 'jabber-activity)
(require 'jabber-vcard)
(require 'jabber-events)
(require 'jabber-chatstates)
(require 'jabber-vcard-avatars)
(require 'jabber-autoaway)
(require 'jabber-time)
(require 'jabber-truncate)

;;;###autoload
(defvar *jabber-current-status* nil
  "The user's current presence status.")

;;;###autoload
(defvar *jabber-current-show* nil
  "The user's current presence show.")

;;;###autoload
(defvar *jabber-current-priority* nil
  "The user's current priority.")

(defvar *jabber-status-history* nil
  "History of status messages.")

(defgroup jabber-faces nil "Faces for displaying Jabber instant messaging."
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
  "Customize Jabber options."
  (interactive)
  (customize-group 'jabber))

;;;###autoload
(defun jabber-info ()
  "Open jabber.el manual."
  (interactive)
  (info "jabber"))

(provide 'jabber)

;;; jabber.el ends here.
