;;; jabber-muc-protocol.el --- Multi-user chat stanza classification  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Classify MUC stanzas without loading room buffers or interactive commands.

;;; Code:

(require 'cl-lib)
(require 'jabber-muc-state)
(require 'jabber-util)
(require 'jabber-xml)

(defconst jabber-muc-xmlns "http://jabber.org/protocol/muc"
  "XEP-0045 MUC namespace.")

(defconst jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user"
  "XEP-0045 MUC user namespace.")

(defconst jabber-muc-xmlns-owner "http://jabber.org/protocol/muc#owner"
  "XEP-0045 MUC owner namespace.")

(defconst jabber-muc-xmlns-admin "http://jabber.org/protocol/muc#admin"
  "XEP-0045 MUC admin namespace.")

(defconst jabber-muc-xmlns-direct-invite "jabber:x:conference"
  "XEP-0249 Direct MUC Invitations namespace.")

(defvar jabber-pending-groupchats (make-hash-table)
  "Hash table of groupchats and nicknames.
Keys are JID symbols; values are strings.
This table records the last nickname used to join the particular
chat room.  Items are thus never removed.")

;;;###autoload
(defun jabber-muc-message-p (message)
  "Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites."
  (let ((from (jabber-xml-get-attribute message 'from))
        (type (jabber-xml-get-attribute message 'type)))
    (or (string= type "groupchat")
        (and (string= type "error")
             (gethash (jabber-jid-symbol from) jabber-pending-groupchats))
        (jabber-xml-path message `((,jabber-muc-xmlns-user . "x") invite))
        (jabber-xml-path
         message `((,jabber-muc-xmlns-direct-invite . "x"))))))

;;;###autoload
(defun jabber-muc-sender-p (jid)
  "Return non-nil if JID is a full JID of an MUC participant."
  (and (jabber-muc-joined-p (jabber-jid-user jid))
       (jabber-jid-resource jid)))

;;;###autoload
(defun jabber-muc-private-message-p (message)
  "Return non-nil if MESSAGE is a private message in a groupchat."
  (let ((from (jabber-xml-get-attribute message 'from))
        (type (jabber-xml-get-attribute message 'type)))
    (and (not (string= type "groupchat"))
         (jabber-muc-sender-p from))))

(defun jabber-muc-presence-p (presence)
  "Return non-nil if PRESENCE is presence from groupchat."
  (let ((from (jabber-xml-get-attribute presence 'from))
        (type (jabber-xml-get-attribute presence 'type))
        (muc-marker
         (cl-find-if
          (lambda (x)
            (equal (jabber-xml-get-attribute x 'xmlns)
                   jabber-muc-xmlns-user))
          (jabber-xml-get-children presence 'x))))
    (or muc-marker
        (and (string= type "error")
             (gethash (jabber-jid-symbol from)
                      jabber-pending-groupchats)))))

(provide 'jabber-muc-protocol)

;;; jabber-muc-protocol.el ends here
