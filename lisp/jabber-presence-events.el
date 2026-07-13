;;; jabber-presence-events.el --- Presence effect dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

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

;; Keeps presence parsing independent from roster, chat, and MUC effects.

;;; Code:

(defvar jabber-presence-roster-update-functions nil
  "Functions called with a parsed roster update.
Arguments are connection, new items, changed items, and deleted items.")

(defvar jabber-presence-muc-functions nil
  "Functions called with a connection and an incoming MUC presence stanza.")

(defvar jabber-presence-contact-functions nil
  "Functions called with a connection and ordinary presence sender.")

(defvar jabber-presence-subscription-request-functions nil
  "Functions called with a connection, sender, and subscription status.")

(defun jabber-presence-events-dispatch-roster-update
    (jc new-items changed-items deleted-items)
  "Dispatch a roster update for JC.
NEW-ITEMS, CHANGED-ITEMS, and DELETED-ITEMS are JID symbol lists."
  (run-hook-with-args 'jabber-presence-roster-update-functions
                      jc new-items changed-items deleted-items))

(defun jabber-presence-events-dispatch-muc (jc presence)
  "Dispatch an incoming MUC PRESENCE stanza on JC."
  (run-hook-with-args 'jabber-presence-muc-functions jc presence))

(defun jabber-presence-events-dispatch-contact (jc from)
  "Dispatch ordinary presence from FROM on JC."
  (run-hook-with-args 'jabber-presence-contact-functions jc from))

(defun jabber-presence-events-dispatch-subscription-request
    (jc from presence-status)
  "Dispatch a subscription request from FROM on JC with PRESENCE-STATUS."
  (run-hook-with-args 'jabber-presence-subscription-request-functions
                      jc from presence-status))

(provide 'jabber-presence-events)
;;; jabber-presence-events.el ends here
