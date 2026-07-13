;;; jabber-roster.el --- roster management    -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2009 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
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
;;

;;; Code:

(require 'cl-lib)
(require 'jabber-util)
(require 'jabber-private)

(defgroup jabber-roster nil "Roster options."
  :group 'jabber)

(defcustom jabber-remove-newlines t
  "Remove newlines in status messages?
Newlines in status messages mess up the roster display.  However,
they are essential to status message poets.  Therefore, you get to
choose the behaviour.

Trailing newlines are always removed, regardless of this variable."
  :type 'boolean)

(defcustom jabber-roster-default-group-name "Ungrouped"
  "Default group name for buddies without groups."
  :type 'string
  :get (lambda (var)
	 (let ((val (symbol-value var)))
	   (when (stringp val)
	     (set-text-properties 0 (length val) nil val))
	   val))
  :set (lambda (var val)
         (when (stringp val)
	   (set-text-properties 0 (length val) nil val))
         (custom-set-default var val)))

;;; Faces

(defface jabber-roster-user-online
  '((t :inherit success :weight bold))
  "Face for displaying online users.")

(defface jabber-roster-user-xa
  '((t :inherit shadow :slant italic))
  "Face for displaying extended away users.")

(defface jabber-roster-user-dnd
  '((t :inherit error :weight bold))
  "Face for displaying do not disturb users.")

(defface jabber-roster-user-away
  '((t :inherit warning :slant italic))
  "Face for displaying away users.")

(defface jabber-roster-user-chatty
  '((t :inherit success :weight bold :slant italic))
  "Face for displaying chatty users.")

(defface jabber-roster-user-error
  '((t :inherit error :slant italic))
  "Face for displaying users sending presence errors.")

(defface jabber-roster-user-offline
  '((t :inherit shadow :slant italic))
  "Face for displaying offline users.")

(defface jabber-roster-groupchat
  '((t :inherit font-lock-type-face))
  "Face for groupchat room names in the roster buffer.")

(defface jabber-roster-groupchat-nick
  '((t :inherit shadow))
  "Face for the user's nickname in groupchat roster entries.")

(defface jabber-roster-unread
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for roster entries with unread messages.")

(defvar jabber-roster-debug nil
  "Debug roster operations.")


;;; Roster data management

(defun jabber-roster-prepare-roster (jc)
  "Make a hash based roster.
JC is the Jabber connection."
  (let* ((state-data (fsm-get-state-data jc))
	 (hash (make-hash-table :test 'equal))
	 (buddies (plist-get state-data :roster))
	 (all-groups '()))
    (dolist (buddy buddies)
      (let ((groups (or (get buddy 'groups)
			(list jabber-roster-default-group-name))))
	(dolist (group groups)
	  (push group all-groups)
	  (puthash group
		   (cons buddy (gethash group hash))
		   hash))))
    (maphash (lambda (key val) (puthash key (nreverse val) hash)) hash)
    (setq all-groups (sort
		      (cl-remove-duplicates all-groups
					    :test #'string=)
		      #'string<))
    (plist-put state-data :roster-groups
	       (mapcar #'list all-groups))
    (plist-put state-data :roster-hash
	       hash)))

(defun jabber-fix-status (status)
  "Make STATUS strings more readable."
  (when status
    (when (string-match "\n+$" status)
      (setq status (replace-match "" t t status)))
    (when jabber-remove-newlines
      (while (string-match "\n" status)
	(setq status (replace-match " " t t status))))
    status))

;;;###autoload
(defun jabber-roster-update (jc new-items changed-items deleted-items)
  "Update roster in memory.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.
JC is the Jabber connection."
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
	 (hash (plist-get (fsm-get-state-data jc) :roster-hash))
	 (all-groups (plist-get (fsm-get-state-data jc) :roster-groups)))

    (dolist (delete-this deleted-items)
      (setq roster (delq delete-this roster)))
    (setq roster (append new-items roster))
    (plist-put (fsm-get-state-data jc) :roster roster)

    (if (not hash)
	(jabber-roster-prepare-roster jc)

      (when jabber-roster-debug
	(message "update hash-based roster"))

      (dolist (delete-this (append deleted-items changed-items))
	(when jabber-roster-debug
	  (message "delete jid: %s" (symbol-name delete-this)))
	(dolist (group (mapcar #'car all-groups))
	  (puthash group
		   (delq delete-this (gethash group hash))
		   hash)))

      (dolist (insert-this (append changed-items new-items))
	(when jabber-roster-debug
	  (message "insert jid: %s" (symbol-name insert-this)))
	(dolist (group (or (get insert-this 'groups)
			   (list jabber-roster-default-group-name)))
	  (puthash group
		   (cons insert-this (gethash group hash))
		   hash)
	  (push (list group) all-groups)))

      (setq all-groups (sort
			(cl-remove-duplicates all-groups
					      :key #'car :test #'string=)
			(lambda (a b) (string< (car a) (car b)))))

      (plist-put (fsm-get-state-data jc) :roster-groups all-groups))))

;;; Private storage (group rolling state)

(defun jabber-roster-restore-groups (jc)
  "Restore roster's groups rolling state from private storage.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-private-get jc 'roster "emacs-jabber"
                      'jabber-roster-restore-groups-1 'ignore))

(defun jabber-roster-restore-groups-1 (jc xml-data)
  "Parse roster groups and restore rolling state.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (when (string= (jabber-xml-get-xmlns xml-data) "emacs-jabber")
    (let* ((data (car (last xml-data)))
           (groups (if (stringp data) (split-string data "\n") nil)))
      (dolist (group groups)
        (let* ((state-data (fsm-get-state-data jc))
               (roll-groups (plist-get state-data :roster-roll-groups)))
          (unless (cl-find group roll-groups :test #'string=)
            (plist-put state-data :roster-roll-groups
                       (cons group roll-groups))))))))

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

(provide 'jabber-roster)

;;; jabber-roster.el ends here
