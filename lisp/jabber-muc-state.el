;;; jabber-muc-state.el --- Multi-user chat state  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Track joined rooms independently from MUC protocol handling and buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar jabber-muc--rooms (make-hash-table :test #'equal)
  "Map room JIDs to lists of connection and nickname pairs.")

(defvar jabber-muc--generation 0
  "Generation counter incremented after every room-state change.")

(defun jabber-muc-nickname (group &optional jc)
  "Return our nickname in GROUP, optionally restricted to JC."
  (let ((entries (gethash group jabber-muc--rooms)))
    (if jc
        (alist-get jc entries)
      (cdar entries))))

(defun jabber-muc-connection (group)
  "Return the first connection joined to GROUP, or nil."
  (caar (gethash group jabber-muc--rooms)))

(defun jabber-muc-joined-p (group &optional jc)
  "Return non-nil when GROUP is joined, optionally through JC."
  (let ((entries (gethash group jabber-muc--rooms)))
    (if jc
        (and (assq jc entries) t)
      (and entries t))))

(defun jabber-muc-our-nick-p (group nick)
  "Return non-nil when NICK belongs to us in GROUP."
  (cl-some (lambda (entry) (string= nick (cdr entry)))
           (gethash group jabber-muc--rooms)))

(defun jabber-muc-room-entries (group)
  "Return connection and nickname pairs for GROUP."
  (gethash group jabber-muc--rooms))

(defun jabber-muc-active-rooms ()
  "Return the JIDs of all joined rooms."
  (hash-table-keys jabber-muc--rooms))

(defun jabber-muc-join-set (group jc nickname)
  "Record that JC joined GROUP as NICKNAME."
  (let ((entries (gethash group jabber-muc--rooms)))
    (if-let* ((existing (assq jc entries)))
        (setcdr existing nickname)
      (push (cons jc nickname) entries))
    (puthash group entries jabber-muc--rooms))
  (cl-incf jabber-muc--generation))

(defun jabber-muc-leave-remove (group &optional jc)
  "Remove GROUP state, or only the entry associated with JC."
  (if jc
      (let ((entries (assq-delete-all
                      jc (gethash group jabber-muc--rooms))))
        (if entries
            (puthash group entries jabber-muc--rooms)
          (remhash group jabber-muc--rooms)))
    (remhash group jabber-muc--rooms))
  (cl-incf jabber-muc--generation))

(defun jabber-muc-generation ()
  "Return the room-state generation counter."
  jabber-muc--generation)

(provide 'jabber-muc-state)

;;; jabber-muc-state.el ends here
