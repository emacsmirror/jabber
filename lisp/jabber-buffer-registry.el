;;; jabber-buffer-registry.el --- Chat buffer lookup registry  -*- lexical-binding: t; -*-

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

;; Provides lookup by chat kind without depending on chat rendering modules.

;;; Code:

(defvar jabber-buffer-registry--buffers (make-hash-table :test #'equal)
  "Hash table mapping chat kinds and peer keys to live buffers.")

(defvar-local jabber-buffer-registry--keys nil
  "Registry keys owned by the current buffer.")

(defun jabber-buffer-registry--key (kind peer)
  "Return the registry key for chat KIND and PEER."
  (cons kind peer))

(defun jabber-buffer-registry--remove-current ()
  "Remove registry entries owned by the current buffer."
  (dolist (key jabber-buffer-registry--keys)
    (when (eq (gethash key jabber-buffer-registry--buffers)
              (current-buffer))
      (remhash key jabber-buffer-registry--buffers)))
  (setq jabber-buffer-registry--keys nil))

(defun jabber-buffer-registry-register (kind peer &optional buffer)
  "Register BUFFER under chat KIND and PEER.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((key (jabber-buffer-registry--key kind peer)))
      (puthash key (current-buffer) jabber-buffer-registry--buffers)
      (unless (member key jabber-buffer-registry--keys)
        (push key jabber-buffer-registry--keys))
      (add-hook 'kill-buffer-hook
                #'jabber-buffer-registry--remove-current nil t)
      (current-buffer))))

(defun jabber-buffer-registry-find (kind peer)
  "Return the live buffer registered under chat KIND and PEER."
  (let* ((key (jabber-buffer-registry--key kind peer))
         (buffer (gethash key jabber-buffer-registry--buffers)))
    (if (buffer-live-p buffer)
        buffer
      (remhash key jabber-buffer-registry--buffers)
      nil)))

(provide 'jabber-buffer-registry)
;;; jabber-buffer-registry.el ends here
