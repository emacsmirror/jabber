;;; jabber-awesome.el --- emacs-jabber interface to awesome and naughty  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 - Evgenii Terechkov - evg@altlinux.org

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

(eval-when-compile (require 'jabber-alert))

(defcustom jabber-awesome-args ", timeout=5"
  "Additional args to naughty."
  :type 'string
  :group 'jabber-alerts)

(defun jabber-awesome-message (text &optional title)
  "Show MSG in Awesome"
  ;; Possible errors include not finding the awesome binary.
  (condition-case nil
      (let ((process-connection-type))
        (shell-command-to-string (format "echo 'naughty.notify({text = \"%s\" %s})' | awesome-client -"
					 (or title text) jabber-awesome-args)))
    (error nil)))

(define-jabber-alert awesome "Show a message through the Awesome window manager"
  'jabber-awesome-message)
(define-personal-jabber-alert jabber-muc-awesome)

(provide 'jabber-awesome)

;;; jabber-awesome.el ends here