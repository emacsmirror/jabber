;;; jabber-test-roster.el --- Tests for jabber-roster  -*- lexical-binding: t; -*-

;;; Commentary:

;; Roster display.

;;; Code:

(require 'ert)

;; Pre-define variables that jabber-muc.el expects at load time:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-roster)

;;; Group 1: jabber-fix-status

(ert-deftest jabber-test-roster-fix-status-trailing-newlines ()
  "Trailing newlines are removed."
  (let ((jabber-remove-newlines nil))
    (should (string= (jabber-fix-status "Hello\n\n") "Hello"))))

(ert-deftest jabber-test-roster-fix-status-internal-newlines-removed ()
  "Internal newlines removed when jabber-remove-newlines is t."
  (let ((jabber-remove-newlines t))
    (should (string= (jabber-fix-status "line1\nline2") "line1 line2"))))

(ert-deftest jabber-test-roster-fix-status-internal-newlines-kept ()
  "Internal newlines kept when jabber-remove-newlines is nil."
  (let ((jabber-remove-newlines nil))
    (should (string= (jabber-fix-status "line1\nline2") "line1\nline2"))))

(ert-deftest jabber-test-roster-fix-status-nil ()
  "Nil input returns nil."
  (should (null (jabber-fix-status nil))))

;;; Group 2: Face definitions

(ert-deftest jabber-test-roster-faces-use-inherit ()
  "Modernized roster faces use :inherit."
  (dolist (face-spec '((jabber-roster-user-online . success)
                       (jabber-roster-user-away . warning)
                       (jabber-roster-user-xa . shadow)
                       (jabber-roster-user-dnd . error)
                       (jabber-roster-user-error . error)
                       (jabber-roster-user-offline . shadow)
                       (jabber-roster-groupchat . font-lock-type-face)
                       (jabber-roster-groupchat-nick . shadow)
                       (jabber-roster-unread . font-lock-warning-face)))
    (let* ((face (car face-spec))
           (expected-parent (cdr face-spec))
           (spec (face-default-spec face)))
      (should (facep face))
      (when expected-parent
        (let* ((attrs (cdar spec))
               (inherit (plist-get attrs :inherit)))
          (should (eq inherit expected-parent)))))))

(provide 'jabber-test-roster)
;;; jabber-test-roster.el ends here
