;;; jabber-uri.el --- XMPP URI parsing and dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Parse XMPP URIs separately from dispatching their interactive commands.

;;; Code:

(require 'url-parse)
(require 'jabber-ahc)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-register)
(require 'jabber-util)

(defun jabber-uri--parse-args (text)
  "Return decoded key-value pairs from URI argument TEXT."
  (when text
    (mapcar (lambda (pair)
              (pcase-let ((`(,key ,value) (split-string pair "=")))
                (cons key (jabber-unhex value))))
            (split-string text ";"))))

(defun jabber-uri-parse (uri)
  "Return the JID, method, and arguments parsed from XMPP URI."
  (when (string-match "//" uri)
    (error "URIs with authority part are not supported"))
  (unless (string-match
           "\\`xmpp:\\([^?]+\\)\\(?:\\?\\([a-z]+\\)\\(?:;\\(.*\\)\\)?\\)?\\'"
           uri)
    (error "Invalid XMPP URI '%s'" uri))
  (list :jid (jabber-unhex (match-string 1 uri))
        :method (match-string 2 uri)
        :args (jabber-uri--parse-args (match-string 3 uri))))

(defun jabber-uri--dispatch (uri-data)
  "Run the command described by parsed URI-DATA."
  (let ((account (jabber-read-account))
        (jid (plist-get uri-data :jid))
        (method (plist-get uri-data :method))
        (args (plist-get uri-data :args)))
    (cond
     ((string= method "join")
      (jabber-muc-join
       account jid (jabber-muc-read-my-nickname account jid) t))
     ((string= method "register")
      (jabber-get-register account jid))
     ((string= method "command")
      (jabber-ahc-execute-command account jid (cdr (assoc "node" args))))
     (t
      (jabber-chat-with account jid)))))

(defun jabber-handle-uri (uri &rest _ignored-args)
  "Handle XMPP URI according to draft-saintandre-xmpp-iri-04.
See Info node `(jabber)XMPP URIs'.
IGNORED-ARGS are ignored arguments the handler may pass."
  (interactive "sEnter XMPP URI: ")
  (let ((uri-data (jabber-uri-parse uri)))
    (raise-frame)
    (jabber-uri--dispatch uri-data)))

(defun jabber-url-xmpp (url)
  "Handle XMPP URLs from internal Emacs functions."
  (jabber-handle-uri (url-recreate-url url)))

(fset 'url-xmpp #'jabber-url-xmpp)

(provide 'jabber-uri)

;;; jabber-uri.el ends here
