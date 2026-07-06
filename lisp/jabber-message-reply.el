;;; jabber-message-reply.el --- XEP-0461 Message Replies  -*- lexical-binding: t; -*-

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

;; XEP-0461 Message Replies with XEP-0428 Fallback Indication.
;; Adds reply-to-message support in chat buffers.  The user positions
;; point on a message, invokes `jabber-chat-reply', a quoted fallback
;; is inserted into the composition area, and on send the <reply> and
;; <fallback> elements are added to the stanza.  The fallback text is
;; kept in the displayed body as-is.

;;; Code:

(require 'ewoc)
(require 'jabber-util)
(require 'jabber-disco)
(require 'jabber-xml)


(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chat-send-hooks)         ; jabber-chat.el
(defvar jabber-point-insert)            ; jabber-chatbuffer.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-chatting-with)           ; jabber-chatbuffer.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

(declare-function jabber-muc-sender-p "jabber-muc" (jid))

(defconst jabber-message-reply-xmlns "urn:xmpp:reply:0"
  "XEP-0461 Message Replies namespace.")

(defconst jabber-message-reply-fallback-xmlns "urn:xmpp:fallback:0"
  "XEP-0428 Fallback Indication namespace.")

;;; Buffer-local reply state

(defvar-local jabber-message-reply--id nil
  "Stanza ID of the message being replied to.")

(defvar-local jabber-message-reply--jid nil
  "JID of the original message author.")

(defvar-local jabber-message-reply--fallback-text nil
  "Fallback quote string inserted into the composition area.")

;;; Pure functions

(defun jabber-message-reply--build-fallback-text (author body)
  "Build a fallback quote string from AUTHOR and BODY.
Returns \"> Author:\\n> line1\\n> line2\\n\"."
  (let ((lines (if (or (null body) (string-empty-p body))
                   nil
                 (split-string body "\n"))))
    (concat "> " author ":\n"
            (mapconcat (lambda (line) (concat "> " line))
                       lines
                       "\n")
            "\n")))

(defun jabber-message-reply--strip-fallback (body range)
  "Return BODY with the reply fallback RANGE removed.
RANGE is nil, `all', or a (START END) list of code point offsets as
stored in :fallback-range.  Invalid ranges leave BODY unchanged.
Quoting a reply must not re-quote its own quote."
  (pcase range
    ('nil body)
    ('all "")
    (`(,start ,end)
     (if (and (integerp start) (integerp end)
              (<= 0 start end (length body)))
         (concat (substring body 0 start) (substring body end))
       body))
    (_ body)))

(defun jabber-message-reply--select-id (msg muc-p)
  "Select the appropriate message ID from MSG for a reply.
In MUC (when MUC-P is non-nil), use :server-id only.
In 1:1 chat, use :id.  Returns nil if unavailable."
  (if muc-p
      (plist-get msg :server-id)
    (plist-get msg :id)))

;;; Send hook

(defun jabber-message-reply--send-hook (body _id)
  "Add <reply> and <fallback> elements when replying to a message.
BODY is the message text.  Clears reply state after producing elements.
The <fallback> range is emitted only when BODY still starts with the
inserted quote: after an edit the advertised range would cover the
user's own text, which receivers strip from display.
Stays inert during corrections: the pending reply belongs to the
message being composed, not to a re-sent old one."
  (when (and jabber-message-reply--id
             (not (bound-and-true-p jabber-chat--sending-correction)))
    (let ((reply-id jabber-message-reply--id)
          (reply-jid jabber-message-reply--jid)
          (fb-text jabber-message-reply--fallback-text))
      (setq jabber-message-reply--id nil
            jabber-message-reply--jid nil
            jabber-message-reply--fallback-text nil)
      (let ((elements
             (list
              `(reply ((xmlns . ,jabber-message-reply-xmlns)
                       ;; `to' is SHOULD, not MUST (XEP-0461).  Omit it
                       ;; rather than send to="" when replying to our own
                       ;; message (no author JID): strict parsers reject an
                       ;; empty JID and drop the whole reply element.
                       ,@(and reply-jid (not (string-empty-p reply-jid))
                              (list (cons 'to reply-jid)))
                       (id . ,reply-id))))))
        (when (and fb-text (not (string-empty-p fb-text))
                   (string-prefix-p fb-text body))
          (push `(fallback ((xmlns . ,jabber-message-reply-fallback-xmlns)
                            (for . ,jabber-message-reply-xmlns))
                           (body ((start . "0")
                                  (end . ,(number-to-string (length fb-text))))))
                elements))
        elements))))

(add-hook 'jabber-chat-send-hooks #'jabber-message-reply--send-hook)

;;; Helpers

(defun jabber-message-reply--self-jid ()
  "Return our own bare JID for a self-reply, or nil when unknown.
In MUC private chats our address is the room occupant JID, not the
account JID, so there is no useful value and the reply `to'
attribute is omitted (it is a SHOULD, not a MUST)."
  (and (bound-and-true-p jabber-buffer-connection)
       (not (and (bound-and-true-p jabber-chatting-with)
                 (jabber-muc-sender-p jabber-chatting-with)))
       (jabber-connection-bare-jid jabber-buffer-connection)))

(defun jabber-message-reply--author-name (jid)
  "Return a short display name for JID.
In MUC buffers the resource is the nickname.
In 1:1 chat, use the username part of the JID."
  (if (bound-and-true-p jabber-group)
      (or (jabber-jid-resource jid)
          (jabber-jid-displayname jid))
    (or (jabber-jid-username jid)
        (jabber-jid-user jid))))

;;; Interactive commands

;;;###autoload
(defun jabber-chat-reply ()
  "Reply to the message at point in the ewoc.
Stores reply state and inserts fallback quote text at the start of
the input area, so it lines up with the <fallback> range (offset 0)
even when a draft is already present."
  (interactive)
  (unless (bound-and-true-p jabber-chat-ewoc)
    (user-error "Not in a chat buffer"))
  (let* ((ewoc-node (ewoc-locate jabber-chat-ewoc (point)))
         (data (and ewoc-node (ewoc-data ewoc-node)))
         (msg (and data (cadr data)))
         (muc-p (bound-and-true-p jabber-group))
         (id (and msg (jabber-message-reply--select-id msg muc-p))))
    (unless id
      (user-error "No message ID at point"))
    (let* ((from (plist-get msg :from))
           (author (if from
                       (jabber-message-reply--author-name from)
                     "me"))
           (body (jabber-message-reply--strip-fallback
                  (or (plist-get msg :body) "")
                  (plist-get msg :fallback-range)))
           (jid (or from (jabber-message-reply--self-jid) ""))
           (fallback (jabber-message-reply--build-fallback-text author body)))
      (setq jabber-message-reply--id id
            jabber-message-reply--jid (if (stringp jid) jid (format "%s" jid))
            jabber-message-reply--fallback-text fallback)
      (goto-char jabber-point-insert)
      (insert fallback)
      (message "Replying to %s (C-c C-k to cancel)" author))))

;;;###autoload
(defun jabber-chat-cancel-reply ()
  "Cancel the pending reply and remove the inserted quote.
The quote is deleted only while still intact at the start of the
input area; edited input is left alone."
  (interactive)
  (when jabber-message-reply--id
    (let ((fb-text jabber-message-reply--fallback-text))
      (setq jabber-message-reply--id nil
            jabber-message-reply--jid nil
            jabber-message-reply--fallback-text nil)
      (when (and fb-text (not (string-empty-p fb-text)))
        (let ((end (+ jabber-point-insert (length fb-text))))
          (when (and (<= end (point-max))
                     (string= fb-text
                              (buffer-substring-no-properties
                               jabber-point-insert end)))
            (delete-region jabber-point-insert end)))))
    (message "Reply cancelled")))

;;; Disco

(jabber-disco-advertise-feature jabber-message-reply-xmlns)

(provide 'jabber-message-reply)
;;; jabber-message-reply.el ends here
