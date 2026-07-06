;;; jabber-test-message-reply.el --- Tests for jabber-message-reply  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0461 Message Replies with XEP-0428 Fallback.

;;; Code:

(require 'ert)
(require 'ewoc)

(require 'jabber-xml)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-muc)
(require 'jabber-message-reply)

;;; Group 1: jabber-message-reply--build-fallback-text

(ert-deftest jabber-test-message-reply-fallback-single-line ()
  "Single-line body produces two-line fallback."
  (let ((result (jabber-message-reply--build-fallback-text "Alice" "Hello")))
    (should (equal "> Alice:\n> Hello\n" result))))

(ert-deftest jabber-test-message-reply-fallback-multi-line ()
  "Multi-line body quotes each line."
  (let ((result (jabber-message-reply--build-fallback-text "Bob" "Line 1\nLine 2\nLine 3")))
    (should (equal "> Bob:\n> Line 1\n> Line 2\n> Line 3\n" result))))

(ert-deftest jabber-test-message-reply-fallback-empty-body ()
  "Empty body produces author line plus empty quote."
  (let ((result (jabber-message-reply--build-fallback-text "Carol" "")))
    (should (equal "> Carol:\n\n" result))))

(ert-deftest jabber-test-message-reply-fallback-nil-body ()
  "Nil body produces author line plus newline."
  (let ((result (jabber-message-reply--build-fallback-text "Dave" nil)))
    (should (equal "> Dave:\n\n" result))))

;;; Group 2: jabber-message-reply--select-id

(ert-deftest jabber-test-message-reply-select-id-1to1 ()
  "In 1:1 chat, select :id."
  (let ((msg (list :id "client-id-1" :server-id "server-id-1")))
    (should (equal "client-id-1"
                   (jabber-message-reply--select-id msg nil)))))

(ert-deftest jabber-test-message-reply-select-id-muc-server ()
  "In MUC, prefer :server-id."
  (let ((msg (list :id "client-id-2" :server-id "server-id-2")))
    (should (equal "server-id-2"
                   (jabber-message-reply--select-id msg t)))))

(ert-deftest jabber-test-message-reply-select-id-muc-requires-server-id ()
  "In MUC without :server-id, do not use the stanza id."
  (let ((msg (list :id "client-id-3" :server-id nil)))
    (should-not (jabber-message-reply--select-id msg t))))

(ert-deftest jabber-test-message-reply-select-id-missing ()
  "Missing both IDs returns nil."
  (let ((msg (list :id nil :server-id nil)))
    (should-not (jabber-message-reply--select-id msg nil))))

;;; Group 3: jabber-message-reply--send-hook

(ert-deftest jabber-test-message-reply-send-hook-produces-elements ()
  "Send hook produces reply and fallback elements and clears state."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id-1")
    (setq-local jabber-message-reply--jid "alice@example.com")
    (setq-local jabber-message-reply--fallback-length 20)
    (let* ((body "> Alice:\n> Hello\nReply text here")
           (elements (jabber-message-reply--send-hook body "new-id")))
      ;; Should produce elements
      (should elements)
      ;; Should contain a reply element
      (should (cl-some (lambda (el) (eq (car el) 'reply)) elements))
      ;; Should contain a fallback element
      (should (cl-some (lambda (el) (eq (car el) 'fallback)) elements))
      ;; State should be cleared
      (should-not jabber-message-reply--id)
      (should-not jabber-message-reply--jid)
      (should-not jabber-message-reply--fallback-length))))

(ert-deftest jabber-test-message-reply-send-hook-nil-when-no-reply ()
  "Send hook returns nil when no reply state is set."
  (with-temp-buffer
    (should-not (jabber-message-reply--send-hook "Hello" "msg-id"))))

(ert-deftest jabber-test-message-reply-send-hook-reply-attributes ()
  "Reply element has correct to and id attributes."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "bob@example.com/phone")
    (setq-local jabber-message-reply--fallback-length 15)
    (let* ((elements (jabber-message-reply--send-hook
                      "> Bob:\n> Hey\nYes!" "new-msg"))
           (reply-el (cl-find 'reply elements :key #'car)))
      (should reply-el)
      (let ((attrs (cadr reply-el)))
        (should (equal "urn:xmpp:reply:0" (cdr (assq 'xmlns attrs))))
        (should (equal "bob@example.com/phone" (cdr (assq 'to attrs))))
        (should (equal "target-msg" (cdr (assq 'id attrs))))))))

(ert-deftest jabber-test-message-reply-send-hook-no-fallback-when-zero-length ()
  "No fallback element when fallback-length is 0."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "target-msg")
    (setq-local jabber-message-reply--jid "carol@example.com")
    (setq-local jabber-message-reply--fallback-length 0)
    (let ((elements (jabber-message-reply--send-hook "Just a reply" "new-msg")))
      (should elements)
      (should (cl-some (lambda (el) (eq (car el) 'reply)) elements))
      (should-not (cl-some (lambda (el) (eq (car el) 'fallback)) elements)))))

(ert-deftest jabber-test-message-reply-send-hook-omits-empty-to ()
  "Replying to our own message (no author JID) omits the `to' attribute.
A `to=\"\"' is an invalid JID and makes strict parsers drop the reply."
  (with-temp-buffer
    (setq-local jabber-message-reply--id "orig-id")
    (setq-local jabber-message-reply--jid "")
    (setq-local jabber-message-reply--fallback-length 0)
    (let* ((elements (jabber-message-reply--send-hook "Just a reply" "new-msg"))
           (reply-el (cl-find 'reply elements :key #'car))
           (attrs (cadr reply-el)))
      (should reply-el)
      (should (equal "orig-id" (cdr (assq 'id attrs))))
      (should-not (assq 'to attrs)))))

(ert-deftest jabber-test-message-reply-outgoing-body-keeps-fallback ()
  "The outgoing reply stanza body retains the quoted fallback text.
The send hook adds <reply>/<fallback> elements but must never strip
the quoted prefix from the body that is actually sent."
  (with-temp-buffer
    (let* ((fallback (jabber-message-reply--build-fallback-text "alice" "Hello"))
           (body (concat fallback "my reply"))
           (jabber-chat-send-hooks (list #'jabber-message-reply--send-hook)))
      (setq-local jabber-message-reply--id "orig-1")
      (setq-local jabber-message-reply--jid "alice@example.com")
      (setq-local jabber-message-reply--fallback-length (length fallback))
      (let ((stanza `(message ((to . "room@conf.example.com")
                               (type . "groupchat")
                               (id . "m1"))
                              (body () ,body))))
        (jabber-chat--run-send-hooks stanza body "m1")
        (let ((sent-body (car (jabber-xml-node-children
                               (car (jabber-xml-get-children stanza 'body))))))
          (should (string-prefix-p "> alice:\n> Hello\n" sent-body))
          (should (string-suffix-p "my reply" sent-body)))
        (should (jabber-xml-child-with-xmlns stanza "urn:xmpp:reply:0"))
        (should (jabber-xml-child-with-xmlns stanza "urn:xmpp:fallback:0"))))))

;;; Group 4: incoming fallback parsing

(ert-deftest jabber-test-message-reply-keeps-incoming-fallback ()
  "Incoming XEP-0461 reply fallback is kept in the displayed body."
  (let* ((stanza '(message ((from . "alice@example.com/tablet")
                            (id . "reply-1")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello\nActual reply")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/tablet")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "0")
                                            (end . "17"))))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "> Alice:\n> Hello\nActual reply" (plist-get msg :body)))
    (should (equal "orig-1" (plist-get msg :reply-to-id)))))

(ert-deftest jabber-test-message-reply-keeps-whole-body-fallback ()
  "Reply fallback with no body range is kept in the displayed body."
  (let* ((stanza '(message ((from . "alice@example.com/tablet")
                            (id . "reply-1")
                            (type . "chat"))
                           (body () "> Alice:\n> Hello")
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/tablet")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0")))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "> Alice:\n> Hello" (plist-get msg :body)))
    (should (equal "orig-1" (plist-get msg :reply-to-id)))))

(ert-deftest jabber-test-message-reply-preserves-malformed-fallback ()
  "Malformed XEP-0461 fallback ranges leave the body unchanged."
  (let* ((body "> Alice:\n> Hello\nActual reply")
         (stanza `(message ((from . "alice@example.com/tablet")
                            (id . "reply-2")
                            (type . "chat"))
                           (body () ,body)
                           (reply ((xmlns . "urn:xmpp:reply:0")
                                   (to . "alice@example.com/tablet")
                                   (id . "orig-1")))
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "x")
                                            (end . "17"))))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal body (plist-get msg :body)))))

(ert-deftest jabber-test-message-reply-preserves-fallback-without-reply ()
  "Fallback text is preserved when no XEP-0461 reply element is present."
  (let* ((body "> Alice:\n> Hello\nActual reply")
         (stanza `(message ((from . "alice@example.com/tablet")
                            (id . "reply-3")
                            (type . "chat"))
                           (body () ,body)
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0"))
                                     (body ((start . "0")
                                            (end . "17"))))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal body (plist-get msg :body)))))

(ert-deftest jabber-test-message-reply-preserves-whole-fallback-without-reply ()
  "Whole-body fallback is preserved when no XEP-0461 reply element is present."
  (let* ((body "> Alice:\n> Hello")
         (stanza `(message ((from . "alice@example.com/tablet")
                            (id . "reply-4")
                            (type . "chat"))
                           (body () ,body)
                           (fallback ((xmlns . "urn:xmpp:fallback:0")
                                      (for . "urn:xmpp:reply:0")))))
         (msg (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal body (plist-get msg :body)))))

;;; Group 5: jabber-chat-reply input placement

(ert-deftest jabber-test-message-reply-inserts-fallback-at-input-start ()
  "`jabber-chat-reply' puts the quote at offset 0 of the input area.
The <fallback> range is start=0, so the quote must land at
`jabber-point-insert' even when a draft is already present, not at
`point-max'."
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (d) (insert (format "%s\n" (plist-get (cadr d) :body)))))))
      (setq-local jabber-chat-ewoc ewoc)
      (let ((node (ewoc-enter-last
                   ewoc (list :foreign (list :id "orig-1"
                                             :from "alice@example.com/phone"
                                             :body "Hello")))))
        (goto-char (point-max))
        (setq-local jabber-point-insert (point-marker))
        (insert "my draft")
        (goto-char (ewoc-location node))
        (jabber-chat-reply)
        (let ((input (buffer-substring-no-properties
                      jabber-point-insert (point-max)))
              (quote (jabber-message-reply--build-fallback-text "alice" "Hello")))
          (should (string-prefix-p quote input))
          (should (string-suffix-p "my draft" input))
          (should (= (length quote) jabber-message-reply--fallback-length))
          (should (equal "orig-1" jabber-message-reply--id)))))))

(provide 'jabber-test-message-reply)

;;; jabber-test-message-reply.el ends here
