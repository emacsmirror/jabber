;; Tests for jabber-xml-skip-tag-forward  -*- lexical-binding: t; -*-

(require 'jabber-xml)
(require 'cl-lib)

(cl-flet ((parses-p
	    (text)
	    (with-temp-buffer
	      (insert text)
	      (goto-char (point-min))
	      (catch 'unfinished
	        (jabber-xml-skip-tag-forward)
	        (= (point) (point-max))))))
  
  ;; 1. Just plain XML
  (unless (parses-p "<stream:features><starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>ANONYMOUS</mechanism><mechanism>DIGEST-MD5</mechanism><mechanism>PLAIN</mechanism></mechanisms><register xmlns='http://jabber.org/features/iq-register'/></stream:features>")
    (error "Testcase 1 failed"))

  ;; 2. XML with CDATA
  (unless (parses-p "<message><body><![CDATA[<foo & bar>]]></body></message>")
    (error "Testcase 2 failed")))
