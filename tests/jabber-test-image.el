;;; jabber-test-image.el --- Tests for jabber-image  -*- lexical-binding: t; -*-

;;; Commentary:

;; Image size cap and detected-type policy checks.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'jabber-image)

(defconst jabber-test-image--png-bytes
  (unibyte-string #x89 ?P ?N ?G #x0d #x0a #x1a #x0a
                  0 0 0 13 ?I ?H ?D ?R)
  "Enough PNG magic bytes for `image-type-from-data'.")

(defconst jabber-test-image--gif-bytes
  (concat "GIF89a" (unibyte-string 1 0 1 0 0 0 0))
  "Enough GIF magic bytes for `image-type-from-data'.")

;;; Size cap

(ert-deftest jabber-test-image-size-ok-p-nil-limit-allows-any ()
  (let ((jabber-image-max-bytes nil))
    (should (jabber-image--size-ok-p (make-string 100000 ?x)))))

(ert-deftest jabber-test-image-size-ok-p-enforces-limit ()
  (let ((jabber-image-max-bytes 10))
    (should (jabber-image--size-ok-p "123456789"))
    (should (jabber-image--size-ok-p "1234567890"))
    (should-not (jabber-image--size-ok-p "12345678901"))))

;;; Type allowlist

(ert-deftest jabber-test-image-type-ok-p-detects-png ()
  (should (jabber-image--type-ok-p jabber-test-image--png-bytes '(png)))
  (should-not (jabber-image--type-ok-p jabber-test-image--png-bytes '(jpeg))))

(ert-deftest jabber-test-image-type-ok-p-detects-gif ()
  (should (jabber-image--type-ok-p jabber-test-image--gif-bytes '(gif png)))
  (should-not (jabber-image--type-ok-p jabber-test-image--gif-bytes '(png))))

(ert-deftest jabber-test-image-type-ok-p-nil-allows-any ()
  (should (jabber-image--type-ok-p jabber-test-image--png-bytes nil))
  (should (jabber-image--type-ok-p "not an image at all" nil)))

;;; jabber-image-from-data

(defmacro jabber-test-image--with-fake-create (&rest body)
  "Run BODY with `jabber-image-create' stubbed to a recorder.
Binds `calls' to the list of DATA arguments received."
  `(let ((calls nil))
     (cl-letf (((symbol-function 'jabber-image-create)
                (lambda (data &rest _)
                  (push data calls)
                  (list 'image :type 'png))))
       ,@body)))

(ert-deftest jabber-test-image-from-data-respects-size-cap ()
  (jabber-test-image--with-fake-create
   (let ((jabber-image-max-bytes 4))
     (should-not (jabber-image-from-data jabber-test-image--png-bytes))
     (should (null calls)))))

(ert-deftest jabber-test-image-from-data-respects-allowlist ()
  (jabber-test-image--with-fake-create
   (let ((jabber-image-max-bytes nil))
     (should-not (jabber-image-from-data jabber-test-image--png-bytes '(jpeg)))
     (should (null calls)))))

(ert-deftest jabber-test-image-from-data-nil-allowlist-decodes ()
  (jabber-test-image--with-fake-create
   (let ((jabber-image-max-bytes nil))
     (should (equal (jabber-image-from-data jabber-test-image--png-bytes)
                    '(image :type png)))
     (should (equal calls (list jabber-test-image--png-bytes))))))

(ert-deftest jabber-test-image-from-data-nil-data-returns-nil ()
  (jabber-test-image--with-fake-create
   (should-not (jabber-image-from-data nil))
   (should (null calls))))

(ert-deftest jabber-test-image-from-data-decode-error-returns-nil ()
  (cl-letf (((symbol-function 'jabber-image-create)
             (lambda (&rest _) (error "boom"))))
    (let ((jabber-image-max-bytes nil))
      (should-not (jabber-image-from-data jabber-test-image--png-bytes)))))

;;; Response body extraction

(ert-deftest jabber-test-image-response-body-extracts-bytes ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: image/png\r\n\r\nBODY")
    (should (equal (jabber-image--response-body) "BODY"))))

(ert-deftest jabber-test-image-response-body-nil-without-separator ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK")
    (should-not (jabber-image--response-body))))

(provide 'jabber-test-image)

;;; jabber-test-image.el ends here
