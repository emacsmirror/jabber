;;; jabber-httpupload.el --- Emacs Jabber HTTP Upload Implementation -*- lexical-binding: t; -*-

;; Copyright 2021 cnngimenez
;;
;; Author: cnngimenez
;; Maintainer: cnngimenez
;; Version: 0.1.0
;; Keywords: comm
;; URL: https://github.com/cnngimenez/emacs-jabber
;; Package-Requires: ((emacs "26.1") (jabber "0.8.92"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file implements the HTTP Upload XEP-0363 extension.  This extension
;; enables the user to send files through XMPP clients by using server space.
;; 
;; Please read the jabber-httpupload.org file for information.

;;; Code:

(require 'seq)
(require 'fsm)
(require 'mailcap)
(require 'jabber)

(defgroup jabber-httpupload nil "Jabber HTTP Upload Settings."
  :group 'jabber)

(defcustom jabber-httpupload-upload-function #'jabber-httpupload-put-file-curl
  "The function used to upload the file.
Some functions calls external programs such as Curl and wget, please check their
documentation for more information."
  :group 'jabber-httpupload
  :type 'function)

(defcustom jabber-httpupload-record-command "sox -d -t ogg $(filename).ogg"
  "What is the command used to record audio?
Use $(filename) where the temporal filename should be."
  :group 'jabber-httpupload
  :type 'function)

(defvar jabber-httpupload-support nil
"An alist of jabber connections and the node with HTTP Upload support.
This is filled by the `jabber-httpupload-test-all-connections-suport'.
Each element are of the form (jabber-connection . string/nil).  If the value is
a string, it is the upload item IRI, if nil means no support.")

(defun jabber-httpupload-test-all-connections-support ()
  "Test all connections in `jabber-connections' for HTTP Upload support.
Store the results at `jabber-httpupload-support'.
If the connection is already tested, ignore it."
  (let ((connections (seq-difference jabber-connections
                                     (mapcar #'car jabber-httpupload-support))))
    (dolist (jc connections)
      (jabber-httpupload-test-connection-support jc))))

(defun jabber-httpupload-test-connection-support (jc)
  "Test if HTTP Upload is supported on the JC connection's server.
If it is supported, store the item IRI at `jabber-httpupload-support'.

This function is asynchronous, thus it won't return any results."
  (jabber-httpupload-apply-to-items jc
                               (lambda (jc result)
                                 (jabber-httpupload-test-item-support jc (elt result 1)))))

(defun jabber-httpupload-test-item-support (jc iri)
  "Test if the IRI Disco item supports HTTP Upload.
Get the Disco Info from the provided IRI at the current JC jabber connection,
if the HTTP Upload namespace feature is in the answer, store the IRI
in `jabber-httpupload-support'."
  (jabber-disco-get-info jc iri nil
                         (lambda (jc _data result)
                           (when (member "urn:xmpp:http:upload"
                                         (nth 1 result))
                             ;; This item supports HTTP Upload... register it!
                             (push (cons jc iri) jabber-httpupload-support)))
                           nil))

(defun jabber-httpupload-apply-to-items (jc callback)
  "Retrieve al Disco IRIs from the server connected in JC.
Return a list of IRI strings.

JC is a jabber connection.
CALLBACK is a function that receives two arguments: Jabber connection and
the item vector."
  (let ((node (plist-get (fsm-get-state-data jc) :server)))
    (jabber-disco-get-items jc node nil
                            (lambda (jc _data result)
                              (dolist (item result)
		      (message "item: %S" item)
                                (funcall callback jc item)))
                            nil)))

(defun jabber-httpupload-server-has-support (jc)
  "Check if the server has HTTP Upload support.
Return the tuple (jabber-connection . upload-url) when there is support from
the server.  Return nil when the server does not support HTTP Upload.

If the server is not in `jabber-httpupload-support', then it is considered as
it is not supported.  It SHOULD be tested on-line with
`jabber-httpupload-test-connection-support' as soon as the connection and
authentication is established.

JC is the Jabber Connection to use."

  (seq-find (lambda (tuple)
              (and (equal jc (car tuple))
                   (cdr tuple)))
            jabber-httpupload-support))

(defun jabber-httpupload-parse-slot-answer (xml-data)
  "Retrieve the slot data from the XML-DATA information.
The XML-DATA is the stanza receive from the Jabber Connection after requesting
the slot for a file.
The returned list has the PUT URL and the GET URL."
  (list
   (jabber-xml-get-attribute (jabber-xml-path xml-data '(slot put)) 'url)
   (jabber-xml-get-attribute (jabber-xml-path xml-data '(slot get)) 'url)))

(defun jabber-httpupload--request-slot-successful (jc xml-data data)
  "Callback function used when the slot request succeeded.
XML-DATA is the received XML from the server.
DATA is a triple (filedata success-callback success-args) where:
  FILEDATA is a triple (filename size content-type)
  SUCCESS-CALLBACK is a function to call after parsing and requesting the
    upload.
  It should accept following arguments: JC XML-DATA FILEDATA PUT-GET-URLS
    and SUCCESS-ARGS.
  SUCCESS-ARGS is a list to pass to the SUCCESS-CALLBACK."
  (let ((urls (jabber-httpupload-parse-slot-answer xml-data))
        (filedata (car data))
        (success-callback (nth 1 data))
        (success-args (nth 2 data)))
    (funcall success-callback jc xml-data filedata urls success-args)))

(defun jabber-httpupload--request-slot-failed (jc xml-data data)
  "Callback function used when the slot request failed.

DATA is a list (filedata error-callback error-args) where:
  FILEDATA is a triple (filename size content-type)
  ERROR-CALLBACK is a function to call.  If no error-callback is provided, then
  `error' is used.  Its arguments are JC XML-DATA FILEDATA ERROR-ARGS.
  ERROR-ARGS is list passed to the ERROR-CALLBACK."
  (let ((filedata (car data))
        (error-callback (nth 1 data))
        (error-args (nth 2 data)))
    (if error-callback
        (funcall error-callback jc xml-data filedata error-args)
      (error (format "The file %s cannot be uploaded: SLOT rejected. %S"
                     (car data) xml-data)))))

(defun jabber-httpupload-request-slot (jc filedata success-callback success-args
                                      &optional error-callback error-args)
  "Request a slot for HTTP Upload to the server's connection.
JC is an active Jabber Connection.
FILEDATA is a list with (filename size content-type).
SUCCESS-CALLBACK is a function name to call when the slot is received.  Its
  arguments should be: jc xml-data data and put-get-URLs.
SUCCESS-ARGS is a list of arguments used by the SUCCESS-CALLBACK
ERROR-CALLBACK is a function to call on failure.  Its arguments should be:
  jc xml-data.
ERROR-ARGS is a list with arguments for ERROR-CALLBACK."
  (let ((filename (file-name-nondirectory (car filedata)))
        (size (nth 1 filedata))
        (content-type (nth 2 filedata)))
    (jabber-send-iq jc (cdr (jabber-httpupload-server-has-support jc)) "get"
                    `(request ((xmlns . "urn:xmpp:http:upload:0")
                               (filename . ,filename)
                               (size . ,size)
                               (content-type . ,content-type)))
                    #'jabber-httpupload--request-slot-successful
                    (list filedata success-callback success-args)
                    #'jabber-httpupload--request-slot-failed
                    (list filedata error-callback error-args))))

(defun jabber-httpupload-ignore-certificate (jc)
  "Should the SSL/TLS certificates be ignore from JC connection?
Check if JC URL is in the variable `jabber-invalid-certificate-servers', if it
is the XMPP and HTTPs connection should be established regarding their
certificate validation status."
  (member (plist-get (fsm-get-state-data jc) :server)
          jabber-invalid-certificate-servers))

(defun jabber-httpupload-upload-file (filepath content-type put-url
                                           callback callback-arg
                                           &optional ignore-cert-problems)
  "Update the given file at FILEPATH to the provided PUT-URL.
The CONTENT-TYPE (MIME type) of the file must match the one provided
to the Jabber Connection with `jabber-httpupload-request-slot'.
IGNORE-CERT-PROBLEMS allows to connect with HTTPS servers with invalid or
non-trusted SSL/TLS certificates.
When the process ends, a callback function is called using the following
code: (funcall CALLBACK CALLBACK-ARG)"
  (unless (funcall jabber-httpupload-upload-function filepath content-type put-url
                   callback callback-arg
                   ignore-cert-problems)
    (error (concat "The upload function failed to PUT the file to the server. "
                   "Try other function or install the required program"))))

(defvar jabber-httpupload-upload-processes nil
  "List of running processes uploading the file to the server.
List of running processes uploading the file to the server associated with
their callback and arguments.
Each element has the following format: (process . (callback arg))")

(defun jabber-httpupload-process-ended (process)
  "What to do when an upload process ends.
PROCESS is the process that ended.
First remove the process from `jabber-httpupload-upload-processes',
then call its callback with the provided argument."
  (let* ((data (assq process jabber-httpupload-upload-processes))
         (callback (cadr data))
         (callback-arg (caddr data)))
    (setq jabber-httpupload-upload-processes
          (assq-delete-all process jabber-httpupload-upload-processes))
    (funcall callback callback-arg)))

(defun jabber-httpupload-curl-sentinel (process event)
  "Detect when Curl ends and act accordingly.
PROCESS is the asynchronous Curl call.
EVENT is a string describing the reason the sentinel were called.

When EVENT is \"finished\n\", then the function
`jabber-httpupload-process-ended' is called."
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "Sentinel: %S event received." event))))
  (when (string= event "finished\n")
    (jabber-httpupload-process-ended process)))

(defun jabber-httpupload-put-file-curl (filepath content-type put-url
                                             callback callback-arg
                                             &optional ignore-cert-problems)
  "Use the curl command to put the file at FILEPATH into the PUT-URL.
Send the SIZE and CONTENT-TYPE MIME as headers.
IGNORE-CERT-PROBLEMS enable the use of HTTPS connections with invalid or
non-trusted SSL/TLS certificates.  If nil, curl will validate the certificate
provided by the HTTP/S Web server.
When the process ends, the function CALLBACK is called like the following
call: (funcall CALLBACK CALLBACK-ARG).
The process is registered at `jabber-httpupload-upload-processes' AList with
the provided CALLBACK and CALLBACK-ARG."
  (let* ((exec-path (executable-find "curl"))
         (cmd (format "%s %s --upload-file '%s' -H \"content-type: %s\" '%s'"
                      exec-path
                      (if ignore-cert-problems                          
                          "--insecure"
                        "")
                      filepath content-type put-url)))
    (when exec-path
      (with-current-buffer (get-buffer-create "*jabber-httpupload-put-file-curl*")
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format  "%s Uploading to %s with curl:\n$ %s"
                           (current-time-string)
                           put-url
                           cmd))
          (let ((process (start-process-shell-command "jabber-httpupload-put-file-curl"
                                                      (current-buffer)
                                                      cmd)))
            (push (cons process (list callback callback-arg))
                  jabber-httpupload-upload-processes)
            (set-process-sentinel process #'jabber-httpupload-curl-sentinel))
          (insert "-- done --")
          t)))))

(defun jabber-httpupload-send-file-url (jc jid get-url)
  "Send the GET URL address to the JID user.
The message requiers the GET-URL of the slot file, the receiver's JID 
and the JC Jabber Connection."
  ;; This could be a possibliity, but... cannot send the x tag.
  ;; (jabber-send-message jc jid nil get-url nil)
  (let ((fromjid (jabber-connection-original-jid jc))
        (type (if (assoc jid *jabber-active-groupchats*)
                  "groupchat"
                "chat")))
    (jabber-send-sexp jc
                      `(message ((to . ,jid)
                                 (from . ,fromjid)
                                 (type . ,type))
                                (body () ,get-url)
                                (x ((xmlns . "jabber:x:oob"))
                                   (url () ,get-url))))))

(defun jabber-httpupload-send-file (jc jid filepath)
  "Send the file at FILEPATH to the user JID.
JC is the Jabber Connection to send the file URL."
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Send file to: " nil nil nil 'full t)
                     (read-file-name "File to send:")))
  (unless (jabber-httpupload-server-has-support jc)
    (error "The Jabber Connection provided has no HTTP Upload support"))
  (let* ((size (file-attribute-size (file-attributes filepath)))
         (content-type (mailcap-extension-to-mime (file-name-extension filepath)))
         (filedata (list filepath size content-type)))
    (jabber-httpupload-request-slot jc filedata
                                    #'jabber-httpupload--slot-reserved
                                    (list jid))))

(defun jabber-httpupload--upload-done (data)
  "Callback function used when the upload is done.
When the upload process finished, a callback function is called with an
argument.
This function is expected to be used as the CALLBACK argument for the function
`jabber-httpupload-upload-file', DATA is its CALLBACK-ARG argument.
Also, see `jabber-httpupload-process-ended' for more information.
DATA is expected to have the following foramt: (jc jid get-url).
After the upload is done, send the get-url to the destined Jabber user JID."
  (let ((jc (car data))
        (jid (nth 1 data))
        (get-url (nth 2 data)))
    (condition-case err
        (jabber-httpupload-send-file-url jc jid get-url)
      (error "Cannot send message.  Error: %S" err))))

(defun jabber-httpupload--slot-reserved (jc _xml-data filedata urls extra-data)
  "Callback function used when the slot request succeeded.
JC is the current Jabber Connection.
XML-DATA is the received XML from the server.
FILEDATA is a triple `(filepath size content-type).
URLS is a tuple `(put-url get-url).
EXTRA-DATA is a list `(jid)"
  (let ((filepath (car filedata))
        (content-type (nth 2 filedata))
        (jid (car extra-data))
        (get-url (cadr urls))
        (put-url (car urls)))
    (message "jabber-httpupload: slot PUT and GET URLs: %S" urls)
    (condition-case err
        (jabber-httpupload-upload-file (expand-file-name filepath)
                                       content-type
                                       put-url
                                       #'jabber-httpupload--upload-done (list jc jid get-url)
                                       (jabber-httpupload-ignore-certificate jc))
      (error "Cannot upload the file.  Error: %S" err))))

(add-hook 'jabber-post-connect-hooks #'jabber-httpupload-test-connection-support)

(provide 'jabber-httpupload)

;;; jabber-httpupload.el ends here
