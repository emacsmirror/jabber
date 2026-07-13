;;; jabber-xdata.el --- XMPP data form helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Parse XMPP data forms without depending on widget rendering.

;;; Code:

(require 'jabber-xml)

(defun jabber-xdata-form-type (x)
  "Return the form type of the XData form X.
Return nil when X has no XEP-0068 FORM_TYPE field."
  (catch 'form-type
    (dolist (field (jabber-xml-get-children x 'field))
      (when (and (string= (jabber-xml-get-attribute field 'var) "FORM_TYPE")
                 (string= (jabber-xml-get-attribute field 'type) "hidden"))
        (throw 'form-type
               (car (jabber-xml-node-children
                     (car (jabber-xml-get-children field 'value)))))))))

(define-obsolete-function-alias 'jabber-widget-xdata-formtype
  #'jabber-xdata-form-type "0.11.0")

(provide 'jabber-xdata)

;;; jabber-xdata.el ends here
