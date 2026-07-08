;;; jabber-image.el --- image display support  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; Shared image creation and async fetching for avatars and inline
;; previews.  All images use dynamic sizing via `image-property'
;; with :max-width/:max-height instead of ImageMagick scaling.

;;; Code:

(require 'mm-decode)
(require 'url-queue)
;; For the `image-property' setf-expander (not preloaded on emacs-nox).
(require 'image)

(defgroup jabber-image nil
  "Image display settings."
  :group 'jabber)

(defcustom jabber-image-max-width 300
  "Maximum width in pixels for inline images."
  :type 'integer)

(defcustom jabber-image-max-height 300
  "Maximum height in pixels for inline images."
  :type 'integer)

(defun jabber-image--mime-to-type (mime-type)
  "Return an image type symbol for MIME-TYPE string, or nil."
  (when mime-type
    (pcase mime-type
      ("image/png"  'png)
      ("image/jpeg" 'jpeg)
      ("image/gif"  'gif)
      ("image/webp" 'webp)
      ("image/svg+xml" 'svg)
      ("image/bmp"  'bmp)
      ("image/x-xbitmap" 'xbm)
      ("image/x-xpixmap" 'xpm)
      ("image/tiff" 'tiff)
      (_ nil))))

(defun jabber-image-create (data &optional mime-type max-width max-height)
  "Create a dynamically-sized image from raw DATA string.
MIME-TYPE is a MIME type string like \"image/png\"; if nil Emacs
auto-detects the type.  MAX-WIDTH and MAX-HEIGHT default to
`jabber-image-max-width' and `jabber-image-max-height'."
  (let ((image (create-image data
                             (jabber-image--mime-to-type mime-type)
                             t)))
    (setf (image-property image :max-width)
          (or max-width jabber-image-max-width))
    (setf (image-property image :max-height)
          (or max-height jabber-image-max-height))
    image))

(defun jabber-image-create-from-file (file &optional max-width max-height)
  "Create a dynamically-sized image from FILE path.
MAX-WIDTH and MAX-HEIGHT default to `jabber-image-max-width' and
`jabber-image-max-height'."
  (let ((image (create-image file)))
    (setf (image-property image :max-width)
          (or max-width jabber-image-max-width))
    (setf (image-property image :max-height)
          (or max-height jabber-image-max-height))
    image))

(defun jabber-image-fetch (url callback &rest cbargs)
  "Fetch image at URL asynchronously.
When complete, call CALLBACK with the image object (or nil on
error) followed by CBARGS.  Image is sized per
`jabber-image-max-width' and `jabber-image-max-height'."
  (url-queue-retrieve
   url
   (lambda (status cb args)
     (let ((url-buffer (current-buffer))
           (image (unless (plist-get status :error)
                    (goto-char (point-min))
                    (when (re-search-forward "\r?\n\r?\n" nil t)
                      (let* ((handle (mm-dissect-buffer t))
                             (img (mm-get-image handle)))
                        (when img
                          (setf (image-property img :max-width)
                                jabber-image-max-width)
                          (setf (image-property img :max-height)
                                jabber-image-max-height)
                          img))))))
       (kill-buffer url-buffer)
       (apply cb image args)))
   (list callback cbargs)
   'silent
   'inhibit-cookies))

(provide 'jabber-image)

;;; jabber-image.el ends here
