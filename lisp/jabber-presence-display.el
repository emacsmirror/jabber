;;; jabber-presence-display.el --- Shared presence display data  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Immutable mappings shared by presence renderers and completion UI.

;;; Code:

;;;###autoload
(defconst jabber-presence-faces
  '(("" . jabber-roster-user-online)
    ("away" . jabber-roster-user-away)
    ("xa" . jabber-roster-user-xa)
    ("dnd" . jabber-roster-user-dnd)
    ("chat" . jabber-roster-user-chatty)
    ("error" . jabber-roster-user-error)
    (nil . jabber-roster-user-offline))
  "Mapping from presence types to faces.")

(defconst jabber-presence-strings
  `(("" . ,(propertize "Online" 'face 'jabber-roster-user-online))
    ("away" . ,(propertize "Away" 'face 'jabber-roster-user-away))
    ("xa" . ,(propertize "Extended Away" 'face 'jabber-roster-user-xa))
    ("dnd" . ,(propertize "Do not Disturb" 'face 'jabber-roster-user-dnd))
    ("chat" . ,(propertize "Chatty" 'face 'jabber-roster-user-chatty))
    ("error" . ,(propertize "Error" 'face 'jabber-roster-user-error))
    (nil . ,(propertize "Offline" 'face 'jabber-roster-user-offline)))
  "Mapping from presence types to readable, colorized strings.")

(provide 'jabber-presence-display)

;;; jabber-presence-display.el ends here
