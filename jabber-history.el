;; jabber-history.el - recording message history

;; Copyright (C) 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2004 - Mathias Dahl

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

;;; Log format:
;; Each message is on one separate line, represented as a vector with
;; five elements.  The first element is time encoded according to
;; JEP-0082.  The second element is direction, "in" or "out".
;; The third element is the sender, "me" or a JID.  The fourth
;; element is the recipient.  The fifth element is the text
;; of the message.

;; FIXME: when rotation is enabled, jabber-history-query won't look
;; for older history files if the current history file doesn't contain
;; enough backlog entries.

(require 'jabber-core)
(require 'jabber-util)


(provide 'jabber-history)

;; arch-tag: 0AA0C235-3FC0-11D9-9FE7-000A95C2FCD0
