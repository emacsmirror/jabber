;; jabber-socks5.el - SOCKS5 bytestreams by JEP-0065

;; Copyright (C) 2003, 2004, 2007 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

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

(require 'jabber-iq)
(require 'jabber-disco)
(require 'jabber-si-server)
(require 'jabber-si-client)

;; jabber-core will require fsm for us
(require 'jabber-core)

(provide 'jabber-socks5)

;;; arch-tag: 9e70dfea-2522-40c6-a79f-302c8fb82ac5
