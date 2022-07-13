(require 'button)

(defvar jabber-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'jabber-popup-chat-menu)
    (define-key map "\C-c\C-r" 'jabber-popup-roster-menu)
    (define-key map "\C-c\C-i" 'jabber-popup-info-menu)
    (define-key map "\C-c\C-m" 'jabber-popup-muc-menu)
    (define-key map "\C-c\C-s" 'jabber-popup-service-menu)
    ;; note that {forward,backward}-button are not autoloaded.
    ;; thus the `require' above.
    (when (fboundp 'forward-button)
      (define-key map [?\t] 'forward-button)
      (define-key map [backtab] 'backward-button))
    map))

;;;###autoload
(defvar jabber-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" 'jabber-connect-all)
    (define-key map "\C-d" 'jabber-disconnect)
    (define-key map "\C-r" 'jabber-switch-to-roster-buffer)
    (define-key map "\C-j" 'jabber-chat-with)
    (define-key map "\C-l" 'jabber-activity-switch-to)
    (define-key map "\C-a" 'jabber-send-away-presence)
    (define-key map "\C-o" 'jabber-send-default-presence)
    (define-key map "\C-x" 'jabber-send-xa-presence)
    (define-key map "\C-p" 'jabber-send-presence)
    map)
  "Global Jabber keymap (usually under C-x C-j).")

;;;###autoload
(define-key ctl-x-map "\C-j" jabber-global-keymap)
