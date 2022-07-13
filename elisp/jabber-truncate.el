(defvar jabber-log-lines-to-keep 1000
  "Maximum number of lines in chat buffer.")

(defun jabber-truncate-top (buffer &optional ewoc)
  "Clean old history from a chat BUFFER.
Optional EWOC is ewoc-widget to work.  Default is `jabber-chat-ewoc'
`jabber-log-lines-to-keep' specifies the number of lines to
keep.

Note that this might interfer with
`jabber-chat-display-more-backlog': you ask for more history, you
get it, and then it just gets deleted."
  (interactive)
    (let* ((inhibit-read-only t)
           (work-ewoc (if ewoc ewoc jabber-chat-ewoc))
          (delete-before
           ;; go back one node, to make this function "idempotent"
           (ewoc-prev
            work-ewoc
            (ewoc-locate work-ewoc
                         (save-excursion
                           (set-buffer buffer)
                           (goto-char (point-max))
                           (forward-line (- jabber-log-lines-to-keep))
                           (point))))))
      (while delete-before
        (setq delete-before
              (prog1
                  (ewoc-prev work-ewoc delete-before)
                (ewoc-delete work-ewoc delete-before))))))

(defun jabber-truncate-muc (nick group buffer text proposed-alert)
  "Clean old history from MUC buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep."
  (jabber-truncate-top buffer))

(defun jabber-truncate-chat (from buffer text proposed-alert)
  "Clean old history from chat buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep.

Note that this might interfer with
`jabber-chat-display-more-backlog': you ask for more history, you
get it, and then it just gets deleted."
  (jabber-truncate-top buffer))
