;;;###autoload
(defgroup jabber-keepalive nil
  "Keepalive functions try to detect lost connection"
  :group 'jabber)

(defcustom jabber-keepalive-interval 600
  "Interval in seconds between connection checks."
  :type 'integer
  :group 'jabber-keepalive)

(defcustom jabber-keepalive-timeout 20
  "Seconds to wait for response from server."
  :type 'integer
  :group 'jabber-keepalive)

(defvar jabber-keepalive-timer nil
  "Timer object for keepalive function.")

(defvar jabber-keepalive-timeout-timer nil
  "Timer object for keepalive timeout function.")

(defvar jabber-keepalive-pending nil
  "List of outstanding keepalive connections.")

(defvar jabber-keepalive-debug nil
  "Log keepalive traffic when non-nil.")

;;;###autoload
(defun jabber-keepalive-start (&optional jc)
  "Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect it if it doesn't answer.  See variable `jabber-keepalive-interval'
and variable `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument."
  (interactive)

  (when jabber-keepalive-timer
    (jabber-keepalive-stop))

  (setq jabber-keepalive-timer
	(run-with-timer 5
			jabber-keepalive-interval
			'jabber-keepalive-do))
  (add-hook 'jabber-post-disconnect-hook 'jabber-keepalive-stop))

(defun jabber-keepalive-stop ()
  "Deactivate keepalive."
  (interactive)

  (when jabber-keepalive-timer
    (jabber-cancel-timer jabber-keepalive-timer)
    (setq jabber-keepalive-timer nil)))

(defun jabber-keepalive-do ()
  (when jabber-keepalive-debug
    (message "%s: sending keepalive packet(s)" (current-time-string)))
  (setq jabber-keepalive-timeout-timer
	(run-with-timer jabber-keepalive-timeout
			nil
			'jabber-keepalive-timeout))
  (setq jabber-keepalive-pending jabber-connections)
  (dolist (c jabber-connections)
    ;; Whether we get an error or not is not interesting.
    ;; Getting a response at all is.
    (jabber-ping-send c nil 'jabber-keepalive-got-response nil nil)))

(defun jabber-keepalive-got-response (jc &rest args)
  (when jabber-keepalive-debug
    (message "%s: got keepalive response from %s"
	     (current-time-string)
	     (plist-get (fsm-get-state-data jc) :server)))
  (setq jabber-keepalive-pending (remq jc jabber-keepalive-pending))
  (when (and (null jabber-keepalive-pending) (timerp jabber-keepalive-timeout-timer))
    (jabber-cancel-timer jabber-keepalive-timeout-timer)
    (setq jabber-keepalive-timeout-timer nil)))

(defun jabber-keepalive-timeout ()
  (jabber-cancel-timer jabber-keepalive-timer)
  (setq jabber-keepalive-timer nil)

  (dolist (c jabber-keepalive-pending)
    (message "%s: keepalive timeout, connection to %s considered lost"
	     (current-time-string)
	     (plist-get (fsm-get-state-data c) :server))

    (run-hook-with-args 'jabber-lost-connection-hooks c)
    (jabber-disconnect-one c nil)))

(defcustom jabber-whitespace-ping-interval 30
  "Send a space character to the server with this interval, in seconds.

This is a traditional remedy for a number of problems: to keep NAT
boxes from considering the connection dead, to have the OS discover
earlier that the connection is lost, and to placate servers which rely
on the client doing this, e.g.  Openfire.

If you want to verify that the server is able to answer, see
`jabber-keepalive-start' for another mechanism."
  :type '(integer :tag "Interval in seconds")
  :group 'jabber-core)

(defvar jabber-whitespace-ping-timer nil
  "Timer object for whitespace pings.")

;;;###autoload
(defun jabber-whitespace-ping-start (&optional jc)
  "Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts."
  (interactive)

  (when jabber-whitespace-ping-timer
    (jabber-whitespace-ping-stop))

  (setq jabber-whitespace-ping-timer
	(run-with-timer 5
			jabber-whitespace-ping-interval
			'jabber-whitespace-ping-do))
  (add-hook 'jabber-post-disconnect-hook 'jabber-whitespace-ping-stop))

(defun jabber-whitespace-ping-stop ()
  "Deactivate whitespace pings."
  (interactive)

  (when jabber-whitespace-ping-timer
    (jabber-cancel-timer jabber-whitespace-ping-timer)
    (setq jabber-whitespace-ping-timer nil)))

(defun jabber-whitespace-ping-do ()
  (dolist (c jabber-connections)
    (ignore-errors (jabber-send-string c " "))))
