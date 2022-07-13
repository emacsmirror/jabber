;;;###autoload
(defun jabber-private-get (jc node-name namespace success-callback error-callback)
  "Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result."
  (jabber-send-iq jc nil "get"
		  `(query ((xmlns . "jabber:iq:private"))
			  (,node-name ((xmlns . ,namespace))))
		  #'jabber-private-get-1 success-callback
		  #'(lambda (jc xml-data error-callback)
		      (funcall error-callback jc xml-data))
		  error-callback))

(defun jabber-private-get-1 (jc xml-data success-callback)
  (funcall success-callback jc
	   (car (jabber-xml-node-children
		 (jabber-iq-query xml-data)))))

;;;###autoload
(defun jabber-private-set (jc fragment &optional
			      success-callback success-closure-data
			      error-callback error-closure-data)
  "Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

JC is the Jabber connection."
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . "jabber:iq:private"))
			  ,fragment)
		  success-callback success-closure-data
		  error-callback error-closure-data))
