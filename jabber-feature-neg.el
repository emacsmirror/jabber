(jabber-disco-advertise-feature "http://jabber.org/protocol/feature-neg")

(defun jabber-fn-parse (xml-data type)
  "Parse a Feature Negotiation request, return alist representation.
XML-DATA should have one child element, <x/>, in the jabber:x:data
namespace.

TYPE is either 'request or 'response.

Returned alist has field name as key, and value is a list of offered
alternatives."
  (let ((x (car (jabber-xml-get-children xml-data 'x))))
    (unless (and x
		 (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data"))
      (jabber-signal-error "Modify" 'bad-request "Malformed Feature Negotiation"))

    (let (alist
	  (fields (jabber-xml-get-children x 'field)))
      (dolist (field fields)
	(let ((var (jabber-xml-get-attribute field 'var))
	      (value (car (jabber-xml-get-children field 'value)))
	      (options (jabber-xml-get-children field 'option)))
	  (setq alist (cons
		       (cons var
			     (cond
			      ((eq type 'request)
			       (mapcar #'(lambda (option)
					   (car (jabber-xml-node-children
						 (car (jabber-xml-get-children
						       option 'value)))))
				       options))
			      ((eq type 'response)
			       (jabber-xml-node-children value))
			      (t
			       (error "Incorrect Feature Negotiation type: %s" type))))
		       alist))))
      ;; return alist
      alist)))

(defun jabber-fn-encode (alist type)
  "Transform a feature alist into an <x/> node int the jabber:x:data namespace.
Note that this is not the reverse of `jabber-fn-parse'.

TYPE is either 'request or 'response."
  (let ((requestp (eq type 'request)))
    `(x ((xmlns . "jabber:x:data")
	 (type . ,(if requestp "form" "submit")))
	 ,@(mapcar #'(lambda (field)
		       `(field
			 ((type . "list-single")
			  (var . ,(car field)))
			 ,@(if requestp
				(mapcar
				 #'(lambda (option)
				     `(option nil (value nil ,option)))
				 (cdr field))
			      (list `(value nil ,(cadr field))))))
		   alist))))

(defun jabber-fn-intersection (mine theirs)
  "Find values acceptable to both parties.

MINE and THEIRS are alists, as returned by `jabber-fn-parse'.

An alist is returned, where the keys are the negotiated variables,
and the values are lists containing the preferred option.  If
negotiation is impossible, an error is signalled.  The errors are as
specified in XEP-0020, and not necessarily the ones of higher-level
protocols."

  (let ((vars (mapcar #'car mine))
	(their-vars (mapcar #'car theirs)))

    ;; are the same variables being negotiated?
    (sort vars 'string-lessp)
    (sort their-vars 'string-lessp)
    (let ((mine-but-not-theirs (cl-set-difference vars their-vars :test 'string=))
	  (theirs-but-not-mine (cl-set-difference their-vars vars :test 'string=)))
      (when mine-but-not-theirs
	(jabber-signal-error "Modify" 'not-acceptable (car mine-but-not-theirs)))
      (when theirs-but-not-mine
	(jabber-signal-error "Cancel" 'feature-not-implemented (car theirs-but-not-mine))))

      (let (alist)
	(dolist (var vars)
	  (let ((my-options (cdr (assoc var mine)))
		(their-options (cdr (assoc var theirs))))
	    (let ((common-options (cl-intersection my-options their-options :test 'string=)))
	      (if common-options
		  ;; we have a match; but which one to use?
		  ;; the first one will probably work
		  (setq alist
			(cons (list var (car common-options))
			      alist))
		;; no match
		(jabber-signal-error "Modify" 'not-acceptable var)))))
	alist)))
