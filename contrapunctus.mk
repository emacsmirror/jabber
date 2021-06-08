.phony: all setup tangle compile lint clean

all: setup tangle compile lint

setup:
	emacs --batch --eval="(package-initialize)" \
	--eval="(mapcar #'package-install '(indent-lint package-lint relint))"

jabber.el:
	emacs -q -Q --batch --eval="(require 'ob-tangle)" \
        --eval='(org-babel-tangle-file "jabber.org")'

tangle: jabber.el

compile: tangle
	if [ -z "$$ORG_PATH" ]; then \
	  emacs -q -Q --batch --eval='(byte-compile-file "jabber.el")' ; \
	else \
	  emacs -q -Q --batch --eval="(add-to-list 'load-path \"$$ORG_PATH\")" --eval='(byte-compile-file "jabber.el")' ; \
	fi

lint-check-declare: tangle
	emacs -q -Q --batch --eval='(check-declare-file "jabber.el")'

lint-checkdoc: tangle
	emacs -q -Q --batch --eval='(checkdoc-file "jabber.el")'

lint-package-lint: setup tangle
	emacs -Q --batch --eval='(package-initialize)' \
        --eval="(require 'package-lint)" \
        -f 'package-lint-batch-and-exit' jabber.el

lint-relint: setup tangle
	emacs -q -Q --batch --eval='(relint-file "jabber.el")'

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint

clean-tangle:
	-rm jabber.el

clean-elc:
	-rm *.elc

clean: clean-elc
