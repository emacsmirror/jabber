.phony: all autoload compile lint clean

build: autoload compile

dev: autoload compile lint

autoload:
	emacs -q --batch --eval="(require 'package)" \
	--eval="(package-generate-autoloads \"jabber\" default-directory)"

compile:
	emacs -q -Q -L . -L lisp -L lisp/jabber-fallback-lib --batch \
	--eval="(setq print-length nil)" \
	-f batch-byte-compile lisp/

lint-check-declare:
	for file in lisp/*.el ; do \
	emacs -q -Q --batch --eval="(check-declare-file \"$$file\")" ; \
	done

lint-checkdoc:
	for file in lisp/*.el ; do \
	emacs -q -Q --batch --eval="(checkdoc-file \"$$file\")" ; \
	done

lint-package-lint:
	emacs -Q --batch \
	--eval='(package-initialize)' --eval="(require 'package-lint)" \
        -f 'package-lint-batch-and-exit' $(wildcard lisp/*.el)

lint-relint:
	emacs -Q --batch \
	--eval='(package-initialize)' --eval="(require 'relint)" \
	-f 'relint-batch' "lisp"

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint

clean-elc:
	-rm lisp/*.elc

clean: clean-elc
