.phony: all autoload compile lint clean

build: autoload compile

dev: autoload compile lint

autoload:
	emacs -q --batch --eval="(require 'package)" \
	--eval="(package-generate-autoloads \"jabber\" default-directory)"

compile:
	emacs -q -Q --batch \
	--eval="(setq print-length nil)" \
	--eval="(add-to-list 'load-path \"$(pwd)\")" \
	--eval="(add-to-list 'load-path \"jabber-fallback-lib\")" \
	-f batch-byte-compile elisp/

lint-check-declare:
	for file in elisp/*.el ; do \
	emacs -q -Q --batch --eval="(check-declare-file \"$$file\")" ; \
	done

lint-checkdoc:
	for file in elisp/*.el ; do \
	emacs -q -Q --batch --eval="(checkdoc-file \"$$file\")" ; \
	done

lint-package-lint:
	emacs -Q --batch \
	--eval='(package-initialize)' --eval="(require 'package-lint)" \
        -f 'package-lint-batch-and-exit' $(wildcard elisp/*.el)

lint-relint:
	emacs -Q --batch \
	--eval='(package-initialize)' --eval="(require 'relint)" \
	-f 'relint-batch' "elisp"

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint

clean-elc:
	-rm elisp/*.elc

clean: clean-elc
