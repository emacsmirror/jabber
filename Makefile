.phony: all setup tangle autoload compile lint clean

build: tangle autoload compile

dev: setup tangle autoload compile lint

setup:
	emacs --batch --eval="(package-initialize)" \
	--eval="(mapcar #'package-install '(indent-lint package-lint relint))"

# No -q or -Q without ORG_PATH - if the user has a newer version of
# Org, we want to use it.
jabber.el:
	if [ -z "${ORG_PATH}" ]; then \
          echo ; \
	  echo "[WARNING] ORG_PATH is unset. Org versions older than 9.3.8 have a bug" ; \
	  echo "which result in them always using absolute paths in comment links in" ; \
	  echo "tangled source files. If your Org version is older than 9.3.8, please" ; \
	  echo "upgrade it before tangling." ; \
          echo ; \
          emacs --batch \
	emacs --batch \
	--eval="(progn (package-initialize) (require 'ob-tangle))" \
	--eval='(org-babel-tangle-file "jabber.org")' ; \
        else \
          emacs -q -Q --batch \
          --eval="(add-to-list 'load-path \"${ORG_PATH}\")" \
          --eval="(require 'ob-tangle)" \
          --eval='(org-babel-tangle-file "jabber.org")' ; \
        fi

tangle: jabber.el

autoload:
	emacs -q --batch --eval="(require 'package)" \
	--eval="(package-generate-autoloads \"jabber\" default-directory)"

compile: tangle
	emacs -q -Q --batch --eval='(byte-compile-file "jabber.el")' ; \

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
