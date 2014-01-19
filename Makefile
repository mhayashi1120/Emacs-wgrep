EMACS = emacs

check: compile
	$(EMACS) -q -batch \
		-L . -l wgrep.el -l wgrep-ag.el -l wgrep-helm.el -l wgrep-ack.el \
		-l wgrep-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch \
		-L . -l wgrep.elc -l wgrep-ag.elc -l wgrep-helm.elc -l wgrep-ack.elc \
		-l wgrep-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -L . -f batch-byte-compile \
		wgrep.el wgrep-ag.el wgrep-helm.el wgrep-ack.el
