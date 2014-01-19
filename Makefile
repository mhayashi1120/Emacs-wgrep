EMACS = emacs

check: compile

compile:
	$(EMACS) -q -batch -L . -f batch-byte-compile wgrep.el wgrep-ag.el wgrep-helm.el wgrep-ack.el
