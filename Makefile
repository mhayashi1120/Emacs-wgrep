EMACS ?= emacs
BATCH := $(EMACS) -q -batch -L .
TEST_BATCH := $(BATCH) -l ./ext/dash.el -l ./ext/s.el

ELC := *.elc
GENERATED := $(ELC)

check: compile
	$(TEST_BATCH) -l wgrep.el -l wgrep-test.el -f ert-run-tests-batch-and-exit
	$(TEST_BATCH) -l wgrep.elc -l wgrep-test.el -f ert-run-tests-batch-and-exit

check2: check download-external subtest

subtest:
	$(BATCH) \
		-L ./ext -l wgrep.el -l wgrep-ag.el -l wgrep-helm.el -l wgrep-ack.el -l wgrep-pt.el \
		-l ext/ag.el \
		-l wgrep-test.el -l wgrep-subtest.el \
		-eval "(ert-run-tests-batch-and-exit '(tag wgrep-subtest))"
	$(BATCH) \
		-L ./ext -l wgrep.elc -l wgrep-ag.elc -l wgrep-helm.elc -l wgrep-ack.elc -l wgrep-pt.elc \
		-l ext/ag.el \
		-l wgrep-test.el -l wgrep-subtest.el \
		-eval "(ert-run-tests-batch-and-exit '(tag wgrep-subtest))"

download-external: clean-ext
	mkdir -p ext
	cd ext && wget "https://github.com/Wilfred/ag.el/raw/master/ag.el"
	cd ext && wget "https://github.com/magnars/dash.el/raw/master/dash.el"
	cd ext && wget "https://github.com/magnars/s.el/raw/master/s.el"

compile:
	$(BATCH) -f batch-byte-compile \
		wgrep.el wgrep-ag.el wgrep-helm.el wgrep-ack.el wgrep-pt.el

clean-ext:
	rm -rf ext

clean:
	rm -f $(GENERATED)
