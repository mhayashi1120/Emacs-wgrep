-include env.mk

EMACS ?= emacs
BATCH := $(EMACS) -q -batch -L .

TEST_BATCH := $(BATCH) -l $(DASH-EL) -l $(S-EL)

# NOTE: This come from `pacakge-lint/run-tests.sh`
LINT_BATCH := $(BATCH) -eval "(progn \
   (require 'package) \
   (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
   (package-initialize) \
   (package-refresh-contents))"

EL = wgrep.el
EL += wgrep-ack.el
EL += wgrep-ag.el
EL += wgrep-helm.el
EL += wgrep-pt.el

ELC := $(EL:%.el=%.elc)

LOAD_EL := $(EL:%=-l %)
LOAD_ELC := $(ELC:%=-l %)

GENERATED := *.elc

###
### General target
###

all: check

check: compile
	$(TEST_BATCH) -l wgrep.el -l wgrep-test.el -f ert-run-tests-batch-and-exit
	$(TEST_BATCH) -l wgrep.elc -l wgrep-test.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile $(EL)

clean:
	rm -f $(GENERATED)

###
### for Package maintainer
###

lint:
	$(LINT_BATCH) -l $(PACKAGE-LINT-EL) -f package-lint-batch-and-exit $(EL)

check-extra: check subtest

subtest:
	$(TEST_BATCH) \
		-l $(AG-EL) \
		$(LOAD_EL) \
		-l wgrep-test.el -l wgrep-subtest.el \
		-eval "(ert-run-tests-batch-and-exit '(tag wgrep-subtest))"
	$(TEST_BATCH) \
		-l $(AG-EL) \
		$(LOAD_ELC) \
		-l wgrep-test.el -l wgrep-subtest.el \
		-eval "(ert-run-tests-batch-and-exit '(tag wgrep-subtest))"

package: lint check check-extra compile
	@echo ""
	@echo "======================================="
	@echo "Successfuly finished package build. "
	@echo "Maintainer should add correct `git tag` each pacakges."
	@echo "======================================="
	@echo ""
