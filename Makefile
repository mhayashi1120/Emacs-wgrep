###
### Package
###

-include env.mk

NEEDED-PACKAGES ?= dash ag s

EL := wgrep.el
EL += wgrep-ack.el
EL += wgrep-ag.el
EL += wgrep-helm.el
EL += wgrep-pt.el
EL += wgrep-deadgrep.el

TEST_EL := wgrep-test.el
TEST_EL += wgrep-test-helper.el

##
## Emacs
##

EMACS ?= emacs

BATCH := $(EMACS) -Q -batch -L .

##
## package.el
##

ifdef ELPA-DIR
	BATCH += -eval "(setq package-user-dir (expand-file-name \"$(ELPA-DIR)\"))"
endif

# This come from `package-lint/run-tests.sh`
define package-installer
  "(progn \
   (require 'package) \
   (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
   (package-initialize) \
   (package-refresh-contents) \
   (dolist (pkg '($(1))) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"
endef

###
### Command
###

BUILD_BATCH := $(BATCH) -eval "(require 'package)" -f package-initialize
ifndef EMACS_LINT_IGNORE
	BUILD_BATCH += -eval "(setq byte-compile-error-on-warn t)"
endif

ifdef EMACS_LINT_IGNORE
	LINT_BATCH := true
else
	LINT_BATCH := $(BATCH) -eval $(call package-installer, package-lint)
endif

CI_BATCH := $(BATCH) -eval $(call package-installer, package-lint $(NEEDED-PACKAGES))

###
### Files
###

ELC := $(EL:%.el=%.elc)
BUILD_GENERATED := *.elc
MAINTAINER_GENERATED := elpa *~

LOAD_EL := $(EL:%=-l %)
LOAD_ELC := $(ELC:%=-l %)

LOAD_TEST_EL := $(TEST_EL:%=-l %)

###
### General rule
###

.PHONY: all check compile clean

all: check

check: compile
	$(BUILD_BATCH) $(LOAD_EL) $(LOAD_TEST_EL) -f ert-run-tests-batch-and-exit
	$(BUILD_BATCH) $(LOAD_ELC) $(LOAD_TEST_EL) -f ert-run-tests-batch-and-exit

compile:
	$(BUILD_BATCH) -f batch-byte-compile $(EL)

clean:
	rm -rf $(BUILD_GENERATED)

###
### Maintainer rule
###

.PHONY: lint package maintainer-clean

lint:
	$(LINT_BATCH) -f package-lint-batch-and-exit $(EL)

package: lint check


maintainer-clean: clean
	rm -rf $(MAINTAINER_GENERATED)

###
### CI/CD rule
###

.PHONY: ci prepare-cicd

ci: prepare-cicd package

prepare-cicd:
	$(CI_BATCH)
