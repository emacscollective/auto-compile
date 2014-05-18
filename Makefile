EMACS  ?= emacs
EFLAGS ?=
BATCH   = $(EMACS) $(EFLAGS) -batch -Q
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -L . $(LOADPATH) -f batch-byte-compile

ELS  = auto-compile.el
ELCS = $(ELS:.el=.elc)

LOADPATH ?= -L ../packed

lisp: $(ELCS)
%.elc: %.el
	@$(BATCHC) $<

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)
