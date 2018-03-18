-include .config.mk

PKG = auto-compile

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = packed

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

all: lisp

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make readme       - generate README.md)
	$(info make clean        - remove byte-code and autoloads)
	@printf "\n"

lisp: $(ELCS) loaddefs

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

.PHONY: README.md
README.md:
	@echo "Generating README.md..."
	@$(BATCHE) "\
(let (start end commentary)\
  (with-temp-buffer\
    (insert-file \"auto-compile.el\")\
    (re-search-forward \"^;;; Commentary:\n\n\")\
    (setq start (point))\
    (re-search-forward \"^;;; Code:\")\
    (forward-line -1)\
    (setq end (point-marker))\
    (replace-regexp \"^;; ?\" \"\"  nil start end)\
    (replace-regexp \"^- \" \"* \"  nil start end)\
    (replace-regexp \"\\\\(\`[^']+\\\\)'\" \"\\\\1\`\" nil start end)\
    (setq commentary (buffer-substring start end)))\
  (with-current-buffer (find-file-noselect \"README.md\")\
    (erase-buffer)\
    (insert \"Automatically compile Emacs Lisp libraries\n\")\
    (insert \"------------------------------------------\n\n\")\
    (insert commentary ?\n)\
    (save-buffer)))"
