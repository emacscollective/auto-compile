;;; auto-compile.el --- automatically compile Emacs Lisp files

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Updated: 20100307
;; Version: 0.6
;; Homepage: https://github.com/tarsius/auto-compile
;; Keywords: compile, convenience, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically compile Emacs Lisp files when they are saved or when
;; their buffers are killed.  Also see `auto-compile-mode's doc-string.

;;; Code:

(require 'read-char-spec)

(defgroup auto-compile nil
  "Automatically compile Emacs Lisp files."
  :group 'Convenience
  :link '(function-link auto-compile-mode))

(defun auto-compile-modify-hooks (&optional local)
  "Modify value of various hooks according to `auto-compile-when' and LOCAL.
LOCAL if non-nil should be `remove-local' or `set-local'."
  (cond ((or (eq local 'remove-local)
	     (and (not local)
		  (not auto-compile-mode)))
	 (remove-hook 'after-save-hook  'check-parens local)
	 (remove-hook 'after-save-hook  'auto-compile-file-maybe local)
	 (remove-hook 'kill-buffer-hook 'auto-compile-file-maybe local))
	((eq auto-compile-when t)
	 (remove-hook 'after-save-hook  'check-parens local)
	 (add-hook    'after-save-hook  'auto-compile-file-maybe t local)
	 (remove-hook 'kill-buffer-hook 'auto-compile-file-maybe local))
	(t
	 (add-hook    'after-save-hook  'check-parens t local)
	 (remove-hook 'after-save-hook  'auto-compile-file-maybe local)
	 (add-hook    'kill-buffer-hook 'auto-compile-file-maybe local))))

;;;###autoload
(define-minor-mode auto-compile-mode
  "Automatically compile Emacs Lisp files.

A file might be compiled every time it is saved or only when it's buffer
is destroyed.  This is controlled through the option `auto-compile-when'.

A file might be compiled (1) automatically, (2) after the user has been
asked, (3) never.

This behavior depends on various variables described below. The function
`auto-compile-file-maybe' goes through various steps to decide what should
be done.  These steps are listed here.  After each step, if the behavior
has been unambiguously decided, all remaining steps, and therefor the
variables they depend on, don't have any effect.

0. If `auto-compile-flag' is set locally (as a buffer local value) obey
   it.

1. If `auto-compile-flag' is set globally to `ask-always' then ask the
   user.

2. If `auto-compile-consider-no-byte' is set to nil the file _might_ be
   compiles if and only if a byte file already exists. Otherwise if set
   to t file _might_ be compiled regardless if a byte file exists.

   If this is set to t you can still use command `byte-compile-file' to
   manually compile a file once and cause `auto-compile' to consider it
   in the future.

3. If the file is explicitly included or excluded then do as requested.

   The regexps in `auto-compile-include' and `auto-compile-exclude' are
   used for explicit inclusion and exclusion their values are lists of
   regular expressions.

   First the best match in each variable is determined independently.
   The best match is usually the longest matched string.  But if one or
   more of the regular expressions that are used to match at the end of
   a string (that is if it ends with $) then only the matches of such
   regular expressions are compared by size.

   Then the best match from each variable are compared.  If only one ends
   with $ then it wins.  Otherwise if both or none end with $ the longer
   one wins.

   This mechanism, while somewhat complicated, allow rather sophisticated
   rules.

4. For all others files the global value of `auto-compile-flag' decides
   what should be done.

   t                Compile file without asking.
   nil              Don't compile file.
   ask              Ask whether file should be compiled.
   compiledp        Recompile if compiled file exists; otherwise don't.
   compiledp-or-ask Recompile if compiled file exists; otherwise ask.

After the user was prompted whether to compile some file the choice can be
saved.  See option `auto-compile-remember'.

Before a file is actually compiled `check-paren' is called, which
in case of an unmatched bracket or quote positions point near the error.
When only compiling upon killing of a file-visiting buffers you can still
choose to always call `check-paren' when saving.  See option
`auto-compile-when'.

When turned on `auto-compile-mode' is in effect in all buffer visiting
Emacs Lisp files.  Though it might not have an effect in some of them as
described above.  You can also toggle automatic compilation on and off in
a given buffer using `toggle-local-auto-compile'.  This even works if
`auto-compile-mode' is not turned on."
  :global t
  (auto-compile-modify-hooks))

(defcustom auto-compile-when t
  "Event triggering compilation.

t   Compile when saving file.
nil Compile when killing buffer.
1   Compile when killing buffer; check parentheses when saving

This variable can be set locally for a file."
  :group 'auto-compile
  :type '(choice
	  (const :tag "Compile when saving" t)
	  (const :tag "Compile when killing buffer." nil)
	  (const :tag "Compile when killing buffer; check parens when saving." 1))
  :set (lambda (variable value)
	 (set-default variable value)
	 (auto-compile-modify-hooks)))

(put 'auto-compile-when 'safe-local-variable 'booleanp)

(defcustom auto-compile-flag 'compiledp-or-ask
  "Level of automation when compiling files.

t                Compile file if it has not explicitly been excluded.
nil              Only compile file if it has explicitly been included.
ask              Ask whether file should be compiled.
ask-always       Always ask whether file should be compiled.
compiledp        Recompile if byte-file exists; otherwise don't.
compiledp-or-ask Recompile if byte-file exists; otherwise ask.

Exact behavior depends on some other variables. See `auto-compile-mode'.

This variable can be set locally for a file to t or nil.  If set locally
the global value `ask-always' does not have any effect for the given file."
  :group 'auto-compile
  :type '(choice
          (const :tag "Compile file if it has not explicitly been excluded." t)
          (const :tag "Only compile file if it has explicitly been included." nil)
          (const :tag "Ask whether file should be compiled." ask)
          (const :tag "Always ask whether file should be compiled." ask-always)
          (const :tag "Recompile if byte-file exists; otherwise don't." compiledp)
          (const :tag "Recompile if byte-file exists; otherwise ask." compiledp-or-ask)))

(put 'auto-compile-flag 'safe-local-variable 'booleanp)

(defcustom auto-compile-remember 'save
  "Duration for which user choices should be remembered.

session Remember choice for this session only.
save    Remember for future sessions.
ask     Ask whether to remember choice.
nil     Don't remember choice."
  :group 'auto-compile
  :type '(choice
	  (const :tag "Remember choice for this session only." session)
	  (const :tag "Remember choice for future sessions." save)
	  (const :tag "Ask whether to remember choice." ask)
	  (const :tag "Don't remember choice." nil)))

(defcustom auto-compile-include nil
  "List of inclusion regular expressions for automatic compilation.

Matching files are automatically compiled.

Entries can start with \"~\" which will be replaced with the users home
directory before the entry is actually used as regular expression.  This
also works if an entry starts with \"^~\".

See `auto-compile-mode' for a description of how conflicts
between this option and `auto-compile-exclude' are handled."
  :group 'auto-compile
  :type '(repeat regexp))

(defcustom auto-compile-exclude nil
  "List of exclusion regular expressions for automatic compilation.

Matching files are excluded from automatic compilation.

Entries can start with \"^~\" which will be replaced with the users home
directory before the entry is actually used as regular expression.  This
also works if an entry starts with \"^~\".

See `auto-compile-mode' for a description of how conflicts
between this option and `auto-compile-include' are handled."
  :group 'auto-compile
  :type '(repeat regexp))

(defcustom auto-compile-consider-no-byte nil
  "Whether files with no matching byte file are considered for compilation.

t   Consider file regardless if byte file exists.
nil Only consider file if byte file exists.

If this is set to t you can still use command `byte-compile-file' to
manually compile a file once and cause `auto-compile' to consider it
in the future."
  :group 'auto-compile
  :type '(choice
          (const :tag "Consider file regardless if byte file exists." t)
          (const :tag "Only consider file if byte file exists." nil)))

(defun toggle-local-auto-compile ()
  "Toggle the local buffer local value of `auto-compile-flag'.

This always toggles between t and nil.  If there is no local value yet
and the global value isn't a boolean then set the local value to nil.

This toggle is mainly intended for situations when you know that some file
compile temporarily won't compile without errors and/or warnings or even is
in an unbalanced state.

If your library is in an balanced state and `auto-compile-when' is
customized to check if it is well balanced this would annoyingly jump to
the error doing exactly what you wanted to avoid.  In order to prevent
this `auto-compile-when' is locally set to t if it's global value is 1.

After your library can be safely compiled again use command
`auto-compile-kill-local' to remove all relevant local variables.

You can use the command even when `auto-compile-mode' is not enabled,
allowing you to use this library in a less intrusive way only in situation
when you want to explicitly update or test your changes but not call
`byte-compile-file' manually."
  (interactive)
  (make-local-variable 'auto-compile-flag)
  (cond ((memq auto-compile-flag '(t 1))
	 (unless auto-compile-mode
	   (auto-compile-modify-hooks 'set-local))
	 (when (eq auto-compile-flag 1)
	   (make-local-variable 'auto-compile-flag)
	   (setq auto-compile-when t))
	 (setq auto-compile-flag nil)
	 (message "Automatic compilation locally disabled"))
	(t
	 (setq auto-compile-flag t)
	 (message "Automatic compilation locally enabled"))))

(defalias 'auto-compile-toggle-local 'toggle-local-auto-compile)

(defun auto-compile-kill-local (&optional zap)
  "Kill all local `auto-compile' variables possibly zapping memory.

This is useful after you have used `toggle-local-auto-compile' or when
you where already prompted whether to compile some file but have changed
your mind.  The next time you will save the file (or kill the buffer) you
are asked again.

If you have chosen to also save your choice then you have to call this
function with a prefix argument in order to be asked again.

This will modify `auto-compile-include' or `auto-compile-exclude'.  If
this does not work multiple expressions in these variables might match the
file name.  Try using this command again or customize these variables
manually.

The modifications made by this command are only in effect in the current
session.  To save them permanently you have to use Custom.  You could also
use library `cus-edit++.el' which prompts when exiting Emacs and there are
unsaved changes to custom options.

If you have saved your choice by adding a file local variable this command
fails and you have to remove the definition manually."
  (interactive "p")
  (kill-local-variable 'auto-compile-flag)
  (when zap
    (let ((match (auto-compile-file-match (buffer-file-name))))
      (when match
	(remove (cadr match)
		(symbol-value (if (car match)
				  'auto-compile-include
				'auto-compile-exclude))))))
  (kill-local-variable 'auto-compile-when)
  (auto-compile-modify-hooks 'remove-local))

(defun auto-compile-file-do (file)
  "Check parenthesis in FILE, then compile FILE.
Always return t."
  (check-parens)
  (byte-compile-file file)
  t)

(defun auto-compile-file-ask (file)
  "Ask the user whether to compile FILE.
Always return t."
  (let ((compile (yes-or-no-p (format "Compile %s " file)))
	remember save)
    (when compile
      (auto-compile-file-do file))
    (case auto-compile-remember
      (session (setq remember t))
      (save (setq remember t save 'list))
      (ask (eval (read-char-spec
		  "Remember choice? "
		  '((?y (setq remember t)
			"Remember choice until buffer is closed.")
		    (?s (setq remember t save 'list)
			"Do not remember choice, ask again.")
		    (?f (setq remember t save 'file)
			"Remember choice and save in variable.")
		    (?n nil
			"Remember choice and save in file."))))))
    (when remember
      (make-local-variable 'auto-compile-flag)
      (setq auto-compile-flag compile))
    (case save 
      (file (unless (require 'save-local-vars nil t)
	      (error "Library save-local-vars required to save choice in file"))
	    (with-no-warnings
	      (save-local-variable 'auto-compile-flag)))
      (list (let* ((symbol (if compile
			       'auto-compile-include
			     'auto-compile-exclude))
		   (value (cons (concat "^" (regexp-quote file))
				(symbol-value symbol))))
	      (set symbol value)
	      (put symbol 'saved-value (list (custom-quote value)))
	      (put symbol 'customized-value nil)
	      (unless (featurep 'cus-edit+)
		(custom-push-theme 'theme-value symbol 'user 'set value)))
	    (custom-save-all)))
    (let ((buffer (get-buffer "*Auto-Compile Help*")))
      (when buffer
	(kill-buffer-and-its-windows buffer))))
  t)

(defun auto-compile-file-maybe ()
  "Compile the file the current buffer is visiting, or not.

Whether the file should be compiled is determined as described in the
doc-string of `auto-compile-mode' and might require prompting for user
input.

The return value is not significant."
  (unless (bound-and-true-p inhibit-auto-compile)
    (let ((file buffer-file-name) byte-file)
      (when (and file
		 (or (string-match "\\.el\\(\\.gz\\)?\\'" file)
		     (eq major-mode 'emacs-lisp-mode)))
	(setq byte-file (cond ((string-match "\\.el\\'" file)
			       (concat file "c"))
			      ((string-match "\\.el.gz\\'" file)
			       (concat (substring file 0 -3) "c"))
			      (t
			       (concat file ".elc"))))
	;; See `auto-compile-mode's doc-string for explanation.
	(cond ((file-newer-than-file-p byte-file file))
	      ;; 0. obey local flag
	      ((local-variable-p 'auto-compile-flag)
	       (when (eq auto-compile-flag t)
		 (auto-compile-file-do file)))
	      ;; 1. ask if we always ask
	      ((eq auto-compile-flag 'ask-always)
	       (auto-compile-file-ask file))
	      ;; 2. missing required byte file
	      ((and (not auto-compile-consider-no-byte)
		    (not (file-exists-p byte-file))))
	      ;; 3. automatic inclusion/exclusion
	      ((case (auto-compile-file-match file)
		 (1) ((nil) t) (t (auto-compile-file-do file))))
	      ;; 4. obey global flag
	      ((eq auto-compile-flag t)
	       (auto-compile-file-do file))
	      ((eq auto-compile-flag 'ask)
	       (auto-compile-file-ask file))
	      ((and (eq auto-compile-flag 'compiledp)
		    (file-exists-p byte-file))
	       (auto-compile-file-do file))
	      ((eq auto-compile-flag 'compiledp-or-ask)
	       (if (file-exists-p byte-file)
		   (auto-compile-file-do file)
		 (auto-compile-file-ask file))))))))

(defun auto-compile-file-match-1 (file regexps)
  "Return the best match for FILE in REGEXPS.

FILE has to be a file name and REGEXPS a list of regular expressions.
Match each member of REGEXPS against FILE and return a cons cell whose car
is the longest match produced by any member of REGEXP.  The cdr of the
return value is t if the regular expression that produced that match ends
with \"$\" or nil otherwise.  If no regular expression matches return nil."
  (let (regexp match tailp result)
    (while (setq regexp (pop regexps))
      (when (string-match
	     (replace-regexp-in-string
	      "^^?\\(~\\)/" (getenv "HOME") regexp nil nil 1)
	     file)
	(setq match (match-string 0 file)
	      tailp (string-match "\\$$" regexp))
	(when (if (eq tailp (cdr result))
		  (> (length match)
		     (length (car result)))
		tailp)
	  (setq result (cons match tailp)))))
    result))

(defun auto-compile-file-match (file)
  "Return t if FILE should be compiled, nil if not and 1 if unknown.

Whether FILE should be compiled is determined if possible by comparing it
with the values of `auto-compile-include' and `auto-compile-exclude' as
described in point three in the doc-string of `auto-compile-mode'."
  (let ((include (auto-compile-file-match-1 file auto-compile-include))
	(exclude (auto-compile-file-match-1 file auto-compile-exclude)))
    (if (and include exclude)
	(or (and (cdr include)
		 (not (cdr exclude)))
	    (>= (length (car include))
		(length (car exclude))))
      (if (or include exclude)
	  (not exclude)
	1))))

(provide 'auto-compile)
;;; auto-compile.el ends here
