;;; auto-compile.el --- automatically compile Emacs Lisp files

;; Copyright (C) 2008, 2009 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Updated: 20090210
;; Version: 0.4
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

(require 'cl)
(require 'save-local-vars nil t)

(defgroup auto-compile nil
  "Automatically compile Emacs Lisp files."
  :group 'Convenience
  :link '(function-link auto-compile-mode))

(defun auto-compile-modify-hooks ()
  (cond ((not auto-compile-mode)
	 (remove-hook 'after-save-hook 'auto-compile-file-maybe)
	 (remove-hook 'kill-buffer-hook 'auto-compile-file-maybe))
	(auto-compile-when
	 (add-hook 'after-save-hook 'auto-compile-file-maybe)
	 (remove-hook 'kill-buffer-hook 'auto-compile-file-maybe))
	(t
	 (remove-hook 'after-save-hook 'auto-compile-file-maybe)
	 (add-hook 'kill-buffer-hook 'auto-compile-file-maybe))))

;;;###autoload
(define-minor-mode auto-compile-mode
  "Automatically compile Emacs Lisp files.

A file might be compiled everytime it is saved or only when it's buffer
is destroyed.  This is controlled through the option `auto-compile-when'.

A file might be compiled (1) automatically, (2) after the user has been
asked, (3) never.

This behaviour depends on various variables described below. The function
`auto-compile-file-maybe' goes through various steps to decide what should
be done.  These steps are listed here.  After each step, if the behaviour
has been unambiously decided, all remaining steps, and therefor the
variables they depend on, don't have any effect.

0. If `auto-compile-flag' is set locally obey it.

1. If `auto-compile-flag' is set globally to `ask-always' then ask the
   user.

2. If `auto-compile-concider-no-byte' is set to nil file _might_ be
   compiles if and only if a byte file already exists. Otherwise if set
   to t file _might_ be compiled regardless if a byte file exists.

3. If the file is explicitly included or excluded then do as requested.
   The regexps in `auto-compile-include' and `auto-compile-exclude' are
   used for explicit inclusion and exclusion.  If a file matches a regexp
   in both variables the following mechanism is used to determine the
   closer match:

   If one of the regexps matches file-names at the end (ends with $) then
   that is assumed to be the closer match.  This allows to have a setting
   for most files in a directory but another for some of them.

   If no or both regexps match file-names at the end then the length of
   the matched strings are compared and the longer wins.  This allows to
   have a setting for files in a directory but another for files in a
   subdirectory.

4. For all others files the global value of `auto-compile-flag' decides
   what should be done.

   t                Compile file without asking.
   nil              Don't compile file.
   ask              Ask wether file should be compiled.
   compiledp        Recompile if compiled file exists; otherwise don't.
   compiledp-or-ask Recompile if compiled file exists; otherwise ask.

After the user was prompted whether to compile some file the choice can be
saved.  See option `auto-compile-remember'."
  :lighter " AC"
  :global t
  (auto-compile-modify-hooks))

(defcustom auto-compile-when t
  "Event triggering compilation.

t   Compile when saving.
nil Compile when killing.

This variable can be set locally for a file."
  :group 'auto-compile
  :type '(choice
          (const :tag "Compile when saving" t)
          (const :tag "Compile on buffer deletion." nil))
  :set (lambda (variable value)
	 (set-default variable value)
	 (auto-compile-modify-hooks)))

(put 'auto-compile-when 'safe-local-variable 'booleanp)

(defcustom auto-compile-flag 'ask
  "Level of automation when compiling files.

t                Compile file if it has not explicitly been excluded.
nil              Only compile file if it has explicitly been included.
ask              Ask whether file should be compiled.
ask-always       Always ask whether file should be compiled.
compiledp        Recompile if byte-file exists; otherwise don't.
compiledp-or-ask Recompile if byte-file exists; otherwise ask.

Exact behaviour depends on some other variables. See `auto-compile-mode'.

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

(defcustom auto-compile-remember 'ask
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

See `auto-compile-mode' for a description of how conflicts
between this option and `auto-compile-exclude' are handled."
  :group 'auto-compile
  :type '(repeat regexp))

(defcustom auto-compile-exclude nil
  "List of exclusion regular expressions for automatic compilation.

Matching files are excluded from automatic compilation.

See `auto-compile-mode' for a description of how conflicts
between this option and `auto-compile-include' are handled."
  :group 'auto-compile
  :type '(repeat regexp))

(defcustom auto-compile-concider-no-byte t
  "If files for which no byte file exists are considered for compilation.

t   Concider file regardless if byte file exists.
nil Only concider file if byte file exists."
  :group 'auto-compile
  :type '(choice
          (const :tag "Concider file regardless if byte file exists." t)
          (const :tag "Only concider file if byte file exists." nil)))

(defun auto-compile-file-ask (file)
  (let ((compile (yes-or-no-p (format "Compile %s " file)))
	remember save)
    (when compile
      (byte-compile-file file))
    (case auto-compile-remember
      (session (setq remember t))
      (save (setq remember t save 'list))
      (ask (let (answer)
	     (while (null answer)
	       (message "Remember choice? (y, n, s, f or ?) ")
	       (setq answer (let ((cursor-in-echo-area t))
			      (read-char-exclusive)))
	       (case (downcase answer)
		 (?y (setq remember t))
		 (?s (setq remember t save 'list))
		 (?f (setq remember t save 'file))
		 (?n)
		 (?? (setq answer nil)
		     (with-output-to-temp-buffer "*Auto-Compile Help*"
		       (princ "\
<y>: Remember choice until buffer is closed.
<n>: Do not remember choice, ask again.
<s>: Remember choice and save in variable.
<f>: Remember choice and save in file.")
		       (save-excursion
			 (set-buffer standard-output)
			 (help-mode))))
		 (t (setq answer nil)
		    (beep)
		    (message "Please answer y, n, s or f; or ? for help")
		    (sit-for 3)))))))
    (when remember
      (make-local-variable 'auto-compile-flag)
      (setq auto-compile-flag compile))
    (case save 
      (file (unless (featurep 'save-local-vars)
	      (error "Library save-local-vars required to save choice in file"))
	    (save-local-variable 'auto-compile-flag))
      (list (let* ((symbol (if compile
			       'auto-compile-include
			     'auto-compile-exclude))
		   (value (custom-quote
			   (cons (concat "^" (regexp-quote file))
				 (symbol-value symbol)))))
	      (set symbol value)
	      (put symbol 'saved-value (list value))
	      (put symbol 'customized-value nil)
	      (unless (featurep 'cus-edit+)
		(custom-push-theme 'theme-value symbol 'user 'set value)))
	    (custom-save-all)))
    (let ((buffer (get-buffer "*Auto-Compile Help*")))
      (when buffer
	(kill-buffer-and-its-windows buffer)))))

(defun auto-compile-file-maybe ()
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
		 (byte-compile-file file))
	       t)
	      ;; 1. ask if we always ask
	      ((eq auto-compile-flag 'ask-always)
	       (auto-compile-file-ask file)
	       t)
	      ;; 2. missing required byte file
	      ((and (not auto-compile-concider-no-byte)
		    (not (file-exists-p byte-file))))
	      ;; 3. automatic inclusion/exclusion
	      ((let* ((domatch (lambda (str reg)
				 (string-match reg str)))
		      (include (car (member* file auto-compile-include
					     :test domatch)))
		      (inmatch (when include
				 (match-string 0 file)))
		      (exclude (car (member* file auto-compile-exclude
					     :test domatch)))
		      (exmatch (when exclude
				 (match-string 0 file))))
		 (cond ((and include exclude)
			(let ((in-end (string-match "\\$$" include))
			      (ex-end (string-match "\\$$" exclude)))
			  (cond ((and in-end (not ex-end))
				 (byte-compile-file file)
				 t)
				((and (not in-end) ex-end))
				((>= (length inmatch)
				     (length exmatch))
				 (byte-compile-file file))
				(t t))))
		       (include
			(byte-compile-file file)
			t)
		       (exclude t))))
	      ;; 4. obey global flag
	      ((eq auto-compile-flag t)
	       (byte-compile-file file))
	      ((eq auto-compile-flag 'ask)
	       (auto-compile-file-ask file))
	      ((and (eq auto-compile-flag 'compiledp)
		    (file-exists-p byte-file))
	       (byte-compile-file file))
	      ((eq auto-compile-flag 'compiledp-or-ask)
	       (if (file-exists-p byte-file)
		   (byte-compile-file file)
		 (auto-compile-file-ask file))))))))

(provide 'auto-compile)
;;; auto-compile.el ends here
