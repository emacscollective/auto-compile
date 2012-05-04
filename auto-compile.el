;;; auto-compile.el --- compile Emacs Lisp files after visiting buffers are saved

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.7.0-pre
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

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the minor mode `auto-compile-global-mode' which
;; automatically compiles Emacs Lisp code when the visiting buffers are
;; saved to their source files, provided that the respective byte code
;; files already exists.  If the byte code file does not already exist
;; nothing is done.

;; To start or stop compiling a source file or multiple files at once use
;; the command `toggle-auto-compile' which toggles automatic compilation
;; by either compiling the selected source file(s) or by removing the
;; respective byte code file(s).  The appropriate action is determined by
;; the existence respectively absence of the byte code file.

;; Automatically compiling Emacs Lisp source files after each save is
;; useful for at least the following reasons:

;; * Emacs prefers the byte code file over the source file even if the
;;   former is outdated.  Without a mode which automatically recompiles
;;   the source files you will at least occasionally forget to do so
;;   manually and end up with an old version of your code being loaded.

;; * There are many otherwise fine libraries to be found on the Internet
;;   which when compiled will confront the user with a wall of compile
;;   warnings and an occasional error.  If authors are informed about
;;   these (often trivial) problems after each save they will likely fix
;;   them quite quickly.  That or they have a high noise tolerance.

;; * It's often easier and less annoying to fix errors and warnings as
;;   they are introduced than to do a "let's compile today's work and see
;;   how it goes".

;; So do yourself and others a favor and enable this mode by adding the
;; following to your init file:
;;
;;     (auto-compile-global-mode 1)

;; Auto-Compile mode is designed to stay out of your way as much as it
;; can while still motivating you to get things fixed.  But Auto-Compile
;; mode can also be configured to be more insistent, which might be
;; annoying initially but less so once existing problems have been fixed.

;; Occasionally you might be tempted to turn of Auto-Compile mode locally
;; because you are doing some work which causes lots of expected warnings
;; until you are actually done.  Don't do so: because Emacs prefers the
;; byte code file you would also have to remove that, in which case you
;; don't have to turn of this mode anymore.  In other words use the
;; command `toggle-auto-compile' instead.

;; Also note that just because no warnings and/or errors are reported when
;; Auto-Compile mode compiles a source file this does not necessarily mean
;; that users of your libraries won't see any.  A likely cause for this
;; would be that you forgot to require a feature which is loaded on your
;; system but not necessarily on the users' systems.  So you should still
;; manually compile your packages before release:
;;
;;     emacs -batch -q --no-site-file \
;;       -L . -L ../dependency/ -f batch-byte-compile *.el

;;; Code:

(defgroup auto-compile nil
  "Compile Emacs Lisp source files after the visiting buffers are saved."
  :group 'convenience
  :prefix 'auto-compile
  :link '(function-link toggle-auto-compile)
  :link '(function-link auto-compile-byte-compile)
  :link '(function-link auto-compile-mode))

;;;###autoload
(define-minor-mode auto-compile-mode
  "Compile Emacs Lisp source files after the visiting buffers are saved.

After a buffer containing Emacs Lisp code is saved to its source file
update the respective byte code file.  If the latter does not exist do
nothing.  Therefor to disable automatic compilation remove the byte code
file.  See command `toggle-auto-compile' for a convenient way to do so."
  :lighter auto-compile-mode-lighter
  :group 'auto-compile
  (if auto-compile-mode
      (add-hook  'after-save-hook 'auto-compile-byte-compile nil t)
    (remove-hook 'after-save-hook 'auto-compile-byte-compile t)))

;;;###autoload
(define-globalized-minor-mode auto-compile-global-mode
  auto-compile-mode auto-compile-on)

(defun auto-compile-on ()
  (when (eq major-mode 'emacs-lisp-mode)
    (auto-compile-mode 1)))

(defcustom auto-compile-verbose nil
  "Whether to print messages describing progress of byte-compiler."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-always-recompile t
  "Whether to recompile all source files when turning on auto compilation.

When turning on auto compilation for files in a directory recompile source
files even if their byte code file already exist and are up-to-date.

If you disable this you may alternatively turn off, then turn on again
auto compilation to recompile all files in the directory."
  :type 'boolean)

(defcustom auto-compile-recursive "^[^.]"
  "Whether to recurse into subdirectories when toggling auto compilation.

Must be a boolean or a regular expression in which case only directories
whose file-name match are recursed into.  The files in a directory
explicitly selected are always processed."
  :type '(choice (const  :tag "All subdirectories" t)
		 (const  :tag "Non-hidden subdirectories" "^[^.]")
		 (string :tag "Matching subdirectories")
		 (const  :tag "Don't" nil)))

(defcustom auto-compile-check-parens t
  "Whether to check for unbalanced parentheses before compiling.

This only has as an effect on files which are currently being visited in
a buffer other files are compiled without this prior check.  If unbalanced
parentheses are found no attempt is made to compile the file as that would
obviously fail also."
  :type 'boolean)

(defcustom auto-compile-visit-failed t
  "Whether to visit source files which failed to compile.

If this is non-nil visit but don't select a source file if it isn't being
visited in a buffer already.  Also set the buffer local value of variable
`auto-compile-pretend-byte-compiled' (which see) to t and mark the buffer
as modified if the value of variable `auto-compile-mark-failed-modified'
is non-nil."
  :type 'boolean)

(defcustom auto-compile-mark-failed-modified t
  "Whether to mark buffers which failed to compile as modified.

This serves as a reminder to fix fatal errors.  While useful this can
get annoying so this variable can be quickly toggled with the command
`auto-compile-toggle-mark-failed-modified'."
  :type 'boolean)

(defcustom auto-compile-ding t
  "Whether to beep (or flash the screen) when an error occurs.

Auto-Compile mode continues after an errors occurs (compile error,
unmatched parens, or failure to remove file) because aborting and
therefor not processing the remaining files would be confusing.  Instead
it continues and beeps or flashes the screen to get the users attention;
set this variable to nil to disable even that."
  :type 'boolean)

(defvar auto-compile-mode-lighter ""
  "Mode lighter for Auto-Compile Mode.")

;;;###autoload
(defun toggle-auto-compile (file action)
  "Toggle automatic compilation of an Emacs Lisp source file or files.

Read a file or directory name from the minibuffer defaulting to the
visited Emacs Lisp source file or `default-directory' if no such file is
being visited in the current buffer.  If the user exits with a directory
selected then all source files in that directory will have their status
set, otherwise just the selected file.

Toggling happens by either compiling the source files(s) or by removing
the respective byte code file(s).  See `auto-compile-mode'.

The appropriate action is determined by the existence respectively absence
of the byte code file for the selected source file.  If a directory was
selected but a source file was current when this command was invoked
use that file to determine the action.  Otherwise prompt the user.

To explicitly select an action use a positive prefix argument to compile
the source file(s) or a negative prefix argument to remove the respective
byte code file(s).

Note that even when a directory was selected, the action is determined
only once and then applied to all source files regardless of the presence
or absence of the respective byte code files."
  (interactive
   (let* ((buf  (current-buffer))
	  (file (when (eq major-mode 'emacs-lisp-mode)
		  (buffer-file-name)))
	  (action
	   (cond
	    (current-prefix-arg
	     (if (> (prefix-numeric-value current-prefix-arg) 0)
		 'start
	       'quit))
	    (file
	     (if (or (file-exists-p (byte-compile-dest-file file))
		     (and (eq major-mode 'emacs-lisp-mode)
			  (file-exists-p (byte-compile-dest-file
					  (buffer-file-name buf)))))
		 'quit
	       'start))
	    (t
	     (case (read-char-choice
		    "Toggle automatic compilation (s=tart, q=uit, C-g)? "
		    '(?s ?q))
	       (?s 'start)
	       (?q 'quit))))))
     (list (read-file-name (concat (capitalize (symbol-name action))
				   " auto-compiling: ")
			   (when file (file-name-directory file))
			   nil t
			   (when file (file-name-nondirectory file)))
	   action)))
  (if (file-regular-p file)
      (case action
	(start (auto-compile-byte-compile file t))
	(quit  (auto-compile-delete-dest (byte-compile-dest-file file))))
    (when (called-interactively-p 'any)
      (let ((log (get-buffer byte-compile-log-buffer)))
	(when log
	  (kill-buffer log))))
    (dolist (f (directory-files file t))
      (cond
       ((file-directory-p f)
	(when (and auto-compile-recursive
		   (or (not (stringp auto-compile-recursive))
		       (string-match
			auto-compile-recursive
			(file-name-nondirectory (directory-file-name f)))))
	  (toggle-auto-compile f action)))
       ((eq action 'start)
	(when (and (string-match "\\.el\\(\\.gz\\)?$" f) ; FIXME
		   (file-exists-p f)
		   (or auto-compile-always-recompile
		       (file-newer-than-file-p f (byte-compile-dest-file f)))
		   (or (not (string-match "^\\.?#" (file-name-nondirectory f)))
		       (file-exists-p (byte-compile-dest-file f))))
	  (auto-compile-byte-compile f t)))
       ((string-match "\\.elc\\(\\.gz\\)?$" f)
	(if (file-exists-p (auto-compile-source-file f))
	    (auto-compile-delete-dest f)
	  (message "Source file was not found; keeping %s" f)))))))

(defun auto-compile-toggle-mark-failed-modified ()
  "Toggle whether buffers which failed to compile are marked as modified."
  (interactive)
  (message (concat (if (setq auto-compile-mark-failed-modified
			     (not auto-compile-mark-failed-modified))
		       "Mark "
		     "Don't mark ")
		   "files that failed to compile as modified")))

(defvar auto-compile-pretend-byte-compiled nil
  "Whether to try again to compile this file after a failed attempt.

Command `auto-compile-byte-compile' sets this buffer local variable to t
after failing to compile a source file being visited in a buffer (or when
variable `auto-compile-visit-failed' is non-nil) causing it to try again
when being called again.  Command `toggle-auto-compile' will also pretend
the byte code file exists.")
(make-variable-buffer-local 'auto-compile-pretend-byte-compiled)

(defun auto-compile-byte-compile (&optional file start)
  "Perform byte compilation for Auto-Compile mode."
  (let (dest buf)
    (when (and file
	       (setq buf (get-file-buffer file))
	       (buffer-modified-p buf)
	       (y-or-n-p (format "Save buffer %s first? " (buffer-name buf))))
      (with-current-buffer buf (save-buffer)))
    (unless file
      (setq file (buffer-file-name)
	    buf  (get-file-buffer file)))
    (catch 'auto-compile
      (when (and auto-compile-check-parens buf)
	(condition-case check-parens
	    (save-restriction
	      (widen)
	      (check-parens))
	  (error
	   (auto-compile-handle-compile-error file buf)
	   (throw 'auto-compile nil))))
      (when (or start
		(file-exists-p (byte-compile-dest-file file))
		(when buf
		  (with-current-buffer buf
		    auto-compile-pretend-byte-compiled)))
	(condition-case byte-compile
	    (let ((byte-compile-verbose auto-compile-verbose))
	      (byte-compile-file file)
	      (when buf
		(with-current-buffer buf
		  (kill-local-variable auto-compile-pretend-byte-compiled))))
	  (file-error
	   (message "Byte-compiling %s failed" file)
	   (auto-compile-handle-compile-error file buf)))))))

(defun auto-compile-delete-dest (dest &optional failurep)
  (unless failurep
    (let ((buf (get-file-buffer (auto-compile-source-file dest))))
      (when buf
	(with-current-buffer buf
	  (kill-local-variable 'auto-compile-pretend-byte-compiled)))))
  (condition-case nil
      (when (file-exists-p dest)
	(message "Deleting %s..." dest)
	(delete-file dest)
	(message "Deleting %s...done" dest))
    (file-error
     (auto-compile-ding)
     (message "Deleting %s...failed" dest))))

(defun auto-compile-handle-compile-error (file buf)
  (auto-compile-ding)
  (let ((dest (byte-compile-dest-file file)))
    (when (file-exists-p dest)
      (auto-compile-delete-dest dest t)))
  (when (or buf
	    (and auto-compile-visit-failed
		 (setq buf (find-file-noselect file))))
    (with-current-buffer buf
      (setq auto-compile-pretend-byte-compiled t)
      (when auto-compile-mark-failed-modified
	(set-buffer-modified-p t)))))

(defun auto-compile-source-file (dest)
  (let ((standard (concat (file-name-sans-extension dest) ".el"))
	(suffixes load-file-rep-suffixes)
	file)
    (while (and (not file) suffixes)
      (unless (file-exists-p (setq file (concat standard (pop suffixes))))
	(setq file nil)))
    (or file standard)))

(defun auto-compile-ding ()
  (when auto-compile-ding
    (ding)))

(provide 'auto-compile)
;;; auto-compile.el ends here
