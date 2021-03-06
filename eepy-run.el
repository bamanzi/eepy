;;; eepy-run --- run & debug python script

;; This file is part of Enhaneced Emacs for PYthon suite (EEPY)
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.


;; Virtualenv Commands
(autoload 'virtualenv-activate "virtualenv"
  "Activate a Virtual Environment specified by PATH" t)
(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)


;;** working with pdb
;; stolen from http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))
 
(add-hook 'python-mode-hook 'python--add-debug-highlight)
 
(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")
 
(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
(define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)

;;** highlight error line in compilation result / shell-mode
;; stolen from http://www.loveshack.ukfsn.org/emacs/python.el
(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

(defun python-compilation-minor-mode (arg)
  (interactive "P")
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (compilation-minor-mode t))



;; PyDB
(autoload 'pydb  "pydb"
  "Run pydb on program FILE in buffer *gud-cmd-FILE*." t)

