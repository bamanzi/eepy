;;; eepy-checker.el --- static checker and flymake checker for EEPY suite

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.

(require 'eepy-init)

;;** develock
;;(setq develock-auto-enable nil)

(autoload 'develock-mode  "develock"
  "additional font-lock keywords for the developers." t)

;;TODO: (defcustom eepy-enable-develock)

(eval-after-load "develock"
  `(progn
     (load-file (concat eepy-install-dir "elisp/develock-py.el"))
     ))


;;** static checker
(defun eepy/lint-file-with (cmdline)
  (interactive "sCmdline:")
  (let ((compile-command (if (string-match "%f" cmdline)
                             (replace-regexp-in-string "%f"
                                                       (shell-quote-argument (file-name-nondirectory (buffer-file-name)))
                                                       cmdline)
                           (concat cmdline " " (shell-quote-argument (file-name-nondirectory (buffer-file-name)))))))
    (call-interactively 'compile)
  ))

   
(defun eepy-pylint ()
  (interactive)
  (eepy/lint-file-with "epylint")  ;; "pylint -rn -f parseable "
  )

(defun eepy-pep8 ()
  (interactive)
  (eepy/lint-file-with "pep8")
  )

(defun eepy-pychecker ()
  (interactive)
  (eepy/lint-file-with "pychecker")
  )

(defun eepy-pyflakes ()
  (interactive)
  (eepy/lint-file-with "pyflakes")
  )
  

(defcustom eepy-static-checker "epylint"
  "Default static checker/lint program for python."
  :group 'eepy
  )

(defun eepy-set-checker (checker)
  "Set default checker (compiler) for `eepy-check'."
  (interactive
   (list (ido-completing-read "Checker: "
                              '("epylint" "pep8" "pyflakes" "pychecker")
                              nil
                              nil
                              nil
                              nil
                              eepy-static-checker)))
  (setq eepy-static-checker checker)
  (message "Default python checker set to %s." checker))

(defun eepy-lint-file ()
  "Static check current file with `eepy-static-checker'"
  (interactive)
  (eepy/lint-file-with eepy-static-checker)
  )

;;** flymake
(when (or (require 'flymake (concat eepy-install-dir "elisp/flymake") t)
          (require 'flymake))
  (if (boundp 'flymake-info-line-regex)
      (setq flymake-info-line-regex     ;;only available in `flymake-patch.el'
            (append flymake-info-line-regex '("unused$" "^redefinition" "used$"))))

  (load "flymake-cursor" nil t)
  (require 'rfringe nil t))


(defun flymake-create-copy-file ()
       "Create a copy local file"
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace)))
         (file-relative-name
          temp-file
          (file-name-directory buffer-file-name))))

(defvar eepy-flymake-default-cmdline "epylint %f"
  "Fail-safe value for `eepy-flymake-cmdline.")

(defvar eepy-flymake-cmdline-pylint    "epylint %f")
(defvar eepy-flymake-cmdline-pep8      "pep8 %f")
(defvar eepy-flymake-cmdline-pychecker "pychecker %f")
(defvar eepy-flymake-cmdline-pyflakes  "pyflakes %f")

(defcustom eepy-flymake-cmdline nil
  "Syntax checker command line used for flymake-mode in python.

The CMDLINE should be something like:

  flymaker %f

e.g.
  python -mpylint \"%f\"

%f will be substituted with a temporary copy of the file that is
 currently being checked.

Make sure a `%f' is included in the command line"
  :type '(choice (const :tag "Off" nil)
                 (const :tag "pylint"     eepy-flymake-cmdline-pylint)
                 (const :tag "pep8"       eepy-flymake-cmdline-pep8)
                 (const :tag "pychecker"  eepy-flymake-cmdline-pychecker)
                 (const :tag "pyflakes"   eepy-flymake-cmdline-pyflakes)
                 (string :tag "custom..."))
  :group 'eepy)


(defun flymake-eepy-init ()
    (let ((cmdline-subst (replace-regexp-in-string "%f"
                                                   (shell-quote-argument (flymake-create-copy-file))
                                                   (or eepy-flymake-cmdline eepy-flymake-default-cmdline))))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))

(add-to-list 'flymake-allowed-file-name-masks '("\\.pyw?\\'" flymake-eepy-init))

(defun eepy-flymake-with (cmdline)
  (interactive
      (list (ido-completing-read "Checker: "
                              '(eepy-flymake-cmdline-pylint
                                eepy-flymake-cmdline-pep8
                                eepy-flymake-cmdline-pyflakes
                                eepy-flymake-cmdline-pychecker)
                              nil
                              nil
                              nil
                              nil
                              eepy-flymake-cmdline)))
;;  (make-variable-buffer-local 'eepy-flymake-cmdline)
  (setq eepy-flymake-cmdline cmdline)
  (flymake-mode -1)
  (flymake-mode t))

(defun python-mode-hook-flymake ()
  "initialize flymake on open python files."
  (when eepy-flymake-cmdline
    (eepy-flymake-with eepy-flymake-cmdline)))

(add-hook 'python-mode-hook 'python-mode-hook-flymake)


(provide 'eepy-checker)
