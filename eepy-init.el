;;; eepy-init.el --- initialization of EEPY

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.

;;** path initialization

;; Trick to get the filename of the installation directory
(defconst eepy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

(add-to-list 'load-path eepy-install-dir)

;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "extensions/yasnippet"
                   "extensions/auto-complete"
                   "elisp"
                   )
                 )
  (add-to-list 'load-path (concat eepy-install-dir relpath)))

;; add `eepy/bin' to PATH, for epylint, pep8 etc
(let ( (bin-path (concat eepy-install-dir "bin")) )
       (add-to-list 'exec-path bin-path)
       (setenv "PATH" (concat bin-path path-separator (getenv "PATH"))))

;; add `eepy/python-libs' to PYTHONPATH
(let ( (pylibs-path (concat eepy-install-dir "python-libs")) )
       (setenv "PYTHONPATH" (concat pylibs-path path-separator (getenv "PYTHONPATH"))))

;;** customization group
(defgroup eepy nil
  "emacs-for-python package"
  :group  'python
  :prefix "eepy-")

;;** major modes
(defcustom eepy-prefered-major-mode 'built-in
  "Default python major mode loaded automatically by EEPY."
  :group 'eepy
  :type '(choice (const :tag "built-in"          'built-in)
                 (const :tag "python-mode.el"    'python-mode-el)
                 (const :tag "fgallina"          'fgallina)
                 (const :tag "loveshack"         'loveshack)))

(cond
 ((eq eepy-prefered-major-mode 'built-in)
  (require 'python))
 ((eq eepy-prefered-major-mode 'python-mode-el)
  (require 'python-mode
           (concat eepy-install-dir "python-modes/python-mode-el/python-mode")))
 (t
  (require 'python
           (concat eepy-install-dir "python-modes/" (format "%s" 'fgallina) "python"))
  ))

(defun eepy-current-buffer-major-mode-vendor ()
  "Detect the vendor of current buffer's major-mode.

In case user loaded another implementation after opened some buffers,
here our check are buffer-specific.

'python-mode-el https://launchpad.net/python-mode
'fgallina       http://github.com/fgallina/python.el
'loveshack      http://www.loveshack.ukfsn.org/emacs/python.el
'built-in       GNU Emacs built-in"
  (let ((pkgpath (symbol-file major-mode)))
    (cond
     ((string-match "python-mode[\./]" pkgpath)
      'python-mode-el               ;; https://launchpad.net/python-mode
     ((string-match "fgallina" pkgpath)
      'fgallina)                    ;; https://github.com/fgallina/python.el
     ((string-match "loveshack" pkgpath)
      'loveshack)                   ;; http://www.loveshack.ukfsn.org/emacs/NEWS.python
     (t
      'built-in)))))


;;** pymacs
(autoload 'pymacs-load  "pymacs"
  "Import the Python module named MODULE into Emacs." t)
(autoload 'pymacs-eval  "pymacs"
  "Compile TEXT as a Python expression, and return its value." t)
(autoload 'pymacs-load  "pymacs"
  "Import the Python module named MODULE into Emacs." t)

;(require 'pymacs (concat eepy-install-dir "extensions/pymacs.el"))


(provide 'eepy-init)
