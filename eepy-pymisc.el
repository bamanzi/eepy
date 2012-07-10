;;; eepy-pymisc --- misc stuff related to python

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.


;; Virtualenv Commands
(autoload 'virtualenv-activate "virtualenv"
  "Activate a Virtual Environment specified by PATH" t)
(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)


;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; rst
(autoload 'rst-mode "rst"
  "Major mode for editing reStructuredText documents." t)

(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))

;; Django
;;...

;; PyDB
(autoload 'pydb  "pydb"
  "Run pydb on program FILE in buffer *gud-cmd-FILE*." t)

(provide 'eepy-pymisc)
