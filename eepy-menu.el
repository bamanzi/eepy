;;; eepy-menu.el -- menu for EEPY suite

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.

(require 'eepy-init)

(require 'eepy-checker)
(require 'eepy-ropemacs)
(require 'eepy-completion)
(require 'eepy-doc)
(require 'eepy-pymisc)
(require 'eepy-misc)

(require 'easymenu)

(defun define-menu-for-eepy (keymap)
  (easy-menu-define eepy-menu keymap
    "Menu for emacs-for-python package."
    '("eepy"
      ["Choose method (imenu)..." imenu t]
      ["Choose method (anything)...)" anything-browse-code
       :active (fboundp 'anything-browse-code) ]
      ["Imenu tree" imenu-tree t]
      "--"
      ("Code folding"
       ["hs-minor-mode" hs-minor-mode
        :style toggle :selected hs-minor-mode]
       ["  hideshowvis" hideshowvis-minor-mode
        :style toggle :selected hideshowvis-minor-mode :active hs-minor-mode
        :help "+/- symbol on left-fringe for toggling folding."]
       ["outline-minor-mode" outline-minor-mode
        :style toggle :selected outline-minor-mode]
       ["  hide sublevels" hide-sublevels t]
       ["  show all" show-all t]
       )
      ;; ["Pretty lambda mode" pretty-lambda-mode
      ;;  :style toggle :selected lambda-mode
      ;;  :help "Pretty-print lambdas"]
      ;; ["Develock mode" develock-mode  ;;FIXME: develock would cause some problems
      ;;  :help "A lightweight way to highlight code formatting problems (indentation, whitespaces, long lines...)"
      ;;  :style toggle :selected (and (boundp 'develock-mode) develock-mode)]
      ["Highlight indentation mode" (if (fboundp 'highlight-indentation-mode)  
                                        (highlight-indentation-mode) ;;https://github.com/antonj/Highlight-Indentation-for-Emacs
                                      (highlight-indentation))  ;;python-mode project
       :style toggle :selected (if (boundp 'highlight-indent-active)
                                   highlight-indent-active
                                 (if (boundp 'highlight-indentation-mode)
				     highlight-indentation-mode
				   nil))
       :help "Highlight indentation."]
      "--"
      ["Smart operator mode" smart-operator-mode
       :style toggle :selected (and (boundp 'smart-operator-mode) smart-operator-mode)
       :help "Insert operators with surrounding spaces smartly."]
      ["Autopair mode" autopair-mode
       :style toggle :selected (and (boundp 'autopair-mode) autopair-mode)
       :help "Automagically pair braces and quotes like TextMate."]
      ("Auto-Complete"
       ["on/off" auto-complete-mode
        :style toggle :selected auto-complete-mode]
       ["Emacs builtin completion" (eepy/toggle-ac-source 'ac-source-python-builtin)
        :style toggle :selected (memq 'ac-source-python-builtin ac-sources)]
       ["pycompletemine source"    (eepy/toggle-ac-source 'ac-source-pycompletemine)
        :active (boundp 'ac-source-pycompletemine) ;;and (boundp 'py-shell)
        :style toggle :selected (memq 'ac-source-pycompletemine ac-sources)]
       ["ropemacs source"          (eepy/toggle-ac-source 'ac-source-nropemacs)
        :active nil
        :style toggle :selected (memq 'ac-source-nropemacs ac-sources)]
       ["yasnippets source"        (eepy/toggle-ac-source 'ac-source-yasnippet)
        :style toggle :selected (memq 'ac-source-yasnippet ac-sources)]
       ["scite-api source"         (eepy/toggle-ac-source 'ac-source-scite-api)
        :active (boundp 'ac-source-scite-api)
        :style toggle :selected (memq 'ac-source-scite-api ac-sources)]
       )
      ("Yasnippets"
       ["on/off" yas/minor-mode :style: toggle :selected (and (boundp 'yas/minor-mode) yas/minor-mode)]
       ["Load Django templates" nil nil]
       )
      "--"
      ("Syntax Check"
       ["Pylint"    eepy-pylint t]
       ["PEP8"      eepy-pep8 t]
       ["Pyflakes"  eepy-pyflakes t]
       ["Pychecker" eepy-pychecker t]
       "--"
       ["Ask for saving files before syntax checking"
        (setq compilation-ask-about-save (not compilation-ask-about-save))
        :style toggle :selected compilation-ask-about-save]
       ["next error"      next-error t]
       ["previous error"  previous-error t]
       )
      ("Flymake"
       ["on/off"          flymake-mode
        :style toggle :selected flymake-mode]
       ["Pylint"               (eepy-flymake-with            eepy-flymake-cmdline-pylint)
        :style radio  :selected (and eepy-flymake-cmdline (string-match-p "pylint "  eepy-flymake-cmdline))]
       ["PEP8"                  (eepy-flymake-with            eepy-flymake-cmdline-pep8)
        :style radio  :selected (and eepy-flymake-cmdline (string-match-p "pep8 "   eepy-flymake-cmdline))]
       ["Pyflakes"              (eepy-flymake-with            eepy-flymake-cmdline-pyflakes)
        :style radio  :selected (and eepy-flymake-cmdline (string-match-p "pyflakes " eepy-flymake-cmdline))]
       ["Pychecker"             (eepy-flymake-with            eepy-flymake-cmdline-pychecker)
        :style radio  :selected (and eepy-flymake-cmdline (string-match-p "pychecker " eepy-flymake-cmdline))]
;;       ["(unknown)"   nil
;;        :style radio]
       ["Set default checker..." (customize-variable 'eepy-flymake-cmdline)]
       "--"
       ["next error"      flymake-goto-next-error :active flymake-mode]
       ["previous error"  flymake-goto-prev-error :active flymake-mode]
       )
      ["Python shell" python-shell]
      ["IPython shell" ipython
       :active (fboundp 'ipython)]  ;;FIXME: python-mode.el needed
      ("Debug"
       ["pdb" pdb t]
       ["ipdb" ipdb t]
       ["pydb" pydb t]
       )
      ;;["byte-compile" nil t]
      "--"
      ["eldoc-mode" eldoc-mode
       :style toggle :selected eldoc-mode]
      ["pylookup" pylookup-lookup
       :active nil]
      ["python.chm (windows)" eepy-chm-lookup-keyword
       :active (eq window-system 'w32)]
      ["pydoc" eepy-pydoc
       :help "Query help with 'pydoc' command line tool"]
      "--"
      ("Project"
       ["open/create rope project..." eepy-open-rope-project]
       ["auto-open ropeproject if found" eepy-toggle-auto-detect-rope-project
        :style toggle :selected eepy-auto-detect-rope-project]
       ["ropemacs-mode" ropemacs-mode
        :style toggle :selected (and (boundp 'ropemacs-mode) ropemacs-mode)]
       "--"
;;       ["eproject" eproject t]
       ["Projectile mode" projectile-global-mode
	:style toggle :selected (and (boundp 'projectile-global-mode) projectile-global-mode)]
       ["Create projectile project..." projectile-create-project]
       "--"
       ["Tags tree" tags-tree t]
      )
      ("Misc"
       ["refactoring: rename current symbol" iedit-mode
       :help "Use `iedit-mode' to replace all occurren of current symbol in whole buffer."]
       ["refactoring: rename current symbol in function"  eepy-iedit-in-defun
       :style toggle :selected (and (boundp 'iedit-mode) iedit-mode)
       :help "Use `narrow-to-defun' and 'iedit-mode' to replace all "]
       )
      )))

(define-menu-for-eepy python-mode-map)

(eval-after-load "python-mode"
  `(progn
     (if (boundp 'py-mode-map) ;;not in python-mode.el >= 6.0.4
         (define-menu-for-eepy py-mode-map))
     ))

(provide 'eepy-menu)
