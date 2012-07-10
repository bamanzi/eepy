;;; eepy-misc.el --- misc stuff of EEPY suite

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.

;; This file contains misc stuff that not closely related to Python


;;** browsing code within buffer
(autoload 'idomenu  "idomenu"
  "Switch to a buffer-local tag from Imenu via Ido." t)
(autoload 'anything-imenu "anything-config"
  "Preconfigured `anything' for `imenu'." t)
(autoload 'anything-browse-code  "anything-config"
  "Preconfigured anything to browse code." t)

(defun eepy/idomenu-or-imenu ()
  (interactive)
  (require 'idomenu nil t)
  (if (fboundp 'idomenu)
      (call-interactively 'idomenu)
    (call-interactively 'imenu)))

(defun eepy/go-to-symbol-within-buffer ()
  "Go to symbol (definition) within current buffer.

This would get rid of some annoyance:
- imenu hererachy (anything flats them)
- imenu not working because of cedet
"
  (interactive)
  (anything
   :prompt "Go to: "
   :input (thing-at-point 'symbol)
   :sources
   '(anything-c-source-imenu
     anything-c-source-browse-code)))

(unless (global-key-binding (kbd "M-g s"))
  (define-key goto-map "s" 'eepy/go-to-symbol-within-buffer))

;;TODO: fix-imenu-create-index-function

(autoload 'imenu-tree  "imenu-tree"
  "Display tree view of imenu." t)
(autoload 'tags-tree  "tags-tree"
  "Display tree view of tags." t)


;; get rid of cedet's imenu
(defun eepy/fix-python-mode-imenu ()
  (when (and (fboundp 'setq-mode-local)  ;;cedet already loaded
             (or (not (boundp 'semantic-idle-scheduler-mode))  ;;semantic-mode not turn on
                 (not semantic-idle-scheduler-mode)))    
    (setq-mode-local python-mode
                     imenu-create-index-function
                     (if (eq beginning-of-defun-function 'py-beginning-of-defun-or-class) ;;python-mode.el
                         'py-imenu-create-index
                       'python-imenu-create-index))))

;;(add-hook 'python-mode-hook 'eepy/fix-python-mode-imenu)

;;** code folding
(autoload 'hideshowvis-minor-mode  "hideshowvis"
  "Toggle Hideshowvis minor mode on or off." t)
(eval-after-load "hideshowvis"
  `(load "hideshow-fringe" 'noerror))

(defcustom eepy/enable-hideshow t
  "Whether to enable `hs-minor-mode' for code folding by default"
  :group 'eepy)

;;FIXME: not used yet
(defvar eepy-hideshow-expression
  '("^\\s-*\\(?:def\\|class\\|if\\|else\\|for\\|try\\|except\\)\\>" nil "#" 
    (lambda (arg)
      (python-end-of-block)
      (skip-chars-backward " \t\n"))
    nil)
  "Expression used in `hs-special-modes-alist' for `python-mode'.")

(defun eepy/let-hideshow-more ()
  "let `hideshow' also handle `if/elif/else/for/try/except'.

By default configuration (in python.el), hideshow would only handle `def' and `class'."
	(let ((python-hideshow-exp
	         '("^\\s-*\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\)\\>"
	           nil
	           "#" 
	           (lambda (arg)
	             (python-end-of-block)
	             (skip-chars-backward " \t\n"))
	           nil))
	        (old-config (assoc 'python-mode hs-special-modes-alist)))
	    (if old-config
	        (setcdr old-config python-hideshow-exp)
	      (add-to-list 'hs-special-modes-alist `(python-mode ,python-hideshow-exp)))))
      
(defun eepy/toggle-hideshow (&optional arg)
  "Toggle `hs-minor-mode' for current buffer.

If called with t or any positive number, turn on `hs-minor-mode';
if called with any negative number, turn if off;
otherwise, turn it on according to `eepy/enable-hideshow'."
  (interactive "p")
  (if (or (and arg (not (integerp arg)))  ;; t
          (and (integerp arg) (> arg 0))  ;; >0
          (and (not arg) eepy/enable-hideshow))
      (if (and (require 'hideshowvis nil t)
               (require 'hideshow-fringe nil t))
          (hideshowvis-enable)
        (hs-minor-mode t))
    (when (and arg (< arg 0))
      (if (fboundp 'hideshowvis-minor-mode)
          (hideshowvis-minor-mode -1))
      (hs-minor-mode -1))))

(add-hook 'python-mode-hook 'eepy/toggle-hideshow 'append)

;;*** outline
(autoload 'qtmstr-outline-mode "qtmstr-outline"
  "TODO" t)

(defcustom eepy/enable-outline nil
  "Whether to enable `outline-minor-mode' for code folding by default."
  :group 'eepy)

(defun eepy/toggle-outline (&optional arg)
  "Toggle `outline-minor-mode' for current buffer.

If called with t or any positive number, turn on `outline-minor-mode';
if called with any negative number, turn if off;
otherwise, turn it on according to `eepy/enable-outline'."
  (interactive "p")
  (if (or (and arg (not (integerp arg)))  ;; t
          (and (integerp arg) (> arg 0))  ;; >0
          (and (not arg) eepy/enable-outline))
      (if (require 'qtmstr-outline nil t)
          (qtmstr-outline-mode t)
        (outline-minor-mode t))
    (when (and arg (< arg 0))
      (if (fboundp 'qtmstr-outline-mode)
          (qtmstr-outline-mode -1)
      (hs-minor-mode -1)))))
               
;;(add-hook 'python-mode-hook 'eepy/toggle-outline 'append)


;;*** highlighting something
(autoload 'pretty-lambda-mode "pretty-lambdada"
  "Buffer-local minor mode to display the word `lambda' as the Greek letter." t)
(autoload 'highlight-indentation  "highlight-indentation"
  "Toggle highlight indentation." t)


;;*** completion
(autoload 'smart-operator-mode  "smart-operator"
  "Insert operators with surrounding spaces smartly." t)

;; autopair
(setq autopair-autowrap t)
(autoload 'autopair-mode  "autopair"
  "Automagically pair braces and quotes like in TextMate." t)
(autoload 'autopair-global-mode "autopair"
  "Toggle Autopair mode in every possible buffer." t)

;; Matching parentheses for all languages and so on
(eval-after-load "autopair"
  `(progn
     ;;(autopair-global-mode t)
     
     ;; Fix for triple quotes in python
     (add-hook 'python-mode-hook
               #'(lambda ()
                   (setq autopair-handle-action-fns
                         (list #'autopair-default-handle-action
                               #'autopair-python-triple-quote-action))))
     ))


;;*** misc
(autoload 'iedit-mode "iedit"
  "Edit multiple regions with the same content simultaneously." t)

;;FIXME: iedit.el already has iedit-symbol-in-defun
(defun eepy/rename-symbol-in-defun ()
  "Enter `iedit-mode' to rename the symbol in current function, or exit it."
  (interactive)
  (let ( (symbol (thing-at-point 'symbol)) )
    ;;FIXME: judge the symbol type
    (if symbol
        (if (assq 'iedit-mode minor-mode-map-alist)
            (progn
              (iedit-mode -1)
              (widen))
          (narrow-to-defun)
          (iedit-mode t)
          (message "When done, run `eepy/rename-symbol-in-defun' again to quit."))
      (message "You need to put cursor on an identifier."))))


(provide 'eepy-misc)
