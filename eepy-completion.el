;;; eepy-completion.el -- code completion for EEPY suite

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.

(require 'eepy-init)


;;** auto-complete
(autoload 'auto-complete-mode  "auto-complete"
  "AutoComplete mode" t)
(autoload 'global-auto-complete-mode  "auto-complete"
  "Toggle Auto-Complete mode in every possible buffer." t)

(eval-after-load "auto-complete"
  `(progn
     (unless (fboundp 'ac-config-default)  ;;if user not loaded `auto-complete' before this
       (load "auto-complete-config")
       (setq ac-dwim t)
       (ac-config-default))

     (add-to-list 'ac-dictionary-directories
                  (concat eepy-install-dir "extensions/auto-complete/dict/"))
     ))

(require 'auto-complete)

;;a helper command
(defun eepy/toggle-ac-source (source &optional desire)
  "Add or remove a SOURCE in `ac-sources'.

If DESIRE given, this source would be absolutely added (if DESIRE > 0) or
remove (if DESIRE <= 0). If DESIRE not given, it would be toggled."
  (interactive
   (list (intern-soft (ido-completing-read "Source: "
										   (loop for x being the symbols
												 if (and (boundp x)
														 (string-match "^ac-source-" (symbol-name x)))
												 collect (symbol-name x))))))
  (when (and source (symbolp source))
	(if desire
		(if (> desire 0)
			(add-to-list 'ac-sources source)
		  (setq ac-sources (remq source ac-sources)))
	  (if (memq source ac-sources)
		  (setq ac-sources (remq source ac-sources))
		(add-to-list 'ac-sources source)))
	(message "Source `%s' %s." source (if (memq source ac-sources)
										  "enabled"
										"disabled"))))


(defcustom eepy-auto-complete-sources
  '(
    ac-source-python-builtin
    ac-source-pycompletemine
    ;;ac-source-scite-api
    ;;ac-source-yasnippet
    ;;ac-source-ipython
    )
  "Default additional auto-completion sources for python-mode
(besides `ac-sources' default value).

You don't need add `ac-source-nropemacs' into this, as it's due to add
by ropeproject hook."
  :group 'eepy)

(defun eepy-python-mode-init-auto-complete ()
  (mapc #'(lambda (source)
            (add-to-list 'ac-sources source))
        eepy-auto-complete-sources))

(add-hook 'python-mode-hook 'eepy-python-mode-init-auto-complete)  
  
;;*** Emacs's built-in completion
;;advantages:
;;      + no other libraries needed
;;      + with 'send region' to inferior python process, you can get more completions
;;        e.g. if you send
;;               foo = re.compile("^(defcustom eepy-")
;;        to inferior python process, then you can get completions for 'foo'
;;disadvantages:
;;      - no doc info, nor function signature
;;      - in order to get completions, you need to send some python code to inferior python

(defun python-symbol-completions-maybe (prefix)
  (let ((python-el (symbol-file major-mode)))
    (if (string-match "lisp/progmodes/python.el" python-el) ;;Emacs builtin python.el
        (python-symbol-completions prefix)
      nil) ;;otherwise, return nil
    ))

(ac-define-source python-builtin
  '( (candidates . (python-symbol-completions-maybe ac-prefix))
     (symbol . "py")
     (prefix . "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=") ))

;; (defun ac-enable-python-builtin-source (&optional on)
;;   (interactive)
;;   (let ((turn-on (or on
;;                      (not (memq ac-source-python-builtin ac-sources)))))
;;     (if turn-on
;;         (add-to-list 'ac-sources 'ac-source-python-builtin)
;;       (setq ac-sources (remq ac-source-python-builtin ac-sources)))))


;;*** pycompletemine from PDEE (https://github.com/pdee/pdee/ )
;; You need `pycompletemine.{el,py}' from PDEE and pymacs
;;advantages:
;;   + differ from `pycomplete', this one would work on both python-mode.el
;;     and GNU Emacs built-in python.el (improved by PDEE and EEPY)
;;   + doc info and signature for completions
;;disadvantages:
;;   - `pymacs' needed
;;   - no 'send region' support, thus no completion for dynamic object

(autoload 'py-complete "pycompletemine" nil t)
(define-key python-mode-map (kbd "ESC M-TAB") 'py-complete)

(ac-define-source pycompletemine
  '((depends pycompletemine)  ;;FIXME: ok?
    (prefix .  "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=")
    (candidates . (pycomplete-get-all-completions-for-ac ac-prefix))
    (symbol . "pyc")
    (document . py-complete-help)))

;; (defun ac-enable-pycompletemine-source (&optional on)
;;   (interactive)
;;   (let ((turn-on (or on
;;                      (not (memq ac-source-pycompletemine ac-sources)))))
;;     (if turn-on
;;         (add-to-list 'ac-sources 'ac-source-pycompletemine)
;;       (setq ac-sources (remq ac-source-pycompletemine ac-sources)))))

;;*** ipython (only python-mode.el supported)
;;TODO: not tested yet
;;stolen from https://bitbucket.org/tavisrudd/emacs.d/src/tip/dss-completion.el

;;; partially working support for using auto complete in ipython buffers
(defun dss-ipython-completion-candidate (&optional use-ido)
  "This is a hacked version of ipython-complete from ipython.el,
    which can be used with either autocomplete-mode or ido.

    It mostly works but there are a few bugs that need resolving...

(defun dss/start-ipy-complete ()
  (interactive)
  (setq ac-sources '(ac-source-dss-ipy-dot
                     ac-source-dss-ipy
                     ac-source-filename)))
(add-hook 'ipython-shell-hook 'dss/start-ipy-complete)
(add-hook 'py-shell-hook 'dss/start-ipy-complete)

"
  (let* ((ugly-return nil)
         (sep ";")
         (python-process (or (get-buffer-process (current-buffer))
                                        ;XXX hack for .py buffers
                             (get-process py-which-bufname)))
         (beg (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                               (point)))
         (end (point))
         (pattern (buffer-substring-no-properties beg end))

         (completions nil)
         (completion nil)
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      "")))))

    (message pattern)
    (process-send-string python-process
                         (format ipython-completion-command-string pattern))
    (accept-process-output python-process)
    (setq completions
          (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))

    (setq completions (if (string-match "\\." pattern)
                          (mapcar
                             (lambda (completion)
                               (car (last (cdr (split-string completion "\\.")))))
                             completions)
                        completions))
    (if use-ido
        (let* ((prefix-beg (if (string-match "\\." pattern)
                               (save-excursion (skip-chars-backward "a-z0-9A-Z_" (point-at-bol))
                                               (point))
                             beg))
               (prefix (buffer-substring-no-properties prefix-beg end))
               (choice (if (<= (length completions) 1)
                           (car completions)
                         (ido-completing-read "Choice:" completions nil nil prefix nil prefix)))
               )
          (if (and choice (not (string= pattern choice)))
              (progn
                (message "%s %s %s %s" prefix prefix-beg beg (point-at-bol))
                (delete-region prefix-beg end)
                (insert choice))))
      (progn
        ;(message "not using ido")
        completions))))


(defun dss/ido-ipython-complete ()
  (interactive)
  (dss-ipython-completion-candidate t))

(eval-after-load "python-mode"
  `(progn
     (require 'ipython)

     (ac-define-source dss-ipy
       '((candidates . dss-ipython-completion-candidate)
         (requires . 0)
         (symbol . "f")))

     (ac-define-source dss-ipy-dot
       '((candidates . dss-ipython-completion-candidate)
         (prefix . c-dot)
         (requires . 0)
         (symbol . "f")))
     ))


;;*** auto-complete-scite-api
;;TODO: ...
(if (require 'auto-complete-scite-api nil t)
    (add-to-list 'ac-scite-api-directories (concat eepy-install-dir "etc")))


;;** ropemacs: (code completion (and other features) for project
(require 'eepy-ropemacs)

;;** yasnippets

(autoload 'yas/minor-mode  "yasnippet-bundle"
  "Toggle YASnippet mode." t)
(autoload 'yas/global-mode "yasnippet-bundle"
  "Toggle Yas/Minor mode in every possible buffer." t)

;; Disabling Yasnippet completion 
(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates)
      )))

(defun epy-get-all-snips ()
  ;;https://github.com/mlf176f2/yas-jit.el
  (require 'yas-jit nil t) ;; FIXME: find a way to conditionally load it
  (if (featurep 'yasnippet)
      (let (candidates)
        (maphash
         (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
        (apply 'append candidates))
    ))

(eval-after-load "auto-complete"
  `(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))
  )


(provide 'eepy-completion)
