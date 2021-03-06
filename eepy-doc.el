;;; eepy-doc.el --- python document querying 

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.

(require 'eepy-init)

;;** Info - `info-lookup-symbol' (C-h S)
;; For python <= 2.5, info document for python is included in the python
;; release packages. 

;; But with python 2.6's switching to sphinx as documentation tool, the texinfo
;; document no longer provided with python official releases.
;;
;; You can generate texinfo documents following these info:
;;  http://stackoverflow.com/questions/1054903/how-do-you-get-python-documentation-in-texinfo-info-format
;;  http://bitbucket.org/jonwaltman/sphinx-info
;;  http://bitbucket.org/jonwaltman/rst2texinfo/src

;;*** pydoc-info: Search and browse the Python documentation in Info
;;  https://bitbucket.org/jonwaltman/pydoc-info/
;;advantages:
;;   1. info document for python-2.7 already provided with pydoc-info
;;   2. you can add other sphinx-based documents to lookup
;;disadvantages:
;;   1. for python versions other than 2.7, you need to generate info document by yourself

(eval-after-load "pydoc-info"
  `(progn
     (add-to-list 'Info-default-directory-list (concat eepy-install-dir "info/"))
     (add-to-list 'Info-directory-list (concat eepy-install-dir "info/"))

     ;;then use C-h S (`info-lookup-symbol') to lookup python doc

     ;;FIXME: not tested
     (when nil
     ;;for python <= 2.5
       (info-lookup-add-help
        :mode 'python-mode
        :parse-rule 'pydoc-info-python-symbol-at-point
        :doc-spec
        '(("(python2.5)Index" pydoc-info-lookup-transform-entry)
          ("(sphinx)Index" pydoc-info-lookup-transform-entry))))
     ))

;;(require 'pydoc-info)

;;*** another way, based on code stolen from Dave Love's python.el
;;
;; But if you're lazy, you can force to use python-2.5's info file
;;   http://packages.debian.org/squeeze/python2.5-doc
(defvar eepy-python-info-force-version nil
  "The version number used to look for pythonX.X-{ref,lib}.info file.

This is just useful when the version number of your python installation
is different from the one of your python info docs.")


(defun eepy-init-python-info-look ()
  "Set up info-look for Python.
Tries to take account of versioned Python Info files, e.g. Debian's
python2.5-ref.info.gz.
Used with `eval-after-load'."
  (let* ((py-version (let ((s
                            (shell-command-to-string (concat python-command " -V"))))
                       (string-match "^Python \\([0-9]+\\.[0-9]+\\>\\)" s)
                       (match-string 1 s)))
         (version (or eepy-python-info-force-version py-version))
         ;; Whether info files have a Python version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (Info-mode)
            ;; First look for Info files corresponding to the version
            ;; of the interpreter we're running.
            (condition-case ()
                ;; Don't use `info' because it would pop-up a *info* buffer.
                (progn
                  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error
               ;; Otherwise see if we actually have an un-versioned one.
               (condition-case ()
                   (progn
                     (Info-goto-node
                      (format "(python-lib)Miscellaneous Index" version))
                     nil)
                 (error
                  ;; Otherwise look for any versioned Info file.
                  (condition-case ()
                      (let (found)
                        (dolist (dir (or Info-directory-list
                                         Info-default-directory-list))
                          (unless found
                            (let ((file (car (file-expand-wildcards
                                              (expand-file-name "python*-lib*"
                                                                dir)))))
                              (if (and file
                                       (string-match
                                        "\\<python\\([0-9]+\\.[0-9]+\\>\\)-"
                                        file))
                                  (setq version (match-string 1 file)
                                        found t)))))
                        found)
                    (error)))))))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
	 ;; The empty prefix just gets us highlighted terms.
	 `((,(concat "(python" version "-ref)Miscellaneous Index"))
	   (,(concat "(python" version "-ref)Module Index"))
	   (,(concat "(python" version "-ref)Function-Method-Variable Index"))
	   (,(concat "(python" version "-ref)Class-Exception-Object Index"))
	   (,(concat "(python" version "-lib)Module Index"))
	   (,(concat "(python" version "-lib)Class-Exception-Object Index"))
	   (,(concat "(python" version "-lib)Function-Method-Variable Index"))
	   (,(concat "(python" version "-lib)Miscellaneous Index")))
       '(("(python-ref)Miscellaneous Index")
	 ("(python-ref)Module Index")
	 ("(python-ref)Function-Method-Variable Index")
	 ("(python-ref)Class-Exception-Object Index")
	 ("(python-lib)Module Index")
	 ("(python-lib)Class-Exception-Object Index")
	 ("(python-lib)Function-Method-Variable Index")
	 ("(python-lib)Miscellaneous Index"))))))

(if eepy-python-info-force-version
    (eepy-init-python-info-look)
  (require 'pydoc-info))


;;** CHM (only available on Windows)
;;TODO: try to auto-detect (or get from registry)
(defcustom eepy-python-chm-file-path "c:/python27/doc/python272.chm"
  "Full path of pythonXXx.chm"
  :group 'eepy
  :type 'filename)

;;NOTE: You need to install `keyhh' utility
;; http://www.keyworks.net/keyhh.htm
;;KeyHH -MyHelp -#klink "ActiveX Control Wizard" htmlhelp.chm
(defun chm-lookup-keyword (typeid help-file symbol)
  "lookup a keyword in a CHM file and display it"
  (interactive)
  (start-process "CHM keyword lookup" nil
                 "keyhh.exe"
                 (concat "-" typeid) 
                 "-#klink" (format "'%s'" symbol)
                 help-file ))

(defun chm-open-page (typeid help-file &optional page)
  "Launch CHM file."
  (start-process "keyhh" nil
                 "keyhh.exe"
                 (concat "-" typeid)
                 help-file
                 (if page
                     (concat "::" page))))

(defun eepy-chm-lookup-keyword (symbol)
  (interactive
   (list (read-string "Help on symbol(CHM): "
                      (or (thing-at-point 'symbol) ""))))
  (if (file-exists-p epy-python-chm-file-path)
      (chm-lookup-keyword "EEPY" eepy-python-chm-file-path symbol)
   (message "File not exists: %s. Please customize variable `eepy-python-chm-file-path'."
            epy-python-chm-file-path)))

(defun eepy-chm-open-page (path)
  (if (file-exists-p epy-python-chm-file-path)  
      (chm-open-page "EEPY" eepy-python-chm-file-path path)
   (message "File not exists: %s. Please customize variable `eepy-python-chm-file-path'."
            epy-python-chm-file-path)))

;;** pylookup
;;TODO: http://taesoo.org/Opensource/Pylookup


;;** haddoc
;;haddoc: Browse HTML Python Documentation From Emacs
;;TODO: http://furius.ca/haddoc/


;;** pydoc command line
;;stolen from http://stackoverflow.com/a/1068731
(defun eepy-pydoc (&optional arg)
  (interactive (list
				(read-string "Call pydoc with arg: "
							 (with-syntax-table python-dotty-syntax-table
							   (current-word)))))
  (setq cmd (concat "pydoc " arg))
  (ad-activate-regexp "auto-compile-yes-or-no-p-always-yes")
  (shell-command cmd)
  (setq pydoc-buf (get-buffer "*Shell Command Output*"))
  ;;(switch-to-buffer-other-window pydoc-buf)
  (with-current-buffer pydoc-buf
    ;; (python-mode)
    (require 'woman)
    (woman-man-buffer)
    (help-mode))    
  (ad-deactivate-regexp "auto-compile-yes-or-no-p-always-yes")
)

(provide 'eepy-doc)
