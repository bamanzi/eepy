(load-file "~/.emacs.d/site-lisp/site-start.el")

(if (load-library "color-theme-zenburn")
    (color-theme-zenburn))


(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

(require 'eepy)
