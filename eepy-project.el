;;; eepy-project.el --- project support of EEPY suite

;; This file is part of Enhaneced Emacs for PYthon suite
;;   http://github.com/bamanzi/eepy

;; Copyright (C) 2012 Ba Manzi <bamanzi@gmail.com>
;; This file is distributed under GPL v2.



;;** ropemacs project
(require 'eepy-ropemacs)


;;** eproject


;;** projectile
(autoload 'projectile-mode "projectile"
  "Minor mode to assist project management and navigation." t)
(autoload 'projectile-global-mode "projectile"
  "Toggle Projectile mode in every possible buffer." t)


(defun projectile-create-project (dir)
  "Create a projectile project (actually create an empty file named .projectile)."
  (interactive "Dproject root dir:")
  (require 'projectile)
  (let ((prj-file (concat dir ".projectile")))
    (unless (file-exists-p prj-file)
      (with-temp-buffer
        (insert ";; This file is meant for projectile project.")
        (write-file prj-file))))
  (if (string-prefix-p dir (buffer-file-name))
      (projectile-on)))

(defun projectile-show-project-info ()
  "Show project info of current project."
  (interactive)
  (if projectile-mode
      (if (projectile-get-project-root)
          (message "Project: %s. Root dir: %s. Type: %s"
                   (propertize (projectile-get-project-name) :bold t)
                   (projectile-get-project-root)
                   (loop for file in projectile-project-root-files
                         when (locate-dominating-file default-directory file)
                         do (return file)))
        (message "No project found along the path of current file."))
    (message "projectile-mode not turned on.")))

(eval-after-load "projectile"
  `(progn
     (define-key projectile-mode-menu [projectile-create-project]
       '(menu-item "Create project..." projectile-create-project
                   :help "Create a projectile project"))

     (define-key projectile-mode-menu [projectile-show-project-info]
       '(menu-item "Show project Info" projectile-show-project-info
                   :help "Show project root dir etc"))
     ))


(provide 'eepy-project)
