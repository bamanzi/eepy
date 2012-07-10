
(require 'anything)
(require 'projectile)

;; helper for anything
(defun anything-c-projectile-list ()
  "Generates a list of files in the current project"
  (projectile-get-project-files
   (projectile-get-project-root)))

(defvar anything-c-source-projectile-list
  `((name . "Projectile Files")
    (candidates . anything-c-projectile-list)
    (type . file)
    (mode-line . "Projectile Files")
    )
  "Anything source definition")

(defun anything-with-projectile-list ()
  "Example function for calling anything with the projectile file source.

Use this function as example and create your own list of anything sources.
"
  (interactive)
  (anything :sources '(anything-c-source-projectile-list)))


(provide 'anything-projectile)
