;;; org-config.el --- configuration for org mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun my/org-settings ()
  "Org pertinent settings."
  (interactive)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k nil))

(eval-after-load "org"
  '(my/org-settings))

(provide 'org-config)

;;; org-config.el ends here
