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
  (setq org-special-ctrl-k nil)
  (setq org-startup-indented t)
  ;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;; (add-hook 'org-mode-hook #'visual-line-mode)
  )

(defun my/open-my-agenda-file ()
  "Open agenda file."
  (interactive)
  (find-file "~/.emacs.d/org/agenda.org"))

(global-set-key (kbd "C-c a g") 'my/open-my-agenda-file)

(eval-after-load "org"
  '(my/org-settings))

(provide 'org-config)

;;; org-config.el ends here
