;;; common-prog.el --- Configuration for programming
;;; Commentary:
;;; Code:

;;; Code style
;;
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

;;; Default bsd style use 8 offset, I use 4 offset
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Indent rigidly like vim << or >>
;;
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

(defun set-auto-indentation-offset ()
  "My prefer indentation style."
  (setq c-basic-offset 4)
  (c-set-offset 'inlambda 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'template-args-cont 0)
  (c-set-offset 'inextern-lang 0))
(add-hook 'c-mode-common-hook 'set-auto-indentation-offset)
(add-hook 'c-mode-hook 'set-auto-indentation-offset)
(add-hook 'c++-mode-hook 'set-auto-indentation-offset)

;; Highlight indentation
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :diminish
  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  :config
  (if (display-graphic-p)
      (progn
        (set-face-background 'highlight-indent-guides-odd-face "#eddfba")
        (set-face-background 'highlight-indent-guides-even-face "#eddfba")
        (set-face-background 'highlight-indent-guides-top-odd-face "#00ff7f")
        (set-face-background 'highlight-indent-guides-top-even-face "#00ff7f"))
    (progn
      (set-face-background 'highlight-indent-guides-odd-face "#cd00cd")
      (set-face-background 'highlight-indent-guides-even-face "#cd00cd")
      (set-face-background 'highlight-indent-guides-top-odd-face "#00cdcd")
      (set-face-background 'highlight-indent-guides-top-even-face "#00cdcd"))))


;;; git client
;;
(use-package magit
  :bind ("C-x g" . magit-status))

;;; diff-hl
;;
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (if (not (display-graphic-p))
      (diff-hl-margin-mode t)))


;;; yasnippet configuration
;;
(use-package yasnippet
  :commands (yas-expand)
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all nil))


;;; Highlight todo keywords in comment
;;
(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode))


;;; Idel highlight symbols
;;
;; (add-hook 'prog-mode-hook 'idle-highlight-mode)


;;; Rainbow-delimiters
;;
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;; Zeal Plugin
;;
;; (global-set-key (kbd "C-c z .") 'zeal-at-point)


;;; projectile is a project manager, like any IDE does
;;
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))


;;; Backward delete word
;;
(defun custom/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun custom/backward-delete-word (arg)
  "Delete characters backward until encountering the begnning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (custom/delete-word (- arg)))

(global-set-key (kbd "M-DEL") 'custom/backward-delete-word)


;;; Semantics Setup
;;
(use-package semantic
  :defines semantic-idle-scheduler-idle-time
  :config
  (semantic-mode 1)
  (global-semantic-idle-scheduler-mode t)
  (setq semantic-idle-scheduler-idle-time 1)
  (global-semantic-idle-summary-mode 0))


(provide 'common-prog)

;;; common-prog.el ends here
