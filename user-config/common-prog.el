;;; common-prog.el --- Configuration for programming
;;; Commentary:
;;; Code:

;; Code style
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

;; Default bsd style use 8 offset, I use 4 offset
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun set-auto-indentation-offset ()
  (setq c-basic-offset 4)
  (c-set-offset 'inlambda 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'template-args-cont 0)
  (c-set-offset 'inextern-lang 0))
(add-hook 'c-mode-hook 'set-auto-indentation-offset)
(add-hook 'c++-mode-hook 'set-auto-indentation-offset)

;; Highlight indentation
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(setq highlight-indent-guides-method 'column)
(setq highlight-indent-guides-auto-enabled nil)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0)

(set-face-background 'highlight-indent-guides-odd-face "#cd00cd")
(set-face-background 'highlight-indent-guides-even-face "#cd00cd")
(set-face-background 'highlight-indent-guides-top-odd-face "#00cdcd")
(set-face-background 'highlight-indent-guides-top-even-face "#00cdcd")

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; diff-hl
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
(eval-after-load 'diff-hl
  '(diff-hl-margin-mode t))

;; yasnippet configuration
(require 'yasnippet)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(eval-after-load 'yasnippet
  '(yas-reload-all))

;; Highlight todo keywords in comment
(add-hook 'prog-mode-hook 'hl-todo-mode)

;; Idel highlight symbols
(add-hook 'prog-mode-hook 'idle-highlight-mode)

;; Rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Zeal Plugin
(global-set-key (kbd "C-c z .") 'zeal-at-point)

;; projectile is a project manager, like any IDE does
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(provide 'common-prog)

;;; common-prog.el ends here
