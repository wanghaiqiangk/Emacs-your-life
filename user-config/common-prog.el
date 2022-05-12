;;; common-prog.el --- Configuration for programming
;;; Commentary:
;;; Code:

;;; Code style
;;
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

(setq-default tab-width 4)

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

(require 'clang-format)
;;; Highlight indentation
;;
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

;; Derived from https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter
  :ensure t
  :config
  (when (bound-and-true-p linum-mode)
    (git-gutter:linum-setup))
  (global-git-gutter-mode t)
  (setq git-gutter:update-interval 0.3)
  :bind
  (("C-x v [" . 'git-gutter:previous-hunk)
   ("C-x v ]" . 'git-gutter:next-hunk)
   ("C-x v c" . 'git-gutter:cancel-update-timer)
   ([remap vc-create-tag] . 'git-gutter:start-update-timer)
   ([remap vc-retrieve-tag] . 'git-gutter:set-start-revision)))

(use-package git-gutter-fringe
  :ensure t
  :config
  (set-face-foreground 'git-gutter-fr:added "green")
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240 248 252] nil nil 'bottom))


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
;; (use-package projectile
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :config
;;   (projectile-mode +1))


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
;; (use-package semantic
;;   :defines semantic-idle-scheduler-idle-time
;;   :config
;;   (semantic-mode -1)
;;   (global-semantic-idle-scheduler-mode t)
;;   (setq semantic-idle-scheduler-idle-time 1)
;;   (global-semantic-idle-summary-mode 0))

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

(use-package xref
  :config
  (substitute-key-definition 'xref-next-line 'xref-next-line-no-show xref--xref-buffer-mode-map)
  (substitute-key-definition 'xref-prev-line 'xref-prev-line-no-show xref--xref-buffer-mode-map))

(defvar wang/switch-arglist-cont-nonempty-lineup
  [(c-lineup-gcc-asm-reg c-lineup-arglist) (+)])

(defun wang/switch-arglist-cont-nonempty-lineup ()
  (interactive)
  (let ((old (assoc 'arglist-cont-nonempty c-offsets-alist))
        (old-lineup nil))
    (if (not old)
        (message "arglist-cont-nonempty is not properly defined.")
      (seq-doseq (lineup wang/switch-arglist-cont-nonempty-lineup)
        (if (equal (cdr old) lineup)
            (setq old-lineup lineup)))
      (if (not old-lineup)
          (message "arglist-cont-nonempty cannot be changed.")
        (seq-doseq (lineup wang/switch-arglist-cont-nonempty-lineup)
          (if (not (equal lineup old-lineup))
              (progn
                (c-set-offset 'arglist-cont-nonempty lineup)
                (message "arglist-cont-nonempty is set to %s." lineup))))))))


(provide 'common-prog)

;;; common-prog.el ends here
