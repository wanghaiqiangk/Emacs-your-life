;;; basic-config.el --- basic configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  The settings shown below affect how Emacs itself works.
;;  For examples, the behaviors of functions, the appearances
;;  how Emacs looks like, common keybindings for convenience, etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;; GUI basic settings
;;
(if (display-graphic-p)
    (progn
      (setq inhibit-startup-screen t)
      (set-face-attribute 'default nil :height 140)
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      ))


;;; Encoding
;;
(define-coding-system-alias 'UTF-8 'utf-8)

;;; smex
;;
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; ido
;;
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;; Disable backup
;;
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "./.emacs.bak")))

;;; Display line number
;;
(add-hook 'find-file-hook (lambda () (display-line-numbers-mode 1))) ; Emacs version is required as 26.x
(set-face-background 'line-number-current-line "yellow")
(set-face-foreground 'line-number-current-line "black")

;;; Enable clipboard
;;
(setq select-enable-clipboard t)
(xclip-mode 1)

;;; For paren
;;
(electric-pair-mode 1)
(show-paren-mode 1)

;;; Show current line after scrolling
;;
(beacon-mode 1)

;;; Undo
;;
(global-undo-tree-mode)

;;; change window
;;
(global-set-key (kbd "C-x o") 'ace-window)

;;; trailing whitespace
;;
(setq-default show-trailing-whitespace t)

;;; shell in emacs
;;
(setq shell-file-name "/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

;;; Whitespace newline character
;;
(global-whitespace-newline-mode 1)
(progn
  (setq whitespace-style (quote (tabs tab-mark newline newline-mark)))
  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “↦” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (newline-mark 10 [182 10])
          (tab-mark 9 [8614 9] [92 9])
          )))

(if (display-graphic-p)
    (progn
      (set-face-foreground 'whitespace-newline "#cccccc")
      (set-face-foreground 'whitespace-tab "#cccccc")
      (set-face-background 'whitespace-tab 'unspecified))
  (progn
    (set-face-foreground 'whitespace-newline "#cd00cd")
    (set-face-foreground 'whitespace-tab "#cd00cd")
    (set-face-background 'whitespace-tab 'unspecified)))

;;; neotree is a file navigator
;;
(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(set-face-attribute 'neo-file-link-face t :foreground "white")
(setq neo-smart-open t)

;;; Search and highlight
;;
(eval-after-load "isearch" '(require 'isearch+))
(setq isearch-lazy-highlight 'all-windows)
(setq lazy-highlight-buffer t)
(setq lazy-highlight-cleanup nil)
(setq lazy-highlight-max-at-a-time nil)
(setq lazy-highlight-initial-delay 0)

;;; avy quick locater
;; A alternative is ace-jump-mode
;;
(global-set-key (kbd "C-c a w") #'avy-goto-char)
(global-set-key (kbd "C-c a l") #'avy-goto-line)

;;; Ivy, counsel, and swiper
;;
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(global-set-key (kbd "C-c m") 'counsel-semantic-or-imenu)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h b") 'counsel-descbinds)
;; Install function-args and activate it,
;; which makes semantic-or-imenu better,
;; or use moo-jump-local instead
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)
;; NOTE Add the backslash before M,
;; or don't take effect
(define-key function-args-mode-map "\M-u" nil)

;;; Keybindings --------------------
;;
(global-set-key (kbd "C-c C-r") 'replace-regexp)
(global-set-key (kbd "C-c r") 'replace-string)
;; Fuzzy isearch
(global-set-key (kbd "C-c f s") 'flx-isearch-forward)
(global-set-key (kbd "C-c f r") 'flx-isearch-backward)
(global-set-key (kbd "C-c h m") 'helm-man-woman)

;;; Tramp bug maybe
;;
(defun ido-remove-tramp-from-cache nil
  "Remove any TRAMP entries from `ido-dir-file-cache'.
This stops tramp from trying to connect to remote hosts on Emacs startup,
which can be very annoying."
  (interactive)
  (setq ido-dir-file-cache
        (cl-remove-if
         (lambda (x)
           (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
         ido-dir-file-cache)))
;; redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved
(defun ido-kill-emacs-hook ()
  (ido-remove-tramp-from-cache)
  (ido-save-history))

;;; Multiple cursors
;;
(autoload 'ace-mc-add-multiple-cursors "ace-mc"
  "Auto load ace-mc-add-multiple-cursors function from ace-mc package, which works similarily as ace-jump.
The default behavior is to query beginning char for word. With prefix C-u, the query is changed for any char. With prefix C-u C-u, the query is changed for line.")
(global-set-key (kbd "C-c a m") 'ace-mc-add-multiple-cursors)

;;; Iedit mode
;;
(autoload 'iedit-mode "iedit"
  "Auto load iedit-mode function from iedit package.
However, it seems to be a little complicated. Learn it by using.")
(global-set-key (kbd "C-c i") 'iedit-mode)

(provide 'basic-config)

;;; basic-config.el ends here
