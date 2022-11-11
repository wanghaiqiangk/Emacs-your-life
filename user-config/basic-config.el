;;; basic-config.el --- customized basic configuration

;; Copyright © 2016-2022 Wang Haiqiang and contributors

;; Author: Wang Haiqiang
;; URL: https://github.com/wanghaiqiangk/Emacs-your-life
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;  The settings shown below affect how Emacs itself works.
;;  For examples, the behaviors of functions, the appearances
;;  how Emacs looks like, common keybindings for convenience, etc.

;;; Code:

(require 'use-package)
(require 'diminish)
(require 'bind-key)
(require 'vlf-setup)

;; https://git.sr.ht/~technomancy/better-defaults
(progn
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  ;; (inhibit-startup-screen t)

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  ;; (savehist-mode 1) ;; refer (use-package savehist) following
  (column-number-mode 1)
  (global-display-fill-column-indicator-mode 1)

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  (global-set-key (kbd "M-/") 'hippie-expand)

  (setq hippie-expand-try-functions-list-backup hippie-expand-try-functions-list
        hippie-expand-try-functions-list-without
        (remove 'try-expand-list hippie-expand-try-functions-list))
  (if t
      (setq hippie-expand-try-functions-list hippie-expand-try-functions-list-without)
    (setq hippie-expand-try-functions-list hippie-expand-try-functions-list-backup))

  (setq-default indent-tabs-mode nil)
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        backup-by-copying t
        custom-file (expand-file-name "custom.el" user-emacs-directory))

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups"))))))

;;; GUI basic settings
;;
(use-package emacs
  :if (display-graphic-p)
  :custom-face
  (default ((t (:height 180))))
  ;; Font is Monospace Regular
  (default ((t (:family "DejaVu Sans Mono"))))
  :config
  (server-start)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (load-theme 'solarized-light t)
  (mouse-avoidance-mode 'exile)

  ;; A common bug is that the Emacs is likely be unable to wake up after suspending.
  ;; Therefore, globally unset the pertinent keybindings to temporary obliterate this bug
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  )

(defgroup wemacs nil
  "My Emacs package."
  :prefix "wemacs-"
  :group 'tools)

(defcustom prefer-graphical-helm nil
  "Whether to use helm under graphcial Emacs. If not, use smex."
  :type 'boolean
  :group 'wemacs)

(defun prefer-graphical-helm-toggle ()
  "Toggle \"prefer-graphical-helm\"."
  (interactive)
  (setq prefer-graphical-helm (not prefer-graphical-helm))
  (message "The prefer-graphical-helm variable is now %s."
           (if prefer-graphical-helm "set" "unset")))

(global-set-key (kbd "C-c w h") 'prefer-graphical-helm-toggle)


;;; Encoding
;;
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(define-coding-system-alias 'UTF-8 'utf-8)


;;; Ease your M-x, using smex or helm
;;
;; For Helm
(when (and (display-graphic-p)
           prefer-graphical-helm)
  (require 'helm-config))

(use-package helm-mode
  :requires (helm-config)
  :config
  (helm-mode 1)
  (bind-key "M-x" 'helm-M-x)
  (bind-key "C-x C-f" 'helm-find-files)
  (bind-key "C-x b" 'helm-buffers-list)
  (bind-key "C-x C-b" 'helm-buffers-list)
  (bind-key "C-h a" 'helm-apropos))

(use-package helm
  :requires (helm-config)
  :custom
  (helm-input-idle-delay                     0.01)
  (helm-reuse-last-window-split-state        t)
  (helm-always-two-windows                   t)
  (helm-split-window-inside-p                nil)
  (helm-actions-inherit-frame-settings       t)
  (helm-use-frame-when-more-than-two-windows t)
  (helm-use-frame-when-dedicated-window      t)
  (helm-frame-background-color               "DarkSlateGray")
  (helm-show-action-window-other-window      'left)
  (helm-move-to-line-cycle-in-source         t)
  (helm-autoresize-max-height                80)
  (helm-autoresize-min-height                20)
  (helm-follow-mode-persistent               t)
  (helm-candidate-number-limit               100))

(use-package helm-lib
  :requires (helm-config)
  :config
  (setq helm-scroll-amount 1))

(use-package helm-descbinds
  :requires (helm-config)
  :config
  (helm-descbinds-mode 1)
  (setq helm-descbinds-window-style 'split-window))

(use-package helm-describe-modes
  :requires (helm-config)
  :bind ([remap describe-mode] . helm-describe-modes))

(use-package helm-adaptive
  :config
  (setq helm-adaptive-history-file nil)
  (helm-adaptive-mode 1))

(global-set-key (kbd "s-l") 'helm-locate)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)

;; For smex
(use-package smex
  :if (or (not (display-graphic-p))
          (not prefer-graphical-helm))
  :config
  (smex-initialize)
  (bind-key "M-x" 'smex))

;; Ido or helm, incrementing search and narrowing selection
(use-package ido
  :if (or (not (display-graphic-p))
          (not prefer-graphical-helm))
  :functions ido-everywhere
  :custom
  (ido-use-faces nil "To see flex highlights")
  (ido-enable-flex-matching t "For further fuzzy matching")
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package flx-ido
  :if (or (not (display-graphic-p))
          (not prefer-graphical-helm))
  :config
  (flx-ido-mode 1))

;;; Display line number
;;
(use-package display-line-numbers
  :if (>= emacs-major-version 26)
  :hook ((find-file after-change-major-mode) . (lambda () (display-line-numbers-mode 1)))
  :config
  (set-face-background 'line-number-current-line "yellow")
  (set-face-foreground 'line-number-current-line "black"))

(use-package linum
  :if (<= emacs-major-version 25)
  :hook ((find-file after-change-major-mode) . (lambda () (global-linum-mode 1))))

;;; Enable clipboard
;;
(use-package xclip
  :init
  (delete-selection-mode)
  (setq select-enable-clipboard t)
  :config
  (xclip-mode 1))

;;; Show current line after scrolling
;;
(use-package beacon
  :diminish
  :config
  (beacon-mode 1))

;;; Undo
;;
(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode)
  (progn
    (let ((undo-tree-history
           (concat user-emacs-directory "undo-tree-history")))
      (unless (file-directory-p undo-tree-history)
        (make-directory undo-tree-history))
      (setq undo-tree-history-directory-alist (list (cons "." undo-tree-history))))))

;;; change window
;;
(use-package ace-window
  :bind ("C-x o" . ace-window))

;;; shell in emacs
;;
(use-package ansi-color
  :hook (shell-mode . ansi-color-for-comint-mode-on))
(defun my/set-system-terminal-emulator ()
  "Start terminal emulator based on environment variable SHELL."
  (interactive)
  (ansi-term (replace-regexp-in-string "[ \t\n]*\\'" "" (shell-command-to-string "echo $SHELL"))))
(use-package term
  :bind ("s-t" . my/set-system-terminal-emulator))

;;; Whitespace newline character
;;
(use-package whitespace
  ;; :hook (before-save . whitespace-cleanup)
  :config
  ;; (global-whitespace-newline-mode 1)
  (global-whitespace-mode 1)
  (progn
    (setq whitespace-style (quote (face tabs trailing tab-mark)))
    (setq whitespace-display-mappings
          ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
          '(
            (tab-mark 9 [10155 9] [92 9])
            )))
  (if (display-graphic-p)
      (progn
        (set-face-foreground 'whitespace-newline "#eddfba")
        ;; (set-face-foreground 'whitespace-tab "#eddfba")
        ;; (set-face-background 'whitespace-tab 'unspecified)
        (set-face-inverse-video 'whitespace-tab nil)
        )
    (progn
      (set-face-foreground 'whitespace-newline "#cd00cd")
      (set-face-foreground 'whitespace-tab "#cd00cd")
      (set-face-background 'whitespace-tab 'unspecified))))

;;; neotree is a file navigator
;;
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :custom
  (neo-smart-open t)
  ;; :custom-face
  ;; (neo-file-link-face ((t (:foreground "black"))))
  :config
  (if (display-graphic-p)
      (set-face-foreground 'neo-file-link-face "black" t)
    (set-face-foreground 'neo-file-link-face "white" t)))

;;; Search and highlight
;;
(use-package isearch+
  :after (isearch)
  :custom
  (isearch-lazy-highlight 'all-windows)
  (lazy-highlight-buffer t)
  (lazy-highlight-cleanup nil)
  (lazy-highlight-max-at-a-time nil)
  (lazy-highlight-initial-delay 0)
  (isearch-lazy-count t)                ; this can replace anzu package
  (lazy-count-prefix-format "%s/%s ")
  :bind (("C-." . isearch-forward-symbol-at-point)))
(eval-after-load "isearch" '(require 'isearch+))

(defun wang/jump-to-char-one-more (query-char)
  "Move the cursor to one more position after the QUERY-CHAR."
  (interactive (list (read-char "Query Char:")))
  ;; possible APIs are provided by avy or ace-jump
  (avy-goto-char-in-line query-char)
  (forward-char 1))

(global-set-key (kbd "C-c f") 'wang/jump-to-char-one-more)


;;; Ivy, counsel, and swiper for emacs completion, function, and search
;;
(use-package counsel
  :if (or (not (display-graphic-p))
          (not prefer-graphical-helm))
  :bind
  (("C-c l" . counsel-list-processes)
   ("C-c m" . counsel-semantic-or-imenu)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h b" . counsel-descbinds)
   ("C-h a" . counsel-apropos)))


;;; Keybindings --------------------
;;
(use-package smart-replace
  :bind ("C-c C-r" . smart-replace-mode))

;;; Tramp bug maybe
;;
(with-eval-after-load "ido"
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

  (defun ido-kill-emacs-hook ()
    "Redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved."
    (ido-remove-tramp-from-cache)
    (ido-save-history)))

(use-package tramp
  :config
  (setq tramp-archive-enabled nil))


;;; Multiple cursors
;;
(use-package ace-mc
  :bind (("s-c" . ace-mc-add-multiple-cursors)))

;;; Iedit mode
;;
(use-package iedit
  :bind ("C-c i" . iedit-mode))

;;; Recent file functionality
;;
(use-package recentf
  :init (recentf-mode 1)
  :config
  (setq recentf-auto-cleanup 'never)
  :bind ("C-x C-r" . recentf-open-files))


;;; Save history after quiting emacs
;;
(use-package savehist
  :init (savehist-mode 1)
  :custom
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring)))

(use-package window-split
  :bind ("C-x |" . toggle-window-split))

(use-package find-file
  :bind ("C-c o" . ff-find-other-file))

(use-package syslog-mode
  :mode ("syslog\\(?:\\.[[:digit:]]\\)?\\'" . syslog-mode))

(use-package which-key
  :hook (after-init . which-key-mode))

;;; rg
(use-package rg
  :bind (("s-." . rg)
         ("C-c s" . rg-kill-current))
  :config
  (add-to-list 'rg-custom-type-aliases '("chp" . "*.[chH] *.[ch]pp *.cc *.C")))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  ;; (setq dumb-jump-quiet t)
  )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;;; directory variables
;;
(defun my/add-dir-local-variable (&optional prefix)
  "Without PREFIX add directory variables, remove otherwise."
  (interactive "p")
  (when (= prefix 1)
    (call-interactively 'add-dir-local-variable))
  (when (= prefix 4)
    (call-interactively 'delete-dir-local-variable)))

;; If one line is too long then native Emacs may have trouble to open
;; such file. In so long mode, Emacs will try to detect such case and
;; suspend some modes which may cause freezing.
(use-package so-long
  :config (global-so-long-mode 1))

;; If a file is opened in Emacs buffer and is changed on disk, Emacs
;; will automatically revert the buffer to retrieve latest contents.
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; When using CamelCase naming convertion, the word separated by
;; capital character is called subword in Emacs. If not superword mode
;; (default), then Emacs can correctly handle forwarding/backwarding
;; words based on CamelCase's style. This is called subword mode.
(use-package subword
  :hook (c-mode-common-hook . subword-mode))

(use-package vimish-fold
  :bind (("C-c @ f" . vimish-fold)
         ("C-c @ t" . vimish-fold-toggle)
         ("C-c @ d" . vimish-fold-delete)))

(defun wang/emacs-start-up-time ()
  (message "Emacs startup cost %.2f seconds"
           (string-to-number (car (split-string (emacs-init-time))))))

(add-hook 'emacs-startup-hook #'wang/emacs-start-up-time)

(defgroup wang nil
  "Wang's customization."
  :group 'convenience)

(defcustom wang-python3-binary-path nil
  "Path to python3 interpreter"
  :type '(string)
  :options '("/usr/bin/python3" "/usr/local/bin/python3")
  :group 'wang)

(defun wang/set-python3-binary-path (path)
  "Set the path to python3 interperter."
  (interactive "spython3 path: ")
  (if (and path
           (file-exists-p path))
      (customize-save-variable 'wang-python3-binary-path path)
    (error "%s is not found" path)))

(provide 'basic-config)

;;; basic-config.el ends here
