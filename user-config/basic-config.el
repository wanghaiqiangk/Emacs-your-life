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


;;; basic settings for use-package
;;
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'vlf-setup)

;;; Enable key-chord
;;
;; (use-package use-package-chords
;;   :config (key-chord-mode 1))
(menu-bar-mode -1)
(column-number-mode 1)
(global-display-fill-column-indicator-mode 1)

;;; GUI basic settings
;;
(use-package emacs
  :if (display-graphic-p)
  :custom
  ;; (inhibit-startup-screen t "No startup screen")
  (tool-bar-mode nil "No tool bar at the top of window")
  (scroll-bar-mode nil "No scroll bar")
  :custom-face
  (default ((t (:height 180))))
  ;; Font is Monospace Regular
  (default ((t (:family "DejaVu Sans Mono"))))
  :config
  (server-start)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (load-theme 'solarized-light t)
  (mouse-avoidance-mode 'exile)
  (setq mouse-yank-at-point t)

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


;;;
;;
(setq require-final-newline t)


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


;;; Disable backup
;;
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "./.emacs.bak")))

;;; Display line number
;;
(use-package display-line-numbers
  :if (>= emacs-major-version 26)
  :hook (find-file . (lambda () (display-line-numbers-mode 1)))
  :config
  (set-face-background 'line-number-current-line "yellow")
  (set-face-foreground 'line-number-current-line "black"))

(use-package linum
  :if (<= emacs-major-version 25)
  :hook (find-file . (lambda () (global-linum-mode 1))))

;;; Enable clipboard
;;
(use-package xclip
  :init
  (delete-selection-mode)
  (setq select-enable-clipboard t)
  :config
  (xclip-mode 1))

;;; For paren
;;
(electric-pair-mode 1)
(show-paren-mode 1)

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
  (global-undo-tree-mode))

;;; change window
;;
(use-package ace-window
  :bind ("C-x o" . ace-window))

;;; trailing whitespace
;;
;; (setq show-trailing-whitespace t)

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

(use-package vterm
  :bind ([f1] . vterm))

;;; Whitespace newline character
;;
(use-package whitespace
  :config
  (global-whitespace-newline-mode 1)
  (progn
    (setq whitespace-style (quote (face tabs trailing tab-mark newline newline-mark)))
    (setq whitespace-display-mappings
          ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
          '(
            ;; (newline-mark 10 [10])
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
  :config
  (bind-key "C-." 'isearch-forward-symbol-at-point))

;;; avy quick locater, oralternatively use ace-jump-mode
;;
(use-package ace-jump-mode
  :custom
  (ace-jump-mode-submode-list '(ace-jump-char-mode
                                ace-jump-word-mode
                                ace-jump-line-mode))
  :bind
  ("C-c 9" . ace-jump-mode)
  ("s-f" . ace-jump-mode))
;; (global-set-key (kbd "C-c a w") #'avy-goto-char)
;; (global-set-key (kbd "C-c a l") #'avy-goto-line)


;;; Ivy, counsel, and swiper for emacs completion, function, and search
;;
(use-package counsel
  :if (or (not (display-graphic-p))
          (not prefer-graphical-helm))
  :bind
  (("C-x C-b" . counsel-ibuffer)
   ("C-c m" . counsel-semantic-or-imenu)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h b" . counsel-descbinds)
   ("C-h a" . counsel-apropos)))
;; Install function-args and activate it,
;; which makes semantic-or-imenu better,
;; or use moo-jump-local instead
(use-package function-args
  :config
  (fa-config-default)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (set-default 'semantic-case-fold t)
  ;; NOTE Add the backslash before M,
  ;; or don't take effect
  (define-key function-args-mode-map "\M-u" nil))


;;; Keybindings --------------------
;;
(use-package smart-replace
  :bind ("C-c C-r" . smart-replace-mode))
;; Fuzzy isearch
(use-package flx-isearch
  :bind (([remap isearch-forward-regexp] . flx-isearch-forward)
         ([remap isearch-backward-regexp] . flx-isearch-backward)))
;; key-chord
;; (key-chord-define-global "gg" 'goto-line)

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
  :bind (("C-c a m" . ace-mc-add-multiple-cursors)
         ("s-c" . ace-mc-add-multiple-cursors)))

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

;;; rgrep
(use-package grep
  :config
  (add-to-list 'grep-files-aliases '("chp" . "*.[ch] *.[ch]pp")))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  ;; (setq dumb-jump-quiet t)
  )

(use-package anzu
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold))

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))


;; (require 'doremi)
;; (require 'doremi-cmd)


(provide 'basic-config)

;;; basic-config.el ends here
