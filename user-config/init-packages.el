;;; init-packages.el --- package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; Manage package archives and installed packages
;;; When new package is needed, add it into package-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; Package archives
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (setq package-archives nil)
  ;;; Tsinghua mirrors
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")) t)
  ;; (add-to-list 'package-archives (cons "gnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")) t)
  ;;; Emacs China mirrors
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")) t)
  ;; (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.emacs-china.org/gnu/")) t)
  ;;; Tencent mirrors
  (add-to-list 'package-archives (cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/")) t)
  (when (< emacs-major-version 24)
    ;; (add-to-list 'package-archives (cons "gnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
    ;; (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.emacs-china.org/gnu/")))
    (add-to-list 'package-archives (cons "gnu" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/")))
    ))

;; Packages list in need
(setq my-package-list
      '(systemd solarized-theme helm-rtags helm-descbinds helm-describe-modes use-package-chords diminish use-package rg darkokai-theme monokai-theme irony-eldoc benchmark-init ace-mc iedit multiple-cursors tramp rtags rtags-xref company-rtags flycheck-rtags ivy-rtags pinyin-search ace-pinyin beacon eno ace-window anaconda-mode company helm helm-core irony ivy undo-tree rainbow-delimiters python company-anaconda cmake-ide company-try-hard company-c-headers counsel-gtags helm-gtags function-args zeal-at-point neotree diff-hl hl-todo idle-highlight-mode flx-ido flx-isearch smex flx company-lsp helm-lsp lsp-ivy lsp-mode lsp-treemacs lsp-ui yasnippet-snippets yasnippet cmake-font-lock cmake-mode flycheck-irony projectile flycheck-clang-analyzer flycheck json-mode xclip vlf switch-window markdown-mode magit highlight-indent-guides gnu-elpa-keyring-update ggtags evil ecb counsel company-irony))

(package-initialize)
;; (setq package-enable-at-startup nil)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-packages)

;;; init-packages.el ends here
