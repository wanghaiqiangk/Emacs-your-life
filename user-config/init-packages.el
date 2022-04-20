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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; (add-to-list 'package-archives (cons "gnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
    ;; (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.emacs-china.org/gnu/")))
    (add-to-list 'package-archives (cons "gnu" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/")))
    ))

;; Packages list in need
(setq my-package-list
      '(vimish-fold imenu-anywhere clang-format magit citre eglot visual-fill-column gitignore-mode yaml-mode go-mode dumb-jump which-key syslog-mode systemd solarized-theme helm-descbinds helm-describe-modes use-package-chords diminish use-package rg benchmark-init ace-mc iedit multiple-cursors tramp pinyin-search ace-pinyin beacon ace-window anaconda-mode company helm helm-core ivy undo-tree rainbow-delimiters python company-anaconda company-try-hard company-c-headers zeal-at-point neotree diff-hl hl-todo idle-highlight-mode flx-ido smex flx yasnippet-snippets yasnippet cmake-font-lock cmake-mode projectile flycheck-clang-analyzer flycheck json-mode xclip vlf switch-window markdown-mode highlight-indent-guides gnu-elpa-keyring-update counsel))

(package-initialize)
;; (setq package-enable-at-startup nil)

(unless package-archive-contents
  (package-refresh-contents))

(defun wang/install-custom-packages ()
    "Check required thirdparty packages and install them if they are
not yet in the system."
    (dolist (package my-package-list)
      (unless (package-installed-p package)
        (package-install package))))

(provide 'init-packages)

;;; init-packages.el ends here
