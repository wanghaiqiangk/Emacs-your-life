;;; superior-prog.el --- Make programming stronger
;;; Commentary:
;;;   None
;;; Code:

(setq completion-ignore-case t)

;;; Keybinding for c/c++
;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++mode)
              (local-set-key (kbd "C-M-a") #'c-beginning-of-defun)
              (local-set-key (kbd "C-M-e") #'c-end-of-defun)
              (local-set-key (kbd "C-c o") #'ff-find-other-file)
              )))

;;; Better cmake syntax highlight
;;
(use-package cmake-font-lock
  :hook (prog-mode . (lambda ()
                       (when (derived-mode-p 'cmake-mode)
                         (cmake-font-lock-activate)))))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.5)
  (company-show-numbers t)
  (company-backends '(company-files
                      company-keywords
                      company-capf
                      company-dabbrev
                      company-dabbrev-code))
  :hook
  (cmake-mode . (lambda ()
                  (add-to-list (make-local-variable 'company-backends)
                               'company-cmake)))
  ((c-mode c++-mode) . (lambda ()
                         (add-to-list (make-local-variable 'company-backends)
                                      '(company-capf
                                        company-c-headers
                                        )))))

(let* ((local-def-file "~/.emacs.d/user-config/local-def.el"))
  (if (file-exists-p local-def-file)
      (load (string-remove-suffix ".el" local-def-file))
    (defvar default-searching-path nil
      "Specify default include searching paths.
Create a new elisp file named as \"local-def.el\" in user-config directory.
Then based on the return values from shell command \"`gcc -print-prog-name=cc1plus` -v\",
properly define this variable.")))
(use-package company-c-headers
  :defines default-searching-path
  :config
  (dolist (includepath default-searching-path)
    (add-to-list 'company-c-headers-path-system includepath)))

;;; Python
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (if (not wang-python3-binary-path)
      (call-interactively #'wang/set-python3-binary-path))
  (setq elpy-shell-echo-output nil ;; This solve strange '^G's in Ipython output.
        elpy-rpc-python-command wang-python3-binary-path
	    python-shell-interpreter "python3"))

(use-package eglot
  :ensure t
  :defer t
  :init
  (require 'eldoc-box)
  (setq x-gtk-resize-child-frames 'resize-mode)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-stay-out-of 'flymake)
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :hook
  (eglot-managed-mode . (lambda () (progn
                                     (flymake-mode -1)
                                     (eldoc-box-hover-mode t)))))

(use-package eldoc-box
  :ensure t)


(use-package citre
  :defer t
  :init
  (require 'citre-config)
  :config
  (setq
   citre-peek-fill-fringe nil
   citre-completion-case-sensitive nil
   ;; citre-project-root-function #'projectile-project-root
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t))


;; (setq gdb-show-main t)


(provide 'superior-prog)

;;; superior-prog.el ends here
