;;; superior-prog.el --- Make programming stronger
;;; Commentary:
;;;   None
;;; Code:

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


;;; RTags
;;
(use-package rtags
  :hook (c-mode-common-hook . (lambda ()
                                (when (derived-mode-p 'c-mode 'c++-mode)
                                  (rtags-start-process-unless-running))))
  :custom
  (rtags-completions-enabled t)
  (rtags-autostart-diagnostics t)
  (rtags-display-result-backend 'helm)
  (rtags-symbolnames-case-insensitive t)
  :config
  (rtags-enable-standard-keybindings))


;;; ggtags
;;
;; (require 'counsel-gtags)
;; (setq counsel-gtags-auto-update t
;;       counsel-gtags-ignore-case t
;;       counsel-gtags-prefix-key "C-cg"
;;       counsel-gtags-use-suggested-key-map t
;;       counsel-gtags-use-input-at-point t)

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (counsel-gtags-mode 1))))

;; (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
;; (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
;; (define-key counsel-gtags-mode-map (kbd "C-c >") 'counsel-gtags-go-forward)


;;; Auto Completion
;; Company and Irony
(use-package company-try-hard
  :bind ("C-x p" . company-try-hard))


(use-package company
  :init (require 'company-rtags)
  :hook (after-init . global-company-mode)
  :custom
  (company-idel-delay 0.5)
  (company-backends '((company-files
                       company-keywords
                       company-capf)
                      (company-dabbrev
                       company-dabbrev-code)))
  :hook
  (cmake-mode . (lambda ()
                  (add-to-list (make-local-variable 'company-backends)
                               'company-cmake)))
  ((c-mode c++-mode) . (lambda ()
                        (add-to-list (make-local-variable 'company-backends)
                                     '((company-rtags
                                        company-c-headers
                                        company-irony)
                                       (company-clang
                                        company-semantic)))))
  (python-mode . (lambda ()
                   (add-to-list (make-local-variable 'company-backends)
                                '(company-anaconda)))))

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


;; To make irony work better,
;; 1. irony-install-server, but this only need to run once.
;; 2. For simple project with clear architecture,
;;    irony-cdb-autosetup-compile-options can handle,
;;    and always check irony-cdb-menu for validation.
;; 3. For architecture being not clear,
;;    run irony-cdb-json-add-compile-commands-path
;;    to setup root directory and path to compile_commands.json,
;;    and always check irony-cdb-menu for validation.
(use-package irony
  :hook ((c-mode c++-mode) . (lambda () (irony-mode t))))

(use-package irony-cdb
  :requires (irony)
  :hook (irony-mode . irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :requires (irony)
  :hook (irony-mode . irony-eldoc))


;;; Python complete
;;
(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))


;;; flycheck is syntax validator
;;
(use-package flycheck
  :config
  (flycheck-add-mode 'json-jsonlint 'json-mode)
  :hook
  (prog-mode . flycheck-mode)
  (c++-mode . (lambda ()
                (progn
                  (setq flycheck-gcc-language-standard "c++11")
                  (setq flycheck-clang-language-standard "c++11"))))
  (python-mode . (lambda ()
                   (progn
                     (setq flycheck-python-pycompile-executable "python3")
                     (setq flycheck-python-pylint-executable "python3")
                     (setq flycheck-python-flake8-executable "python3")))))

(use-package flycheck-rtags
  :hook
  ((c-mode c++-mode) . (lambda ()
                        (progn
                          (flycheck-select-checker 'rtags)
                          (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
                          (setq-local flycheck-check-syntax-automatically nil)))))


;;; cmake-ide, should be called after rtags required
;;
(use-package cmake-ide
  :config
  (cmake-ide-setup)
  (dolist (sys-include-flag default-searching-path)
    (let* ((flag-prefix "-I"))
      (add-to-list 'cmake-ide-flags-c (concat flag-prefix sys-include-flag))
      (add-to-list 'cmake-ide-flags-c++ (concat flag-prefix sys-include-flag)))))


;; (setq gdb-show-main t)

(provide 'superior-prog)

;;; superior-prog.el ends here
