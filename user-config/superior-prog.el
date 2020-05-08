;;; superior-prog.el --- Make programming stronger
;;; Commentary:
;;;   None
;;; Code:

;;; Keybinding for c/c++
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++mode)
              (local-set-key (kbd "C-M-a") #'c-beginning-of-defun)
              (local-set-key (kbd "C-M-e") #'c-end-of-defun)
              (local-set-key (kbd "C-c o") #'ff-find-other-file)
              )))

;;; Better cmake syntax highlight
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'prog-mode-hook
          (lambda ()
            (when (derived-mode-p 'cmake-mode)
              (cmake-font-lock-activate))))

;; Code navigator
;; RTags >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(require 'rtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (rtags-start-process-unless-running))))

(setq rtags-completions-enabled t)
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
(setq rtags-display-result-backend 'ivy)
;; (setq rtags-symbolnames-case-insensitive t)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< RTags

;; ggtags >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ggtags

;; Auto Completion
;; Company and Irony  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(autoload 'company-try-hard "company-try-hard" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.5)
(global-set-key (kbd "C-x p") 'company-try-hard)

(require 'company-c-headers)
(let* ((local-def-file "~/.emacs.d/user-config/local-def.el"))
  (if (file-exists-p local-def-file)
      (load local-def-file)
    (defvar default-searching-path nil
      "Specify default include searching paths.
Create a new elisp file named as \"local-def.el\" in user-config directory.
Then based on the return values from shell command \"`gcc -print-prog-name=cc1plus` -v\",
properly define this variable.")))
(eval-after-load 'company
  '(dolist (includepath default-searching-path)
     (add-to-list 'company-c-headers-path-system includepath)))

(require 'company-rtags)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (irony-mode t))))
;; To make irony work better,
;; 1. irony-install-server, but this only need to run once.
;; 2. For simple project with clear architecture,
;;    irony-cdb-autosetup-compile-options can handle,
;;    and always check irony-cdb-menu for validation.
;; 3. For architecture being not clear,
;;    run irony-cdb-json-add-compile-commands-path
;;    to setup root directory and path to compile_commands.json,
;;    and always check irony-cdb-menu for validation.
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company backends setup
(setq-default company-backends
              '((company-files
                 company-keywords
                 company-capf)
                (company-dabbrev
                 company-dabbrev-code)))
;; For CMake mode
(add-hook 'prog-mode-hook
          (lambda ()
            (when (derived-mode-p 'cmake-mode)
              (add-to-list (make-local-variable 'company-backends)
                         'company-cmake))))
;; For c/c++
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (add-to-list (make-local-variable 'company-backends)
                           '(company-rtags
                             company-c-headers
                              company-irony)))))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (add-to-list (make-local-variable 'company-backends)
                           '(company-clang
                             company-semantic)))))
;; For python
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-anaconda))))
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Company and Irony

;; Python complete >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Python complete

;; flycheck is syntax validator >>>>>>>>>>>>>>>>>>>>
(require 'flycheck)
(require 'flycheck-rtags)

(flycheck-add-mode 'json-jsonlint 'json-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;; flycheck for python
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-pylint-executable "python3")
(setq flycheck-python-flake8-executable "python3")

;; flycheck for rtags
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (my-flycheck-rtags-setup))))
;; <<<<<<<<<<<<<<<<<<<< flycheck

;; cmake-ide, should be called after rtags required
(cmake-ide-setup)
(dolist (sys-include-flag default-searching-path)
  (let* ((flag-prefix "-I"))
    (add-to-list 'cmake-ide-flags-c (concat flag-prefix sys-include-flag))
    (add-to-list 'cmake-ide-flags-c++ (concat flag-prefix sys-include-flag))))

(setq gdb-show-main t)

(provide 'superior-prog)

;;; superior-prog.el ends here
