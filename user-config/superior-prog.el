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
  ;; (rtags-autostart-diagnostics t)
  (rtags-display-result-backend 'helm)
  (rtags-symbolnames-case-insensitive t)
  ;; (rtags-display-current-error-as-tooltip nil)
  :config
  (rtags-enable-standard-keybindings))

;; New functionality, indicating whether current buffer is indexed
;; idea: use `rtags-is-indexed' to change mode-line
;;       encapsulate as function and load after cmake-ide

(use-package company
  :init (require 'company-rtags)
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
                                        company-rtags
                                        company-c-headers
                                        ))))
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


;;; Python complete
;;
(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))


;;; cmake-ide, should be called after rtags required
;;
(defun my/cmake-ide-setup ()
  "Manually start camke-ide as well as rtags."
  (interactive)
  (cmake-ide-maybe-run-cmake)
  (cmake-ide-maybe-start-rdm))
(use-package cmake-ide
  :bind ("C-c a p" . my/cmake-ide-setup)
  :config
  (dolist (sys-include-flag default-searching-path)
    (let* ((flag-prefix "-I"))
      (add-to-list 'cmake-ide-flags-c (concat flag-prefix sys-include-flag))
      (add-to-list 'cmake-ide-flags-c++ (concat flag-prefix sys-include-flag)))))


(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  )


(require 'citre)
(require 'citre-config)
(setq citre-peek-fill-fringe nil)


;; (setq gdb-show-main t)


(provide 'superior-prog)

;;; superior-prog.el ends here
