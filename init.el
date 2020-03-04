(add-to-list 'load-path "~/.emacs.d/configure")
(add-to-list 'load-path "~/.emacs.d/user-config")

(load "~/.emacs.d/user-config/init-packages")

;; Display line number >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(add-hook 'find-file-hook (lambda () (display-line-numbers-mode 1)))
(set-face-attribute 'line-number-current-line t :background "yellow"
                    :foreground "black")
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Display line number

(load "highlight-indent-guides")
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-background 'highlight-indent-guides-odd-face "#cd00cd")
(set-face-background 'highlight-indent-guides-even-face "#cd00cd")
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0)
(set-face-background 'highlight-indent-guides-top-odd-face "#00cdcd")
(set-face-background 'highlight-indent-guides-top-even-face "#00cdcd")

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

(electric-pair-mode 1)
(show-paren-mode 1)

(setq select-enable-clipboard t)
(xclip-mode 1)

;; Company and Irony  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Company only config
(require 'company-try-hard)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-clang-executable "/usr/bin/clang-3.9")
(setq company-idle-delay 0)
(global-set-key (kbd "C-x p") 'company-try-hard)

(require 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-c-headers-path-system "/usr/include/c++/5.4.0/"))

;; Irony only
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

;; Company-Irony, backends setup
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
                           '(company-c-headers
                              company-irony)))))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (add-to-list (make-local-variable 'company-backends)
                           '(company-clang
                             company-semantic)))))
;; For python
(require 'python)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-anaconda))))
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Company and Irony


;; ggtags >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Commit original ggtags if use counsel-gtags
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

(require 'counsel-gtags)
(setq counsel-gtags-auto-update t
      counsel-gtags-ignore-case t
      counsel-gtags-prefix-key "C-cg"
      counsel-gtags-use-suggested-key-map t
      counsel-gtags-use-input-at-point t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (counsel-gtags-mode 1))))

(define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
(define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-pop)
(define-key counsel-gtags-mode-map (kbd "C-c <") 'counsel-gtags-go-forward)
(define-key counsel-gtags-mode-map (kbd "C-c >") 'counsel-gtags-go-backward)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ggtags


;; Ivy, counsel, and swiper >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "C-r") 'swiper-isearch-backward)
;; (setq ivy-display-style 'fancy
;;       swiper-include-line-number-in-search t)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
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
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Ivy, counsel, and swiper


;; smex >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< smex


;; ido >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ido

(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "./.emacs.bak")))

(global-undo-tree-mode)

(load "doremi")
(load "doremi-cmd")

;; In vim there's easyalign
;; (global-unset-key (kbd "M-'"))
;; (defun align-type-var (BEG END)
;;   (interactive "r")
;;   (align-regexp BEG END "\\(\\s-*\\)[^ ]+\\((\\|;\\)" 1 1))
;; (global-set-key (kbd "M-'") 'align-type-var)

(global-set-key (kbd "C-x o") 'ace-window)

;; skeleton >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; (define-skeleton cc-main
;;   "Insert main function."
;;   nil
;;   "int main(int argc, char *argv[])" \n
;;   -4 "{" \n
;;   _
;;   \n -4 "}")

;; (define-skeleton cc-include
;;   "Insert header"
;;   "this prompt is ignored"
;;   ("Enter header file: " "#include " str \n))

;; (global-set-key (kbd "C-c i") 'cc-include)
;; (global-set-key (kbd "C-c m") 'cc-main)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< skeleton

(setq-default show-trailing-whitespace t)
(setq shell-file-name "/bin/bash")

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

;; Highlight todo keywords in comment >>>>>>>>>>>>>>>>>
(add-hook 'prog-mode-hook 'hl-todo-mode)
;; <<<<<<<<<<<<<<<<< Highlight todo keywords in comment

;; Default bsd style use 8 offset, which is compatible to other editors, like subl, vim
;; (smart-tabs-insinuate 'c 'c++)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defun set-auto-indentation-offset ()
  (setq c-basic-offset 4)
  (c-set-offset 'inlambda 0)
  (c-set-offset 'inline-open 0))
(add-hook 'c-mode-hook 'set-auto-indentation-offset)
(add-hook 'c++-mode-hook 'set-auto-indentation-offset)

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'prog-mode-hook
          (lambda ()
            (when (derived-mode-p 'cmake-mode)
              (cmake-font-lock-activate))))

(global-set-key (kbd "C-c C-r") 'replace-regexp)
(global-set-key (kbd "C-c r") 'replace-string)

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
(set-face-attribute 'whitespace-newline t :foreground "#cd00cd")
(set-face-attribute 'whitespace-tab t :foreground "#cd00cd")
(set-face-attribute 'whitespace-tab t :background 'unspecified)

(define-coding-system-alias 'UTF-8 'utf-8)

;; vlf is view-large-file
(require 'vlf-setup)

(global-set-key (kbd "C-x g") 'magit-status)


;; Emacs Code Brower >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; (require 'ecb)
;; (global-set-key (kbd "<f5>") 'ecb-minor-mode)
;; (defun display-buffer-at-bottom--display-buffer-at-bottom-around (orig-fun &rest args)
;;   "Bugfix for ECB: cannot use display-buffer-at-bottom', call display-buffer-use-some-window' instead in ECB frame."
;;   (if (and ecb-minor-mode (equal (selected-frame) ecb-frame))
;;       (apply 'display-buffer-use-some-window args)
;;     (apply orig-fun args)))
;; (advice-add 'display-buffer-at-bottom :around #'display-buffer-at-bottom--display-buffer-at-bottom-around)
;; (setq ecb-layout-name "left11")
;; (setq ecb-new-ecb-frame t)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Emacs Code Brower

;; Split two window vertically >>>>>>>>>>>>>>>>>>>>>>>>>
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x |") 'toggle-window-split)
;; <<<<<<<<<<<<<<<<<<<<<<<<< Split two window vertically

;; neotree is a file navigator
(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(set-face-attribute 'neo-file-link-face t :foreground "white")
(setq neo-smart-open t)

;; flycheck is syntax validator >>>>>>>>>>>>>>>>>>>>
(require 'flycheck)
(flycheck-add-mode 'json-jsonlint 'json-mode)
;; (add-hook 'json-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
;; flycheck for python
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-pylint-executable "python3")
(setq flycheck-python-flake8-executable "python3")
;; <<<<<<<<<<<<<<<<<<<< flycheck


;; projectile is a project manager, like any IDE does
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; yasnippet configuration
(require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))
(add-hook 'prog-mode-hook #'yas-minor-mode)
(eval-after-load 'yasnippet
  '(yas-reload-all))

;; Keybinding for regular expression isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; Keybinding for fuzzy isearch
(global-set-key (kbd "C-M-s") 'flx-isearch-forward)
(global-set-key (kbd "C-M-r") 'flx-isearch-backward)
;; Keybinding for c-function-movement
(add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "C-x [") #'c-beginning-of-defun)))
(add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "C-x ]") #'c-end-of-defun)))


;; diff-hl >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
(eval-after-load 'diff-hl
  '(diff-hl-margin-mode t))
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< diff-hl


;; Idel highlight symbols >>>>>>>>>>>>>>>>>>>>>>>>>>
(add-hook 'prog-mode-hook 'idle-highlight-mode)
;; <<<<<<<<<<<<<<<<<<<<<<<<<< Idel highlight symbols


;; Zeal Plugin >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(global-set-key (kbd "C-c z .") 'zeal-at-point)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Zeal Plugin

;; Python complete >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Python complete
