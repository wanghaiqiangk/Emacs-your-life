(add-to-list 'load-path "~/.emacs.d/configure")

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
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-hook 'find-file-hook (lambda () (linum-mode 1)))

(load "hl-line")

(defface my-linum-hl
  `((t :inherit linum :background "yellow",(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d \u2502")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

(ad-activate 'linum-update)
(setq linum-format 'my-linum-format)
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

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

;; Configure company, irony and company-irony
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-x p") 'company-complete)
(setq company-clang-executable "/usr/bin/clang-3.9")

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(company-irony 1)

;; Use "M-s ." to search symbol at current point
;; Then it works like Vim, which remember your last search contents
;; "C-s" to search forward, while "C-r" to search backward
(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp."
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol (&optional partialp backward)
  "Put symbol at current point into search string.

  If PARTIALP is non-nil, find all partial matches."
  (interactive "P")

  (let (from to bound sym)
    (setq sym
                                        ; this block taken directly from find-tag-default
                                        ; we couldn't use the function because we need the internal from and to values
          (when (or (progn
                      ;; Look at text around `point'.
                      (save-excursion
                        (skip-syntax-backward "w_") (setq from (point)))
                      (save-excursion
                        (skip-syntax-forward "w_") (setq to (point)))
                      (> to from))
                    ;; Look between `line-beginning-position' and `point'.
                    (save-excursion
                      (and (setq bound (line-beginning-position))
                           (skip-syntax-backward "^w_" bound)
                           (> (setq to (point)) bound)
                           (skip-syntax-backward "w_")
                           (setq from (point))))
                    ;; Look between `point' and `line-end-position'.
                    (save-excursion
                      (and (setq bound (line-end-position))
                           (skip-syntax-forward "^w_" bound)
                           (< (setq from (point)) bound)
                           (skip-syntax-forward "w_")
                           (setq to (point)))))
            (buffer-substring-no-properties from to)))
    (cond ((null sym)
           (message "No symbol at point"))
          ((null backward)
           (goto-char (1+ from)))
          (t
           (goto-char (1- to))))
    (isearch-search)
    (if partialp
        (isearch-yank-string sym)
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))

(defun isearch-current-symbol (&optional partialp)
  "Incremental search forward with symbol under point.

    Prefixed with \\[universal-argument] will find all partial
    matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-forward-regexp nil 1)
    (isearch-yank-symbol partialp)))

(defun isearch-backward-current-symbol (&optional partialp)
  "Incremental search backward with symbol under point.

    Prefixed with \\[universal-argument] will find all partial
    matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-backward-regexp nil 1)
    (isearch-yank-symbol partialp)))

;; Subsequent hitting of the keys will increment to the next
;; match--duplicating `C-s' and `C-r', respectively.
(define-key isearch-mode-map [f3] 'isearch-repeat-forward)
(define-key isearch-mode-map [(control f3)] 'isearch-repeat-backward)

;; ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "./.emacs.bak")))

(global-undo-tree-mode)

(load "doremi")
(load "doremi-cmd")

(global-unset-key (kbd "M-'"))
(defun align-type-var (BEG END)
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)[^ ]+\\((\\|;\\)" 1 1))
(global-set-key (kbd "M-'") 'align-type-var)

(global-set-key (kbd "C-x o") 'switch-window)

(define-skeleton cc-main
  "Insert main function."
  nil
  "int main(int argc, char *argv[])" \n
  -4 "{" \n
  _
  \n -4 "}")

(define-skeleton cc-include
  "Insert header"
  "this prompt is ignored"
  ("Enter header file: " "#include " str \n))

(global-set-key (kbd "C-c i") 'cc-include)
(global-set-key (kbd "C-c m") 'cc-main)

(setq-default show-trailing-whitespace t)
(setq shell-file-name "/bin/bash")

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(autoload 'turn-on-fic-mode "fic-mode" nil t)
(add-hook 'c++-mode-hook 'turn-on-fic-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)

;; Default bsd style use 8 offset, which is compatible to other editors, like subl, vim
;; (smart-tabs-insinuate 'c 'c++)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

(load "cmake-mode")

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

(require 'vlf-setup)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'ecb)
(global-set-key (kbd "<f5>") 'ecb-minor-mode)
(defun display-buffer-at-bottom--display-buffer-at-bottom-around (orig-fun &rest args)
  "Bugfix for ECB: cannot use display-buffer-at-bottom', call display-buffer-use-some-window' instead in ECB frame."
  (if (and ecb-minor-mode (equal (selected-frame) ecb-frame))
      (apply 'display-buffer-use-some-window args)
    (apply orig-fun args)))
(advice-add 'display-buffer-at-bottom :around #'display-buffer-at-bottom--display-buffer-at-bottom-around)

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

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(set-face-attribute 'neo-file-link-face t :foreground "white")

;; Add automatically by Customization
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left11")
 '(ecb-options-version "2.50")
 '(ecb-windows-width 0.33)
 '(package-selected-packages
   (quote
    (ecb xclip vlf switch-window markdown-mode magit highlight-indent-guides gnu-elpa-keyring-update ggtags evil counsel company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
