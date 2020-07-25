;;; smart-replace.el --- Intelligently replace string
;;; Commentary:
;;  When it comes to replace, Emacs provides at least two kinds of
;;  functions for it, `replace-string' and `replace-regex'.
;;
;;  This package provides a way to combine these functions together.
;;  Users only need to invoke one command and posibly with prefix, to
;;  call specified internal Emacs function.
;;
;;  TODO:
;;  - Default value at point
;;; Code:

(defvar smart-replace-mode-list
  '(replace-string
    replace-regexp)
  "The list when start smart-replace-mode.")

(defun smart-replace-mode(&optional prefix)
  "Select one of replacement function based on PREFIX."
  (interactive "p")
  (let ((index (/ prefix 4))
        (submode-list-length (length smart-replace-mode-list)))
    (if (< index 0)
        (error "[smart-replace-mode] Invalid prefix command"))
    (if (>= index submode-list-length)
        (setq index (1- submode-list-length)))
    (call-interactively (nth index smart-replace-mode-list))))

(provide 'smart-replace)

;;; smart-replace.el ends here
