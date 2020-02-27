;; Use "M-s ." to search symbol at current point
;; Then it works like Vim, which remember your last search contents
;; "C-s" to search forward, while "C-r" to search backward
;; (defun isearch-yank-regexp (regexp)
;;   "Pull REGEXP into search regexp."
;;   (let ((isearch-regexp nil)) ;; Dynamic binding of global.
;;     (isearch-yank-string regexp))
;;   (isearch-search-and-update))

;; (defun isearch-yank-symbol (&optional partialp backward)
;;   "Put symbol at current point into search string.

;;   If PARTIALP is non-nil, find all partial matches."
;;   (interactive "P")

;;   (let (from to bound sym)
;;     (setq sym
;;                                         ; this block taken directly from find-tag-default
;;                                         ; we couldn't use the function because we need the internal from and to values
;;           (when (or (progn
;;                       ;; Look at text around `point'.
;;                       (save-excursion
;;                         (skip-syntax-backward "w_") (setq from (point)))
;;                       (save-excursion
;;                         (skip-syntax-forward "w_") (setq to (point)))
;;                       (> to from))
;;                     ;; Look between `line-beginning-position' and `point'.
;;                     (save-excursion
;;                       (and (setq bound (line-beginning-position))
;;                            (skip-syntax-backward "^w_" bound)
;;                            (> (setq to (point)) bound)
;;                            (skip-syntax-backward "w_")
;;                            (setq from (point))))
;;                     ;; Look between `point' and `line-end-position'.
;;                     (save-excursion
;;                       (and (setq bound (line-end-position))
;;                            (skip-syntax-forward "^w_" bound)
;;                            (< (setq from (point)) bound)
;;                            (skip-syntax-forward "w_")
;;                            (setq to (point)))))
;;             (buffer-substring-no-properties from to)))
;;     (cond ((null sym)
;;            (message "No symbol at point"))
;;           ((null backward)
;;            (goto-char (1+ from)))
;;           (t
;;            (goto-char (1- to))))
;;     (isearch-search)
;;     (if partialp
;;         (isearch-yank-string sym)
;;       (isearch-yank-regexp
;;        (concat "\\_<" (regexp-quote sym) "\\_>")))))

;; (defun isearch-current-symbol (&optional partialp)
;;   "Incremental search forward with symbol under point.

;;     Prefixed with \\[universal-argument] will find all partial
;;     matches."
;;   (interactive "P")
;;   (let ((start (point)))
;;     (isearch-forward-regexp nil 1)
;;     (isearch-yank-symbol partialp)))

;; (defun isearch-backward-current-symbol (&optional partialp)
;;   "Incremental search backward with symbol under point.

;;     Prefixed with \\[universal-argument] will find all partial
;;     matches."
;;   (interactive "P")
;;   (let ((start (point)))
;;     (isearch-backward-regexp nil 1)
;;     (isearch-yank-symbol partialp)))

;; ;; Subsequent hitting of the keys will increment to the next
;; ;; match--duplicating `C-s' and `C-r', respectively.
;; (define-key isearch-mode-map [f3] 'isearch-repeat-forward)
;; (define-key isearch-mode-map [(control f3)] 'isearch-repeat-backward)