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
