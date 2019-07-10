(if (functionp 'global-hi-lock-mode)
	(global-hi-lock-mode 1)
  (hi-lock-mode 1))

(defface todo-yellow
	`((t :inherit hi-lock-faces :background "yellow" :foreground "black", (face-background 'hi-yellow nil t)))
	"Face for TODO keywords"
	:group 'hi-lock-faces)

(defun highlight-todo ()
  "Highlight all TODO keywords."
  (highlight-regexp "\\(TODO\\)\\|\\(todo\\)" "todo-yellow"))

(add-hook 'find-file-hook 'highlight-todo)
