(if (functionp 'global-hi-lock-mode)
	(global-hi-lock-mode 1)
  (hi-lock-mode 1))

(defun highlight-todo ()
  "Highlight all TODO keywords."
  (highlight-regexp "\\(TODO\\)\\|\\(todo\\)" "hi-yellow"))

(add-hook 'find-file-hook 'highlight-todo)
