(setq org-todo-keywords
      '((sequence "TODO(t)" "DELAY(n)" "INPROGRESS(i)" "|" "DONE(d)")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
	(sequence "|" "CANCELED(c)")
	(dequence "|" "RECREATED(m)")))

(setq org-startup-folded 'show-all)
