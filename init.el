;;; init.el --- Wang's configuration
;;; Commentary:
;;;   Config Manager
;;; Code:

;; (setq debug-on-error t)
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(add-to-list 'load-path "~/.emacs.d/configure")
(add-to-list 'load-path "~/.emacs.d/user-config")

(load "init-packages")
(load "basic-config")
(load custom-file)
(load "common-prog")
(load "superior-prog")
(load "org-config")
(load "text-manipulate")

;;; init.el ends here
