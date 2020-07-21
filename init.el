;;; init.el --- Wang's configuration
;;; Commentary:
;;;   Config Manager
;;; Code:

;; (setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/configure")
(add-to-list 'load-path "~/.emacs.d/user-config")

(load "init-packages")
;; (require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(load "basic-config")
(load "common-prog")
(load "superior-prog")

(require 'window-split)
(require 'vlf-setup)
(require 'doremi)
(require 'doremi-cmd)

;;; init.el ends here
