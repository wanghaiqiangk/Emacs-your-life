;;; init.el --- Wang's configuration
;;; Commentary:
;;;   Config Manager
;;; Code:

(add-to-list 'load-path "~/.emacs.d/configure")
(add-to-list 'load-path "~/.emacs.d/user-config")

(load "init-packages")
(load "basic-config")
(load "common-prog")
(load "superior-prog")

(require 'window-split)
(require 'vlf-setup)
(require 'doremi)
(require 'doremi-cmd)

;;; init.el ends here
