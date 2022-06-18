; set up melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa" . 10)
				   ("gnu" . 5)
				   ("nongnu" . 2)))
(package-initialize)

; set custom settings
(setq make-backup-files nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq lisp-indent-function 'common-lisp-indent-function)
(setq mac-command-modifier 'ctrl)
(setq hg/packages '(exec-path-from-shell
		    magit))

; define custom functions
(defun hg/sync-packages (packages)
  (dolist (package hg/packages)
    (when (not (package-installed-p package))
      (package-install package))))

; refresh and load packages
(package-refresh-contents 'async)
(hg/sync-packages hg/packages)

; exec path from shell for mac
(exec-path-from-shell-initialize)

; key bindings
