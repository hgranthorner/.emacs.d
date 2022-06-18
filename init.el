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
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq hg/packages '(exec-path-from-shell
		    magit
		    browse-kill-ring
		    company))

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

; package settings
(fido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-enable-prefix t)
(global-company-mode 1)

; key bindings
(global-set-key (kbd "C-M-y") #'browse-kill-ring)
