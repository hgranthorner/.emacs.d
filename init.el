; set up melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa" . 10)
				   ("gnu" . 5)
				   ("nongnu" . 2)))
(package-initialize)

; set custom settings
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq lisp-indent-function 'common-lisp-indent-function)
(setq mac-command-modifier 'ctrl)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; fixes c indentation
(setq c-default-style "bsd"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil)

(setq hg/packages '(exec-path-from-shell
		    magit
		    browse-kill-ring
		    company
		    multiple-cursors))

; define custom functions
(defun hg/sync-packages (packages)
  "Install any missing packages on startup."
  (dolist (package hg/packages)
    (when (not (package-installed-p package))
      (package-install package))))

(defun hg/fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

; refresh and load packages
(package-refresh-contents 'async)
(hg/sync-packages hg/packages)
(load custom-file)

; exec path from shell for mac
(exec-path-from-shell-initialize)

; package settings
(fido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-enable-prefix t)
(global-company-mode 1)
(setq mc/always-run-for-all t)

; key bindings
(global-set-key (kbd "C-M-y") #'browse-kill-ring)
(global-set-key (kbd "M-o")   #'other-window)
(global-set-key (kbd "M-i")   #'imenu)

;; multiple cursors
(global-set-key (kbd "C-M->")	#'mc/edit-lines)
(global-set-key (kbd "C->")     #'mc/mark-next-like-this)
(global-set-key (kbd "C-<")	#'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") #'mc/mark-all-like-this)
;;; makes it so that you need to "C-g" to get out of multiple cursor
;;; mode, return inserts a new line.
(with-eval-after-load "multiple-cursors"
  (keymap-set mc/keymap "<return>" nil))
