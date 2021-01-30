;; This is my older mac based init file. Moving to a slightly simpler x-platform version.
;; Archived 1/30/2021

(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(if is-mac
    (setq default-directory "/Users/grant/"))

;; Initialize use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)

(use-package exec-path-from-shell
  :config
  (when is-mac (exec-path-from-shell-initialize)))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(setq backup-directory-alist '(("." . "~/.emacs_backups")))
(setq mac-command-modifier 'control)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 0)
(setq compile-command "make")
(setq left-margin-width 0)
(delete-selection-mode 1)
(setq completion-ignore-case t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "IN PROGRESS" "|" "DONE" "DELEGATED")))

;; Keybinds
(dolist (key '("\C-x C-b" "\M-z" "\M-o" "\C-c c" "\C-c s" "\M-n" "\M-p"))
  (global-unset-key (kbd key)))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c y") 'browse-kill-ring)

;; Completion
(setq completion-styles '(initials partial-completion flex))
(setq completion-cycle-threshold 10)
(fido-mode 1)

;; Dired-x
(load "dired-x")

;; Kill ring
(use-package browse-kill-ring)

;; Speedbar in same frame
(use-package sr-speedbar)
(setq speedbar-show-unknown-files 1)
(setq sr-speedbar-auto-refresh nil)

;; Theme & look
(load "~/.emacs.d/theme.el")
(load-theme 'naysayer t)

;; (use-package naysayer-theme
;;   :init
;;   (load-theme 'naysayer t))

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

;; Autocomplete brackets
(electric-pair-mode 1)
(show-paren-mode 1)
(setq electric-pair-preserve-balance nil)

;; Fix c indentation
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(browse-kill-ring sr-speedbar multiple-cursors magit naysayer-theme exec-path-from-shell diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
