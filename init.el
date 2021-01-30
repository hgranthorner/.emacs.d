(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines for stable
  ;;(add-to-list 'package-archives
  ;;             (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
  (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Check operating system
(setq is-mac (equal system-type 'darwin))
(setq is-windows (equal system-type 'windows-nt))

;; Initialize use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)

;; Font
(if is-windows
    (set-frame-font "Consolas 17" nil t))

;; Sensible startup
(setq mac-command-modifier 'control)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
(setq mac-command-modifier 'control)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(desktop-save-mode 1)
(delete-selection-mode)

(if is-windows
    (setq default-directory "C:/Users/Grant/Dev/"))
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

;; Start Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Dired-x
(load "dired-x")

;; Kill ring
(use-package browse-kill-ring)


;; Autocomplete brackets
(electric-pair-mode 1)
(show-paren-mode 1)
(setq electric-pair-preserve-balance nil)

;; Theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

;; Magit
(use-package magit
  :bind ("C-x g" . 'magit-status))

;; Which key
(use-package which-key
  :config (which-key-mode))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (yas-reload-all)
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
  (setq yas-prompt-functions '(yas-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode)
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
  :diminish yas-minor-mode)

;; Fix c indentation
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

;; Elixir
(use-package alchemist)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets yasnippet which-key alchemist diminish zenburn-theme magit multiple-cursors use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
