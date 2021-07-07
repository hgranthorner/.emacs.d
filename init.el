(setq gc-cons-threshold (* 50 1000 1000))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
;; Check operating system
(setq is-mac (equal system-type 'darwin))
(setq is-windows (equal system-type 'windows-nt))

(use-package diminish
  :init
  :diminish auto-revert-mode
  :diminish eldoc-mode
  )

;; Font
(if is-windows
    (set-frame-font "Consolas 17" nil t))
(if is-mac
    (set-face-attribute 'default nil :height 160))

;; Sensible startup
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
(setq mac-command-modifier 'control)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(desktop-save-mode 1)
(delete-selection-mode)

(if is-windows
    (setq default-directory "C:/Users/Grant/Dev/"))
(if is-mac
    (setq default-directory "~/"))
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "IN PROGRESS" "|" "DONE" "DELEGATED")))

;; Keybinds
(dolist (key '("\C-x C-b" "\M-z" "\M-o" "\C-c c" "\C-c s" "\M-n" "\M-p" "\M-s" "\M-r"))
  (global-unset-key (kbd key)))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)

;; Completion
(setq completion-styles '(initials partial-completion flex))
(setq completion-cycle-threshold 10)

;; Start Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Dired-x
(load "dired-x")

;; Kill ring
(use-package browse-kill-ring)

;; Org mode
(use-package org
  :defer t
  :init
  (setq org-support-shift-select 1))


;; Autocomplete brackets
(electric-pair-mode 1)
(show-paren-mode 1)
(setq electric-pair-preserve-balance nil)

;; Theme
(use-package spacemacs-theme
  :defer t
  :config
  (load-theme 'spacemacs-dark t))

;; Multiple cursors
(use-package multiple-cursors
  :defer 2
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C->" . 'mc/mark-all-like-this))

;; String Inflection
(defun my-string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ;; for python
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ;; for java
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      ;; default
      (string-inflection-ruby-style-cycle))))

(use-package string-inflection
  :config
  (global-unset-key (kbd "C-c s"))
  (global-set-key (kbd "C-c s") 'my-string-inflection-cycle-auto))

;; Magit
(use-package magit
  :bind ("C-x g" . 'magit-status))

;; Which key
(use-package which-key
  :diminish
  :config (which-key-mode))

;; yasnippet
(use-package yasnippet
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

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Ivy
(use-package counsel
  :diminish
  :after ivy
  :init
  (setq ivy-initial-inputs-alist nil)
  :config (counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> o" . counsel-describe-symbol)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-c m" . counsel-mark-ring)
         ("C-x x" . counsel-find-file)))

(use-package ivy
  :defer 1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-x" . ivy-switch-buffer)
         ("C-x M-x" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; Ag
(use-package ag)

;; Projectile
(use-package projectile
  :diminish
  :config
  (setq projectile-project-search-path '("~/repos/"))
  (setq projectile-switch-project-action 'projectile-dired)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Fix c indentation
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

;; Vterm
(use-package vterm)

;; Flycheck
(use-package flycheck
  :diminish
  :init (global-flycheck-mode))

;; Typescript
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
	web-mode-enable-auto-indentation nil
        js-indent-level 2
        ))

(use-package emmet-mode
  :commands emmet-mode
  :config
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'html-mode-hook #'emmet-mode))

;; Pug
(use-package pug-mode)

(setq gc-cons-threshold (* 2 1000 1000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(flycheck pug-mode counsel ivy markdown-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
