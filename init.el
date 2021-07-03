(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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

(use-package diminish
  :init
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

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
(global-linum-mode 1)
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
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
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
(setq org-support-shift-select 1)

;; Autocomplete brackets
(electric-pair-mode 1)
(show-paren-mode 1)
(setq electric-pair-preserve-balance nil)

;; Theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C->" . 'mc/mark-all-like-this))

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
         ("C-c m" . counsel-mark-ring)))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; Projectile
(use-package projectile
  :diminish
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

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

;; Flycheck
(use-package flycheck
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
