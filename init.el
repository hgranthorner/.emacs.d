;; set up melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa" . 10)
				                   ("gnu" . 5)
				                   ("nongnu" . 2)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(package-initialize)

;; set custom settings
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq mac-command-modifier 'ctrl)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; fixes c indentation
(setq c-default-style "bsd"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq reb-re-syntax 'string)
(setq default-tab-width 4)
(setq-default tab-width 4)

;; Added for the defstar library in common lisp
(font-lock-add-keywords 'lisp-mode '("[[:word:]:]*def.*\\*"))

;; define custom functions
(defun hg/sync-packages (packages)
  "Install any missing packages on startup."
  (dolist (package hg/packages)
    (when (not (package-installed-p package))
      (package-install package))))

(defun hg/fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

(defun hg/refresh-projects ()
  (project--read-project-list))

;; refresh and load packages
(package-refresh-contents 'async)


(setq hg/packages '(exec-path-from-shell
                    gruvbox-theme
                    magit
                    browse-kill-ring
                    company
                    multiple-cursors
                    rust-mode
                    eglot
                    which-key
                    lsp-mode
                    cider
                    sly
                    yasnippet
                    yasnippet-snippets
                    go-mode
                    nand2tetris
                    evil
                    evil-collection
                    vertico
                    marginalia
                    orderless
                    org
                    paredit))
(hg/sync-packages hg/packages)

(load custom-file)

(set-face-attribute 'default nil :height 140)

;; exec path from shell for mac
(exec-path-from-shell-initialize)

;; package settings
(load-theme 'gruvbox-dark-hard)

(setq cider-repl-display-help-banner nil)
(setq mc/always-run-for-all t)
(setq inferior-lisp-program "sbcl")
(setq enable-evil nil)

(when enable-evil
  (setq evil-want-keybinding nil)
  (setq evil-move-beyond-eol t)
  (require 'evil))

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(which-key-mode)
(vertico-mode)
(marginalia-mode)
(yas-global-mode 1)
(add-to-list 'auto-mode-alist '("\.hdl" . nand2tetris-mode))
(global-company-mode 1)

;; key bindings
(global-set-key (kbd "C-M-y")   #'browse-kill-ring)
(global-set-key (kbd "M-o")     #'other-window)
(global-set-key (kbd "M-i")     #'imenu)
(global-set-key (kbd "C-M-.")   #'end-of-buffer)
(global-set-key (kbd "C-M-,")   #'beginning-of-buffer)
(global-set-key (kbd "C-.")     #'xref-find-definitions)
(global-set-key (kbd "C-,")     #'xref-go-back)
(global-set-key (kbd "<f5>")    #'compile)
(global-set-key (kbd "C-c r r") #'revert-buffer)
(global-set-key (kbd "M-]")     #'forward-paragraph)
(global-set-key (kbd "M-[")     #'backward-paragraph)
(global-set-key (kbd "C-h h")   #'eldoc)
(global-set-key (kbd "C-]")     #'flymake-goto-next-error)

;; lsp/eglot
(defun hg/setup-lsp ()
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-display-inline-image nil)
  (setq lsp-lens-enable nil)

  (with-eval-after-load "lsp-mode"
    (lsp-enable-which-key-integration t)
    (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
    (define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
    (define-key lsp-mode-map (kbd "C-c d") #'lsp-find-definition)
    (define-key lsp-mode-map (kbd "C-c k") #'lsp-find-references)))

(defun hg/setup-eglot ()
  (add-hook 'clojure-mode-hook #'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'rust-mode-hook #'eglot-ensure)
  (setq eglot-events-buffer-size 0)
  (with-eval-after-load "eglot"
    (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer)
    (define-key eglot-mode-map (kbd "C-c d") #'xref-find-definitions)))

(setq use-eglot t)
(if use-eglot
    (hg/setup-eglot)
  (hg/setup-lsp))

;; multiple cursors
(global-set-key (kbd "C-M->")	#'mc/edit-lines)
(global-set-key (kbd "C->")     #'mc/mark-next-like-this)
(global-set-key (kbd "C-<")	#'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") #'mc/mark-all-like-this)
;;; makes it so that you need to "C-g" to get out of multiple cursor
;;; mode, return inserts a new line.
(with-eval-after-load "multiple-cursors"
  (keymap-set mc/keymap "<return>" nil))

;; paredit

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(with-eval-after-load "paredit"
  (setcdr paredit-mode-map nil)
  (define-key paredit-mode-map (kbd "M-.") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-,") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M->") #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-<") #'paredit-backward-barf-sexp))

(setq lisp-mode-hooks '((emacs-lisp-mode-hook emacs-lisp-mode)
                        (clojure-mode-hook clojure-mode)
                        (lisp-mode-hook lisp-mode)
                        (sly-mode-hook sly-mode)))

(dolist (hook lisp-mode-hooks)
  (add-hook (car hook) #'enable-paredit-mode))

(with-eval-after-load "cider"
  (define-key cider-mode-map (kbd "C-c i r") #'cider-inspect-last-result)
  (define-key cider-mode-map (kbd "C-c i c") #'cider-inspect-last-sexp))

(add-hook 'lisp-mode-hook
		  (lambda ()
			(set (make-local-variable 'lisp-indent-function)
				 'common-lisp-indent-function)))

(with-eval-after-load "sly"
  (keymap-unset sly-mode-map "M-.")
  (keymap-unset sly-mode-map "M-,")
  (define-key sly-mode-map (kbd "C-.") #'sly-edit-definition)
  (define-key sly-mode-map (kbd "C-,") #'sly-pop-find-definition-stack))

;; evil

(with-eval-after-load "evil"
  (evil-mode 1)
  (evil-collection-init)
  (setq evil-leader "SPC")
  (evil-set-leader 'motion (kbd evil-leader))
  (evil-set-leader 'motion (kbd ",") 'local)

  (evil-define-key 'motion 'global (kbd "k") #'evil-previous-visual-line)
  (evil-define-key 'motion 'global (kbd "j") #'evil-next-visual-line)

  (evil-define-key 'motion 'global (kbd "C-.") nil)

  (evil-define-key 'motion 'global (kbd "<leader>wh") #'evil-window-left)
  (evil-define-key 'motion 'global (kbd "<leader>wj") #'evil-window-down)
  (evil-define-key 'motion 'global (kbd "<leader>wk") #'evil-window-up)
  (evil-define-key 'motion 'global (kbd "<leader>wl") #'evil-window-right)
  (evil-define-key 'motion 'global (kbd "<leader>wd") #'delete-window)
  (evil-define-key 'motion 'global (kbd "<leader>ws") #'split-window-below)
  (evil-define-key 'motion 'global (kbd "<leader>wv") #'split-window-right)

  (evil-define-key 'motion 'global (kbd "<leader>fs") #'save-buffer)
  (evil-define-key 'motion 'global (kbd "<leader>ff") #'find-file)

  (evil-define-key 'motion 'global (kbd "<leader>h") help-map)

  (evil-define-key 'motion 'emacs-lisp-mode-map (kbd "<localleader>e") #'eval-last-sexp))
