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
(electric-pair-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; fixes c indentation
(setq c-default-style "bsd"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil)

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


(setq hg/packages '(exec-path-from-shell
		    magit
		    browse-kill-ring
		    company
		    multiple-cursors
                    rust-mode
                    eglot
                    which-key
                    ;lsp-mode
                    cider
                    sly
                    paredit))
(hg/sync-packages hg/packages)

(load custom-file)

;; exec path from shell for mac
(exec-path-from-shell-initialize)

;; package settings
(setq cider-repl-display-help-banner nil)
(which-key-mode)
(fido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-enable-prefix t)
(global-company-mode 1)
(setq mc/always-run-for-all t)
(setq inferior-lisp-program "sbcl")

;; key bindings
(global-set-key (kbd "C-M-y") #'browse-kill-ring)
(global-set-key (kbd "M-o")   #'other-window)
(global-set-key (kbd "M-i")   #'imenu)
(global-set-key (kbd "C-M-.") #'end-of-buffer)
(global-set-key (kbd "C-M-,") #'beginning-of-buffer)

;; company
(with-eval-after-load "company"
  (define-key company-mode-map (kbd "M-,") #'company-complete))

;; lsp/eglot
(defun hg/setup-lsp ()
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (setq lsp-display-inline-image nil)
  (setq lsp-lens-enable nil)

  (with-eval-after-load "lsp-mode"
    (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
    (define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
    (define-key lsp-mode-map (kbd "C-c d") #'lsp-find-definition)))

(defun hg/setup-eglot ()
  (add-hook 'clojure-mode-hook #'eglot-ensure)
  (with-eval-after-load "eglot"
    (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer)
    (define-key eglot-mode-map (kbd "C-c d") #'xref-find-definitions)
    (define-key eglot-mode-map (kbd "C-c h") #'eldoc)))

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
                        (clojure-mode-hook clojure-mode)))

(dolist (hook lisp-mode-hooks)
  (add-hook (car hook) #'enable-paredit-mode))

(with-eval-after-load "cider"
  (define-key cider-mode-map (kbd "C-c i r") #'cider-inspect-last-result)
  (define-key cider-mode-map (kbd "C-c i c") #'cider-inspect-last-sexp))
