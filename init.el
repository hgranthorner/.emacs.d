;; load elpaca
(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(load-theme 'modus-vivendi-tinted t)

;; set custom settings
(setq tab-bar-show nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load custom-file)
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
(set-face-attribute 'default nil :height 160 :family "Ubuntu Mono")
(setq is-mac (string= system-type "darwin"))
(setq use-evil nil)

(setq env-changed nil)
(require 'treesit)

;; Add .asdf to exec-path
(when (file-exists-p (file-truename "~/.asdf"))
  (setq env-changed t)
  (push (file-truename "~/.asdf/shims") exec-path)
  (push (file-truename "~/.asdf/bin") exec-path))

;; Remove "/mnt/c/" for WSL PATH
(setq exec-path
      (cl-remove-if (lambda (s)
                      (and (< 5 (length s))
                           (string= (substring s 0 6) "/mnt/c")
                           (setq env-changed t)))
                    exec-path))

(when exec-path
  (setenv "PATH" (string-join exec-path ":")))

;; key bindings
(keymap-global-set "M-o"     #'other-window)
(keymap-global-set "M-i"     #'imenu)
(keymap-global-set "C-M-."   #'end-of-buffer)
(keymap-global-set "C-M-,"   #'beginning-of-buffer)
(keymap-global-set "C-."     #'xref-find-definitions)
(keymap-global-set "C-,"     #'xref-go-back)
(keymap-global-set "<f5>"    #'compile)
(keymap-global-set "C-c r r" #'revert-buffer)
(keymap-global-set "C-c C-c" #'comment-or-uncomment-region)
(keymap-global-set "M-]"     #'forward-paragraph)
(keymap-global-set "M-["     #'backward-paragraph)
(keymap-global-set "C-h h"   #'eldoc)
(keymap-global-set "C-]"     #'flymake-goto-next-error)
(keymap-global-set "C-c e i" #'hgh/visit-init-file)

;; Added for the defstar library in common lisp
(font-lock-add-keywords 'lisp-mode '("[[:word:]:]*def.*\\*"))

(defun hgh/visit-init-file ()
  (interactive)
  (find-file user-init-file))

(when is-mac
  (setq dired-use-ls-dired t
        insert-directory-program "gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(require 'use-package)
(require 'use-package-ensure)
(require 'bind-key)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :when is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package project
  :bind (("C-x p s" . hgh/project-ripgrep)
         ("C-x p S" . project-shell)))

(use-package diminish
  :after (evil evil-collection which-key)
  :config
  (diminish 'abbrev-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'auto-revert-mode))

(use-package haskell-mode)

(load (concat user-emacs-directory "hindent.el"))
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
(keymap-set hindent-mode-map "C-M-|" #'hindent-reformat-buffer)

(use-package mood-line
  :config
  (mood-line-mode))

(use-package magit
  :defer t)

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (org-mode . mixed-pitch-mode))

(use-package org
  :commands (org-capture org-agenda)
  :hook ((org-mode . org-indent-mode)
         (org-mode . hgh/org-mode-setup))
  :custom
  (org-agenda-files '("~/notes/roam"))
  (org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  :config
  (require 'org-agenda)
  (keymap-set org-agenda-mode-map "j" #'org-agenda-next-line)
  (keymap-set org-agenda-mode-map "k" #'org-agenda-previous-line))

(use-package diminish
  :after (evil evil-collection which-key)
  :config
  (diminish 'abbrev-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'auto-revert-mode))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package magit
  :defer t)

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (org-mode . mixed-pitch-mode))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :demand t
  :after org
  :bind (("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         ("C-c n t" . org-roam-buffer-toggle))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :custom
  (org-roam-complete-everywhere t)
  :init
  (unless (file-exists-p "~/notes")
    (make-directory "~/notes"))

  (unless (file-exists-p "~/notes/roam")
    (make-directory "~/notes/roam"))

  (unless (file-exists-p "~/notes/roam/daily")
    (make-directory "~/notes/roam/daily"))

  (setq org-roam-directory (file-truename "~/notes/roam"))
  (setq org-roam-dailies-directory "daily/")
  (org-roam-db-autosync-mode 1))

(use-package browse-kill-ring
  :bind (("C-M-y" . browse-kill-ring)))

(use-package ace-window
  :bind (("M-O" . ace-window)))

(use-package evil-mc
  :diminish
  :if use-evil
  :after evil
  :bind
  (("C->"   . evil-mc-make-and-goto-next-match)
   ("C-<"   . evil-mc-make-and-goto-prev-match)
   ("C-M->" . evil-mc-make-all-cursors))
  :init
  (global-evil-mc-mode 1)
  :config
  (keymap-set evil-mc-key-map "C-g" #'evil-mc-undo-all-cursors))

(use-package company
  :diminish
  :config
  (global-company-mode 0))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config
  (global-corfu-mode 1))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

(use-package cider
  :bind
  (:map cider-mode-map
        ("C-c i r" . cider-inspect-last-result)
        ("C-c i c" . cider-inspect-last-sexp))
  :init
  (setq cider-repl-display-help-banner nil))

(use-package sly
  :bind
  (:map sly-mode-map
        ("C-." . sly-edit-definition)
        ("C-," . sly-pop-find-definition-stack))
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (keymap-unset sly-mode-map "M-.")
  (keymap-unset sly-mode-map "M-,"))

(use-package yasnippet
  :diminish
  :defer t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package multiple-cursors
  :if (not use-evil)
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package evil
  :if use-evil
  :custom
  (evil-want-keybinding nil)
  (evil-move-beyond-eol t)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)

  (setq evil-leader "SPC")
  (evil-set-leader 'motion (kbd evil-leader))
  (evil-set-leader 'motion (kbd ",") 'local)

  (evil-define-key 'motion 'global (kbd "k") #'evil-previous-visual-line)
  (evil-define-key 'motion 'global (kbd "j") #'evil-next-visual-line)

  (evil-define-key 'motion 'global (kbd "C-.") nil)

  (evil-define-key 'motion 'global (kbd "<leader>SPC") #'project-find-file)
  (evil-define-key 'motion 'global (kbd "<leader>wh") #'evil-window-left)
  (evil-define-key 'motion 'global (kbd "<leader>wj") #'evil-window-down)
  (evil-define-key 'motion 'global (kbd "<leader>wk") #'evil-window-up)
  (evil-define-key 'motion 'global (kbd "<leader>wl") #'evil-window-right)
  (evil-define-key 'motion 'global (kbd "<leader>wd") #'delete-window)
  (evil-define-key 'motion 'global (kbd "<leader>ws") #'split-window-below)
  (evil-define-key 'motion 'global (kbd "<leader>wv") #'split-window-right)
  (evil-define-key 'motion 'global (kbd "K")          #'eldoc)

  (evil-define-key 'motion 'global (kbd "<leader>bb") #'switch-to-buffer)

  (evil-define-key 'motion 'global (kbd "<leader>fs") #'save-buffer)
  (evil-define-key 'motion 'global (kbd "<leader>ff") #'find-file)

  (evil-define-key 'motion 'global (kbd "<leader>h") help-map)

  (evil-define-key 'motion 'global (kbd "g]") #'flymake-goto-next-error)
  (evil-define-key 'motion 'global (kbd "g[") #'flymake-goto-prev-error)
  (evil-define-key 'motion 'global (kbd "ge") #'flymake-show-buffer-diagnostics)


  (evil-define-key 'motion 'emacs-lisp-mode-map (kbd "<localleader>e") #'eval-last-sexp)
  (evil-define-key 'motion 'lisp-mode-map (kbd "<localleader>sf") #'paredit-forward-slurp-sexp)
  (evil-define-key 'motion 'lisp-mode-map (kbd "<localleader>sb") #'paredit-backward-slurp-sexp)
  (evil-define-key 'motion 'lisp-mode-map (kbd "<localleader>bf") #'paredit-forward-barf-sexp)
  (evil-define-key 'motion 'lisp-mode-map (kbd "<localleader>bb") #'paredit-backward-barf-sexp)
  (evil-define-key 'motion 'lisp-mode-map (kbd "<localleader>e") #'sly-eval-last-expression))

(use-package evil-collection
  :if use-evil
  :after (evil)
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(defun hgh/project-ripgrep (regexp)
  (interactive (list (read-from-minibuffer "Search (regexp): " (thing-at-point 'symbol))))
  (ripgrep-regexp regexp (project-root (project-current))))

(defun hgh/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.1)
                  (org-level-2 . 1.75)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.25)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Ubuntu Mono" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun hgh/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun hgh/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . hgh/org-mode-visual-fill))

(use-package elixir-ts-mode
  :defer t
  :config
  (global-subword-mode 1))

(use-package ripgrep
  :defer t)

(use-package paredit
  :diminish
  :defer t)

(use-package web-mode
  :defer t
  :init
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\.svelte" . svelte-mode)))

(use-package eglot
  :hook
  ((clojure-mode            . eglot-ensure)
   (go-mode                 . eglot-ensure)
   (rust-ts-mode            . eglot-ensure)
   (typescript-ts-base-mode . eglot-ensure)
   (elixir-ts-mode          . eglot-ensure)
   (heex-ts-mode            . eglot-ensure)
   (java-ts-mode            . eglot-ensure)
   (svelte-mode             . eglot-ensure)
   (haskell-mode            . eglot-ensure))
  :bind
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c a" . eglot-code-actions)
        ("C-c f" . eglot-format-buffer)
        ("C-c d" . xref-find-definitions))
  :init
  (setq eglot-events-buffer-size 0)
  :config
  ;; Set up using clippy with rust analyzer
  (setf eglot-server-programs
        (cl-remove-if (lambda (c) (equal (car c) 'rust-mode))
                      eglot-server-programs))

  (setf eglot-server-programs (cons (list '(rust-ts-mode rust-mode) "rust-analyzer" :initializationOptions '(:checkOnSave (:command "clippy")))
                                    eglot-server-programs))

  (setf eglot-server-programs (cons '(svelte-mode "svelteserver" "--stdio")
                                    eglot-server-programs))

  (setf eglot-server-programs (cons '(haskell-mode "haskell-language-server-wrapper" "--lsp")
                                    eglot-server-programs)))

;; setting up syntaxes documented here: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
(setq treesit-extra-load-path '("~/src/github.com/casouri/tree-sitter-module/dist"))

;; paredit

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(defun setup-paredit ()
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

(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)
            (setup-paredit)))
