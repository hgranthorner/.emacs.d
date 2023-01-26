;; set up melpa
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(package-initialize)

;; set custom settings
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq custom-file (concat user-emacs-directory "custom.el"))
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
(when (file-exists-p (file-truename "~/.asdf"))
  (push (file-truename "~/.asdf/shims") exec-path)
  (push (file-truename "~/.asdf/bin") exec-path))

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

(defun hg/fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

(defun hgh/visit-init-file ()
  (interactive)
  (find-file user-init-file))

(when is-mac
  (setq dired-use-ls-dired t
        insert-directory-program "gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(require 'use-package)
(require 'use-package-ensure)
(require 'bind-key)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :when is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package diminish
  :after (evil evil-collection which-key)
  :config
  (diminish 'abbrev-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'auto-revert-mode))

(use-package gruvbox-theme
  :config
  ;(load-theme 'gruvbox-dark-hard)
  )

(use-package dracula-theme
  :config
  (load-theme 'dracula))

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
  :defer 1
  :pin org
  :commands (org-capture org-agenda)
  :hook ((org-mode . org-indent-mode)
         (org-mode . hgh/org-mode-setup))
  :init
  (setq org-agenda-files '("~/notes/roam"))
  (setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :after org
  :bind (("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         ("C-c n t" . org-roam-buffer-toggle))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :custom
  (org-roam-complete-everywhere t)
  :init
  (unless (and (file-exists-p "~/notes")
               (file-exists-p "~/notes/roam")
               (file-exists-p "~/notes/roam/daily"))
    (make-directory "~/notes")
    (make-directory "~/notes/roam")
    (make-directory "~/notes/roam/daily"))

  (setq org-roam-directory (file-truename "~/notes/roam"))
  (setq org-roam-dailies-directory "daily/")
  (org-roam-db-autosync-mode 1))

(use-package browse-kill-ring
  :bind (("C-M-y" . browse-kill-ring)))

(use-package evil-mc
  :diminish
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
  (global-company-mode 1))

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

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-move-beyond-eol t)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-collection-init)
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
  :diminish evil-collection-unimpaired-mode)

(use-package vertico
  :config
  (vertico-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(defun hgh/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
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
  (variable-pitch-mode 1)
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
  ((clojure-mode       . eglot-ensure)
   (go-mode            . eglot-ensure)
   (rust-ts-mode       . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (elixir-ts-mode     . eglot-ensure)
   (heex-ts-mode       . eglot-ensure)
   (java-ts-mode       . eglot-ensure)
   (svelte-mode        . eglot-ensure))
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
  (setf (cdr (assoc '(rust-ts-mode rust-mode) eglot-server-programs))
        (list "rust-analyzer" :initializationOptions '(:checkOnSave (:command "clippy"))))
  (setf eglot-server-programs (cons '(svelte-mode "svelteserver" "--stdio") eglot-server-programs)))

;; setting up syntaxes documented here: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
(setq treesit-extra-load-path '("~/src/github.com/casouri/tree-sitter-module/dist"))

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

(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))
