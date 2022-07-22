(define-key key-translation-map "\C-t" "\C-x")
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-c o") 'previous-buffer)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq-default line-spacing 1)

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )


(set-face-attribute 'default nil :font "Hack Nerd Font" :height 160)

(column-number-mode)
; (global-display-line-numbers-mode t)
(add-hook 'prog-mode-hook #'(lambda () (setq display-line-numbers t)))

(dolist (mode '(org-mode-hook
		        term-mode-hook
				shell-mode-hook
		        eshell-mode-hook))
  (add-hook mode (lambda () (global-display-line-numbers-mode 0))))

(setq-default tab-width 4)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			             ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package unicode-fonts)

(setq visible-bell nil)
(setq ring-bell-function (lambda ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPRIVATE"))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package horizon-theme)
(use-package mood-line
  :config
  (mood-line-mode))

(use-package all-the-icons
  :config
  (all-the-icons-install-fonts t))


(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (python-mode . lsp-deferred))


(defun wv/company-backend-with-yas (backends)
      "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backends) (memq 'company-yasnippet backends))
	  backends
	(append (if (consp backends)
		    backends
		  (list backends))
		'(:with company-yasnippet))))

(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  (setq company-backends (mapcar #'wv/company-backend-with-yas company-backends))
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

;; provides visual help in the buffer
;; For example definitions on hover.
;; The `imenu` lets me browse definitions quickly.
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable t
		lsp-ui-sideline-show-diagnostics t
		lsp-ui-sideline-delay 0.4
	    lsp-ui-doc-delay 2)
exec-path-from-shell-variables
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
			  ("C-c i" . lsp-ui-imenu)
			  ("C-c h" . lsp-ui-doc-show)))


 
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(setq lsp-ui-doc-delay 0)
(setq lsp-ui-doc-position 'at-point)


; Golang configuration
(use-package go-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)




;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "python3"))


;; Required to easily switch virtual envs 
;; via the menu bar or with `pyvenv-workon` 
;; Setting the `WORKON_HOME` environment variable points 
;; at where the envs are located. I use miniconda. 
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.pyvenv"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

; Language server for Python 
; Read the docs for the different variables set in the config.
(use-package lsp-pyright
  :ensure t
  :defer t
  :config
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-disable-organize-imports nil
	lsp-pyright-auto-import-completions t
	lsp-pyright-use-library-code-for-types t)
  :hook ((python-mode . (lambda () 
                          (require 'lsp-pyright) (lsp-deferred)))))



(use-package blacken
  :custom (blacken-skip-string-normalization t)
  :hook (python-mode . blacken-mode))




(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("c-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(require 'ivy)


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-switch-buffer)
		 ("C-x C-f" . counsel-find-file)
		 :map minibuffer-local-map
		 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-horizon t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package general
  :config
  (general-create-definer wv/leader-keys
	:keymaps '(normal visual insert emacs)
	:prefix ","
	:global-prefix "C-,")

  (wv/leader-keys
   "g" '(:ignore t :which-key "go to")
   "gd" '(lsp-find-definition :which-key "definition")
   "gr" '(lsp-find-references :which-key "references")
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "n" '(:ignore t :which-key "window")
   "nn" '(evil-window-next :which-key "next")
   "np" '(evil-window-prev :which-key "previous")
   "nc" '(evil-window-delete :which-key "close")))

(defun wv/evil-hook ()
  (dolist (mode '(custom-mode
				  eshell-mode
				  git-rebase-mode
				  term-mode))
	(add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
;  :hook ((evil-mode . wv/evil-hook))
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "gd" 'lsp-find-definition)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package forge
  :after magit)


(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("0" #'(lambda () (text-scale-set 0)) "reset")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-scale (:timeout 4)
  "scale window"
  ("j" enlarge-window "increase v")
  ("k" shrink-window "decrease v")
  ("h" (shrink-window-horizontally 2) "decrease h")
  ("l" (enlarge-window-horizontally 2) "increase h"))

  ("f" nil "finished" :exit t))

(wv/leader-keys
  "ss" '(hydra-text-scale/body :which-key "scale text")
  "sw" '(hydra-window-scale/body :which-key "scale window"))


(use-package tree-sitter
  :defer t
  :hook
  ((go-mode python-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs)


(use-package projectile
  :diminish (projectile-mode)
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/projects" "~/projects/sentinel"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile counsel
  :config (counsel-projectile-mode t))

(use-package treemacs)
(use-package treemacs-projectile)


(use-package magit
  :after evil-collection
  :config
  (evil-collection-magit-setup)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(defun wv/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (display-line-numbers-mode nil)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . wv/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
		org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-todo-keywords
		'((sequence "BACKLOG(b)" "PLANNED(p)" "WORKING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-files '("~/org-wv/tasks.org")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(defun wv/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :after org
  :hook (org-mode . wv/org-mode-visual-fill))

(use-package with-venv)

(use-package dap-mode
  :after with-venv hydra
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-python)
  (require 'dap-dlv-go)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(wv/leader-keys
  "d" '(:ignore t :which-key "debug")
  "dr" '(dap-debug :which-key "run")
  "db" '(dap-breakpoint-toggle :which-key "toggle breakpoint"))

(wv/leader-keys
  "t" '(:ignore t :which-key "treemacs")
  "tt" '(treemacs :which-key "toggle")
  "tp" '(:ignore t :which-key "project")
  "tpc" '(treemacs-collapse-project :which-key "collapse")
  "tpa" '(treemacs-add-project-to-workspace :which-key "add project to workspace")



(use-package protobuf-mode)
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))
(use-package yasnippet-snippets)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" default))
 '(package-selected-packages
   '(treemacs-projectile unicode-fonts yasnippet-snippets protobuf-mode blacken with-venv dap-mode counsel-projectile visual-fill-column org-bullets forge magit evil-magit lsp-pylsp lsp-python-ms tree-sitter-langs tree-sitter projectile hydra evil-collection evil general all-the-icons mood-line elpy doom-themes helpful ivy-rich flycheck exec-path-from-shell company company-mode lsp-ui which-key lsp-mode go-mode counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
