;; Add MELPA repository for package installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq package-selected-packages '(clojure-mode lsp-mode cider lsp-treemacs flycheck company))

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Highlight current line.
(global-hl-line-mode t)
(global-display-line-numbers-mode)
;(set-frame-font "JetBrains Mono-12" nil t)


(when (cl-find-if-not #'package-installed-p package-selected-packages)
    (package-refresh-contents)
      (mapc #'package-install package-selected-packages))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
    ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
    ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
 )

;; Add Leiningen's path to Emacs' exec-path
(add-to-list 'exec-path "/opt/homebrew/bin/lein")

(setq cider-lein-command "/opt/homebrew/bin/lein")



;; Use-package for easier package management
(unless (package-installed-p 'use-package)
      (package-install 'use-package))
(require 'use-package)

;; Clojure Mode
(use-package clojure-mode
  :ensure t)

;; Paredit
(use-package paredit
  :ensure t
  :hook (clojure-mode . paredit-mode))

;; Custom paredit shortcuts
(defun setup-paredit-shortcuts ()
      (define-key paredit-mode-map (kbd "C-c >") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "C-c <") 'paredit-forward-barf-sexp)
	            (define-key paredit-mode-map (kbd "C-c M->") 'paredit-backward-slurp-sexp)
		    	  (define-key paredit-mode-map (kbd "C-c M-<") 'paredit-backward-barf-sexp))
(add-hook 'paredit-mode-hook 'setup-paredit-shortcuts)

(use-package clojure-mode
  :ensure t
  :after lsp-mode)
(use-package lsp-mode
  :defer t
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :defer t
  :ensure t)
(use-package lsp-treemacs
  :defer t
  :ensure t)
(use-package lsp-java
  :defer t
  :ensure t
  :config
  (add-hook 'java-mode-hook #'lsp))
(use-package elisp-format :ensure t)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package auto-complete
  :defer t
  :ensure t
  :config
  (add-to-list 'ac-modes 'nrepl-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-evil
  :after (treemacs evil)
  :defer t
  :ensure t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :defer t
  :ensure t
  :custom
  (add-hook 'projectile-after-switch-project-hook 'treemacs-add-and-display-current-project))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :defer t
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode))


;; ============================ LSP
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-semantic-tokens-enable t
      ;; inhibit-startup-message t ; Do not show the startup screen.
      ;; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ;; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )


;; go to reference
(define-key clojure-mode-map (kbd "C-c r") 'lsp-find-references)
(define-key clojure-mode-map (kbd "C-c d") 'lsp-find-definition)


;; Evil Mode
(use-package evil
  :ensure t
  :config (evil-mode 1))

;; Open Emacs in maximized mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Suppress warnings and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq warning-minimum-level :emergency)


;; Projectile
(use-package projectile             ; Find file in project (ala Vim's CTRL-P or Textmate's Cmd-T)
             :ensure t
             :init
             (projectile-mode +1)
             :bind (:map projectile-mode-map
                         ("s-p" . projectile-command-map)
                         ("C-c p" . projectile-command-map))
             :config
             (setq projectile-project-search-path '("/Users/darlei.soares/dev/nu/")
           projectile-remember-window-configs nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline elisp-format projectile neotree lsp-ui lsp-java evil consult company cider))
 '(safe-local-variable-values
   '((cider-lein-parameters . "catalyst-repl :headless :host localhost"))))

;; Navigation between buffers
(global-set-key (kbd "C-x <right>") 'next-buffer)
(global-set-key (kbd "C-x <left>") 'previous-buffer)

;; Split windows and close windows
(global-set-key (kbd "C-x |") 'split-window-right)   ;; Split pane vertically
(global-set-key (kbd "C-x -") 'split-window-below)  ;; Split pane horizontally
(global-set-key (kbd "C-x 0") 'delete-window)       ;; Close current window
(global-set-key (kbd "C-x 1") 'delete-other-windows) ;; Close other windows

;; Navigation between windows
(global-set-key (kbd "C-w <left>") 'windmove-left)
(global-set-key (kbd "C-w <right>") 'windmove-right)
(global-set-key (kbd "C-w <up>") 'windmove-up)
(global-set-key (kbd "C-w <down>") 'windmove-down)

;; clj-kondo integration
(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-mode)

;; Set default font and size
;(set-face-attribute 'default nil :font "Monospace" :height 220)

(add-hook 'window-setup-hook (lambda () (set-face-attribute 'default nil :height 220)))

;; disable the annoying bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)


;; remove files when closing emacs
(defun clean-emacs-temp-files-on-exit ()
      "Delete all autosave and backup files on exit."
            (let ((autosave-files (directory-files "~/.emacs.d/autosaves/" t "\\`#.*#\\'"))
		  	            (backup-files (directory-files "~/" t "~$")))
	      	    (mapc 'delete-file autosave-files)
		    	        (mapc 'delete-file backup-files)))

(add-hook 'kill-emacs-hook 'clean-emacs-temp-files-on-exit)




