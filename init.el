;; Add MELPA repository for package installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq lsp-clojure-custom-server-command '("bash" "-c" "/opt/homebrew/bin/clojure-lsp"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-complete which-key elisp-format lsp-java lsp-ui magit company paredit cider clojure-mode lsp-mode lsp-treemacs flycheck)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Add Leiningen's path to Emacs' exec-path
(add-to-list 'exec-path "/opt/homebrew/bin/lein")

(setq cider-lein-command "/opt/homebrew/bin/lein")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline elisp-format projectile neotree lsp-ui lsp-java evil consult company cider))
 '(safe-local-variable-values
   '((cider-lein-parameters . "catalyst-repl :headless :host localhost"))))


;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)

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
(use-package elisp-format :ensure t)



;; go to reference
(define-key clojure-mode-map (kbd "C-c r") 'lsp-find-references)
(define-key clojure-mode-map (kbd "C-c d") 'lsp-find-definition)

;; Open Emacs in maximized mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Suppress warnings and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq warning-minimum-level :emergency)


;; Navigation between buffers
(global-set-key (kbd "C-x <right>") 'next-buffer)
(global-set-key (kbd "C-x <left>") 'previous-buffer)

;; Split windows and close windows
(global-set-key (kbd "C-x |") 'split-window-right)   ;; Split pane vertically
(global-set-key (kbd "C-x -") 'split-window-below)  ;; Split pane horizontally
(global-set-key (kbd "C-x 0") 'delete-window)       ;; Close current window
(global-set-key (kbd "C-x 1") 'delete-other-windows) ;; Close other windows

;; Navigation between windows
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)


(set-face-attribute 'default nil :height 140)



;; clj-kondo integration
(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-mode)


;; disable the annoying bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)



;; remove files when closing emacs
(defun clean-emacs-temp-files-on-exit ()
      "Delete all autosave and backup files on exit."
      (let ((autosave-files (directory-files "~/.emacs.d/autosaves/" t "\\`#.*#\\'"))
		  	            (backup-files (directory-files "~/" t "~$")))
      (mapc 'delete-file autosave-files)
      (mapc 'delete-file backup-files)))

(add-hook 'kill-emacs-hook 'clean-emacs-temp-files-on-exit)


;; LISP EDITING
(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  :bind (("C-c d" . paredit-forward-down))
  )

;; Ensure paredit is used EVERYWHERE!
(use-package paredit-everywhere
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'list-mode-hook #'paredit-everywhere-mode))


;; Auto Completion
(use-package company
  :bind (("C-x /". company-complete))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode)
  (push 'company-files company-backends)


(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("/opt/homebrew/bin/clojure-lsp")))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t)

(defun cider-eval-and-insert-comment ()
  "Evaluate the last expression and insert the result as a comment."
  (interactive)
  ;; Assuming `cider-eval-last-sexp` and appending as a comment is your goal
  (cider-eval-last-sexp '(1)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "C-c ;") 'cider-eval-and-insert-comment)))


(global-set-key (kbd "C-c ;") 'cider-eval-and-insert-comment)

