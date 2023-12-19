
;; Add MELPA repository for package installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)



(setq package-selected-packages '(clojure-mode lsp-mode cider lsp-treemacs flycheck company))

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


(use-package lsp-mode
	     	       :ensure t
		       	         :init
				 		   (setq lsp-enable-indentation nil) ;; if you have another preferred formatting tool
						   		     (setq lsp-enable-snippet nil)     ;; if you don't have yasnippet installed
								     		       :hook (clojure-mode . lsp)
										       		         :commands lsp)

(use-package lsp-ui
	     	       :commands lsp-ui-mode)

(use-package lsp-mode
	     	       :ensure t
		       	         :commands (lsp lsp-deferred)
				 		   :hook (clojure-mode . lsp)
						   		     :config
								     		       ;; add more LSP configurations here, if needed
										       		       )

(use-package clojure-mode
	     	       :ensure t
		       	         :after lsp-mode)


(use-package company
	     	       :ensure t
		       	         :config
				 		   (global-company-mode))


;; go to reference
(define-key clojure-mode-map (kbd "C-c r") 'lsp-find-references)
(define-key clojure-mode-map (kbd "C-c d") 'lsp-find-definition)


;; Evil Mode
(use-package evil
	     	       :ensure t
		       	         :config
				 		   (evil-mode 1))

;; Open Emacs in maximized mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Suppress warnings and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq warning-minimum-level :emergency)


;; Projectile
(use-package projectile
	     	       :ensure t
		       	         :config
				 		   (projectile-mode +1)
						   		     (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
(set-face-attribute 'default nil :font "Monospace" :height 220)

(add-hook 'window-setup-hook
	  	            (lambda () (set-face-attribute 'default nil :height 220)))


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




