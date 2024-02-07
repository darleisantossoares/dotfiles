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
                                                                                                                                                                                                  47,15         Top
