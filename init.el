{:paths ["src" "resources"]
Last login: Mon Feb  5 23:55:19 on ttys005
darlei.soares@darlei ~ % cd dev/me
darlei.soares@darlei me % ls
clojure-algorithms
darlei.soares@darlei me %
darlei.soares@darlei me % git clone git@github.com:darleisantossoares/clojure-sandbox.git
Cloning into 'clojure-sandbox'...
remote: Enumerating objects: 3, done.
remote: Counting objects: 100% (3/3), done.
remote: Compressing objects: 100% (2/2), done.
remote: Total 3 (delta 0), reused 0 (delta 0), pack-reused 0
Receiving objects: 100% (3/3), done.
darlei.soares@darlei me %
darlei.soares@darlei me %
pom.xml
darlei.soares@darlei me %
darlei.soares@darlei me %
darlei.soares@darlei me % cd clojure-
cd: no such file or directory: clojure-
darlei.soares@darlei me % cd clojure-sandbox
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % ls
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % touch deps.edn
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % mkdir src
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % vim deps.edn
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % cd src
darlei.soares@darlei src % mkdir sandbox
darlei.soares@darlei src %
darlei.soares@darlei src % cd sandbox
darlei.soares@darlei sandbox %
darlei.soares@darlei sandbox %
darlei.soares@darlei sandbox % pwd
/Users/darlei.soares/dev/me/clojure-sandbox/src/sandbox
darlei.soares@darlei sandbox %
darlei.soares@darlei sandbox %
darlei.soares@darlei sandbox % gpwd
/Users/darlei.soares/dev/me/clojure-sandbox/src/sandbox
darlei.soares@darlei sandbox %
darlei.soares@darlei sandbox % cd ../..
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % ls
deps.edn	src
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % git status
On branch main
pom.xml
Your branch is up to date with 'origin/main'.

Untracked files:
  (use "git add <file>..." to include in what will be committed)
	.clj-kondo/
	.lsp/
	deps.edn
	src/

nothing added to commit but untracked files present (use "git add" to track)
darlei.soares@darlei clojure-sandbox % touch .gitignore
darlei.soares@darlei clojure-sandbox % vim .git
darlei.soares@darlei clojure-sandbox % vim .gitignore
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % git status
On branch main
Your branch is up to date with 'origin/main'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)
	modified:   .gitignore

Untracked files:
  (use "git add <file>..." to include in what will be committed)
	deps.edn
	src/

no changes added to commit (use "git add" and/or "git commit -a")
darlei.soares@darlei clojure-sandbox %
darlei.soares@darlei clojure-sandbox % git add .
darlei.soares@darlei clojure-sandbox % git commit -m "add gitignore"
[main d77d2f3] add gitignore
 4 files changed, 10 insertions(+)
 create mode 100644 deps.edn
 create mode 100644 src/sandbox/clojure_core_fns.clj
 create mode 100644 src/sandbox/clojure_core_fns.clj~
darlei.soares@darlei clojure-sandbox % git push
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
