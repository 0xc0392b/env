;; William's emacs config~
;; last updated 7th March 2021


;; --------------------------------------------------------------------------------
;; Interface


;; don't show default startup screen
(setq inhibit-startup-screen t)

;; turn off system bell
(setq ring-bell-function 'ignore)

;; open scratch file on startup (this should already exist)
(find-file "scratch.md")

;; 80 character margin in code buffers
;; TODO

;; always show line numbers and git gutter
(global-display-line-numbers-mode)
(global-git-gutter-mode 1)

;; hide toolbars
(menu-bar-mode 0)
(tool-bar-mode 0)

;; set default font (this should already be a system font)
;; note: using Inconsolata version >= 3.001 requires emacs to be compiled --with-cairo
;;       instead of Xft. see https://github.com/googlefonts/Inconsolata/issues/42
(set-frame-font "Inconsolata")

;; load theme
(require 'kaolin-themes)
(load-theme 'kaolin-eclipse t)

;; custom key bindings
(global-set-key (kbd "C-\\") 'treemacs)          ; toggle treemacs with C-\
(global-set-key (kbd "C-`") 'auto-complete-mode) ; toggle auto-complete with C-`
(global-set-key (kbd "C-c m c") 'mc/edit-lines)  ; multiple cursors


;; --------------------------------------------------------------------------------
;; Package management


;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list of packages i use
(setq package-list '(treemacs multiple-cursors latex-preview-pane
			      kaolin-themes haskell-mode go-mode git-gutter
			      elm-mode auto-complete))

;; activate all packages
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; --------------------------------------------------------------------------------
;; language-specific configurations


;; preview LaTeX files
(latex-preview-pane-enable)

;; auto go fmt on save golang buffers
(add-hook 'before-save-hook 'gofmt-before-save)

;; SLE project: syntax highlighting for ART eSOS rules and mux DSL
;; note: this is temporary
;; note: you will need to make sure the path below 1. exists and 2. contains the two
;;       following .el scripts
(add-to-list 'load-path "~/Documents/RHUL/rhul-year-3/Term 2/Software Language Engineering/Project")
(load "esos-mode.el")
(load "mux-mode.el")
(add-to-list 'auto-mode-alist '("\\.esos\\'" . esos-mode))
(add-to-list 'auto-mode-alist '("\\.mux\\'" . mux-mode))


;; --------------------------------------------------------------------------------
;; Misc


;; auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "aa")
 '(git-gutter:deleted-sign "rr")
 '(git-gutter:modified-sign "mm")
 '(package-selected-packages
   '(elm-mode multiple-cursors go-mode auto-complete latex-preview-pane
	      git-gutter haskell-mode treemacs kaolin-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
