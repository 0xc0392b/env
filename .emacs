;; --------------------------------------------------------------------------------
;; Interface

;; Don't show default startup screen
(setq inhibit-startup-screen t)

;; Turn off system bell
(setq ring-bell-function 'ignore)

;; Open scratch file on startup
(find-file "scratch.md")

;; 80 character margin in code buffers
;; TODO

;; Always show line numbers and git gutter
(global-display-line-numbers-mode)
(global-git-gutter-mode 1)

;; Hide toolbars
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Set default font
(set-frame-font "Inconsolata")

;; Custom key bindings
(global-set-key (kbd "C-\\") 'treemacs) ; toggle treemacs with C-\
(global-set-key (kbd "C-`") 'auto-complete-mode) ; toggle auto-complete with C-`
(global-set-key (kbd "C-c m c") 'mc/edit-lines) ; multiple cursors

;; Load theme
(require 'kaolin-themes)
(load-theme 'kaolin-eclipse t)


;; --------------------------------------------------------------------------------
;; Package management

;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; --------------------------------------------------------------------------------
;; language-specific configuration

;; Preview LaTeX files
(latex-preview-pane-enable)

;; Auto go fmt on save buffer
(add-hook 'before-save-hook 'gofmt-before-save)

;; SLE project: syntax highlighting for ART eSOS rules and mux DSL
(add-to-list 'load-path "~/Documents/RHUL/rhul-year-3/Term 2/Software Language Engineering/Project")
(load "esos-mode.el")
(load "mux-mode.el")
(add-to-list 'auto-mode-alist '("\\.esos\\'" . esos-mode))
(add-to-list 'auto-mode-alist '("\\.mux\\'" . mux-mode))


;; --------------------------------------------------------------------------------
;; Misc

;; Auto-generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "aa")
 '(git-gutter:deleted-sign "rr")
 '(git-gutter:modified-sign "mm")
 '(package-selected-packages
   '(elm-mode multiple-cursors go-mode auto-complete latex-preview-pane git-gutter haskell-mode treemacs kaolin-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
