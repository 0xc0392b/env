;; William's emacs config~
;; last updated 16th July 2021


;; --------------------------------------------------------------------------------
;; Interface


;; don't show default startup screen
(setq inhibit-startup-screen t)

;; turn off system bell
(setq ring-bell-function 'ignore)

;; open scratch file on startup (this should already exist)
(find-file "~/.scratch.md")

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
(load-theme 'kaolin-dark t)

;; custom key bindings
(global-set-key (kbd "C-\\") 'treemacs)          ; toggle treemacs with C-\
(global-set-key (kbd "C-`") 'auto-complete-mode) ; toggle auto-complete with C-`


;; --------------------------------------------------------------------------------
;; Package management


;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list of packages i use
(setq package-list '(treemacs kaolin-themes haskell-mode go-mode
			      git-gutter elm-mode auto-complete markdown-mode))

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


;; auto go fmt when saving golang buffers
(add-hook 'before-save-hook 'gofmt-before-save)

;; auto elm-format when saving elm buffers (requires npm i -g elm-format)
(add-hook 'elm-mode-hook 'elm-format-on-save-mode)


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
   '(markdown-mode elm-mode go-mode auto-complete git-gutter haskell-mode treemacs kaolin-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
