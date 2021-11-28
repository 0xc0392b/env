;; william's emacs config~
;; last updated 28th november 2021


;; --------------------------------------------------------------------------------
;; package management


;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list of packages i use
(setq package-list '(treemacs use-package doom-themes git-gutter auto-complete
			      haskell-mode elixir-mode go-mode ess elm-mode
			      markdown-mode))

;; activate all packages
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; enable use-package
(require 'use-package)


;; --------------------------------------------------------------------------------
;; interface


;; don't show default startup screen
(setq inhibit-startup-screen t)

;; turn off system bell
(setq ring-bell-function 'ignore)

;; open scratch file on startup (this should already exist)
(find-file "~/.scratch.md")

;; always show line numbers and git gutter
(global-display-line-numbers-mode)
(global-git-gutter-mode 1)

;; hide toolbars
(menu-bar-mode 0)
(tool-bar-mode 0)

;; hide scrollbars
(toggle-scroll-bar 0)

;; set default font (this should already be a system font)
;; note: using Inconsolata version >= 3.001 requires emacs to be compiled --with-cairo
;;       instead of Xft. see https://github.com/googlefonts/Inconsolata/issues/42
(set-frame-font "Inconsolata")

;; load and configure global theme
;;
;; list of dark and light themes i like from the doom-themes package:
;;
;; dark:
;; - doom-acario-dark
;; - doom-challenger-deep
;; - doom-horizon
;; - doom-laserwave
;; - doom-manegarm
;; - doom-moonlight
;; - doom-nord
;; - doom-oceanic-next
;; - doom-outrun-electric
;; - doom-solarized-dark
;; - doom-sourcerer
;;
;; light:
;; - doom-nord-light
;; - doom-solarized-light
;; - doom-tomorrow-day
(use-package doom-themes
	     :ensure t
	     :config

	     (setq doom-themes-enable-bold t
		   doom-themes-enable-italic t)

	     (load-theme 'doom-outrun-electric t))

;; custom key bindings
(global-set-key (kbd "C-\\") 'treemacs)          ; toggle treemacs with C-\
(global-set-key (kbd "C-`") 'auto-complete-mode) ; toggle auto-complete with C-`
(when (fboundp 'windmove-default-keybindings)    ; move point from window to window
  (windmove-default-keybindings 'meta))          ;   using meta + arrow keys


;; --------------------------------------------------------------------------------
;; language-specific configurations


;; auto go fmt when saving golang buffers
(add-hook 'before-save-hook 'gofmt-before-save)

;; auto elm-format when saving elm buffers (requires npm i -g elm-format)
(add-hook 'elm-mode-hook 'elm-format-on-save-mode)

;; auto elixir-format when saving elixir buffers
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))


;; --------------------------------------------------------------------------------
;; email


;; TODO


;; --------------------------------------------------------------------------------
;; misc


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
   '(doom-themes ess elixir-mode markdown-mode elm-mode go-mode auto-complete git-gutter haskell-mode treemacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
