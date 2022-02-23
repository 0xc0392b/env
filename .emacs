;; william's emacs config~
;; last updated 23rd february 2022.


;; --------------------------------------------------------------------------------
;; package management


;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list of packages i use
(setq package-list
      '(treemacs use-package doom-themes git-gutter auto-complete
		 haskell-mode elixir-mode go-mode ess elm-mode
		 markdown-mode magit org-roam verb ein))

;; 1. activate all packages
;; 2. fetch the list of packages available
;; 3. install missing packages
;; 4. enable use-package
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)


;; --------------------------------------------------------------------------------
;; misc


;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; me
(setq user-full-name "William Santos"
      user-mail-address "w@wsantos.net")

;; calendar
(setq calendar-week-start-day 1)
(setq display-time-format "%a %d %b %I:%M%p")

;; warn when opening files > 100MB
(setq large-file-warning-threshold 100000000)

;; automatically refresh buffers/org-roam cache when file changes on disk
(global-auto-revert-mode t)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory "~/org")
  :config
  (org-roam-db-autosync-mode))


;; --------------------------------------------------------------------------------
;; my functions


;; drag current line up and down
;; from https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


;; --------------------------------------------------------------------------------
;; interface


;; turn off system bell
(setq ring-bell-function 'ignore)

;; quit even if processes are running
(setq confirm-kill-processes nil)

;; don't show default startup screen
(setq inhibit-startup-screen t)

;; smooth scrolling
;; from https://github.com/bbatsov/emacs.d/blob/master/init.el#L82
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; treemacs
(use-package treemacs
  :ensure t
  :init
  (global-set-key (kbd "C-\\") 'treemacs)
  (setq treemacs-user-mode-line-format " william's emacs "))

;; save and restore sessions automatically
(setq desktop-path '("~/org/emacs/sessions/"))

(desktop-save-mode)

;; don't blink cursor
;; display time globally
(blink-cursor-mode -1)
(display-time-mode 1)

;; always show line numbers and git gutter
(global-display-line-numbers-mode)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

;; hide toolbars and scrollbars
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar 0)

;; configure mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; highlight parenthesis
(use-package paren
  :config
  (show-paren-mode +1))

;; set default font (this should already be a system font)
;; note: using Inconsolata version >= 3.001 requires emacs to be compiled --with-cairo
;;       instead of Xft. see https://github.com/googlefonts/Inconsolata/issues/42
(set-frame-font "Inconsolata")

;; load and configure global theme.
;; you should always restart emacs after changing this. eval-buffer is not enough.
;; i don't actually know why.
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
;; - doom-acario-light
;; - doom-solarized-light
;; - doom-tomorrow-day
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-acario-dark t))

;; custom key bindings
(global-set-key (kbd "C-`") 'auto-complete-mode)    ; toggle auto-complete with C-`
(global-set-key (kbd "C-1") 'flyspell-mode)         ; toggle spelling checker with C-1
(global-set-key (kbd "C-x C-b") 'buffer-menu)       ; buffer-menu instead of list-buffers
(global-set-key (kbd "C-S-<up>") 'move-line-up)     ; dragging lines up
(global-set-key (kbd "C-S-<down>") 'move-line-down) ; dragging lines down

;; navigating between windows
(when (fboundp 'windmove-default-keybindings)       ; move point from window to window
  (windmove-default-keybindings 'super))            ; using super + arrow keys


;; --------------------------------------------------------------------------------
;; language-specific configurations


;; org-mode stuff
(use-package org
  :ensure t
  :mode
  ("\\.org\\'" . org-mode)
  :config
  (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

;; auto go fmt when saving golang buffers
(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

;; auto elm-format when saving elm buffers (requires npm i -g elm-format)
(use-package elm-mode
  :ensure t
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))

;; auto elixir-format when saving elixir buffers
(use-package elixir-mode
  :ensure t
  :config
  (add-hook
   'elixir-mode-hook (lambda () (add-hook 'before-save-hook
					  'elixir-format nil t))))


;; --------------------------------------------------------------------------------
;; email


;; TODO


;; --------------------------------------------------------------------------------
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
   '(ein verb org-roam magit doom-themes ess elixir-mode markdown-mode elm-mode go-mode auto-complete git-gutter haskell-mode treemacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
