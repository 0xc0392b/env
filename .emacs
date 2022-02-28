;; william's emacs config~
;; last updated 28th february 2022.


;; --------------------------------------------------------------------------------
;; package management


;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list of packages i use
(setq package-list
      '(treemacs use-package doom-themes git-gutter auto-complete
		 haskell-mode elixir-mode go-mode ess elm-mode
		 markdown-mode magit org-roam verb ein circe))

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
;; misc


;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; me
(setq user-full-name     "William Santos"
      user-mail-address  "w@wsantos.net")

;; date, time, calendar
(setq calendar-week-start-day 1)
(setq display-time-format "%a %d %b %I:%M%p")

;; warn when opening files > 100MB
(setq large-file-warning-threshold 100000000)

;; GPG-encrypted credentials
(setq auth-sources '("~/org/authinfo.gpg"))

;; automatically refresh buffers when files change on-disk
(global-auto-revert-mode t)


;; --------------------------------------------------------------------------------
;; functions


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

;; read GPG-encrypted password using auth-source params
;; from https://github.com/emacs-circe/circe/wiki/Configuration#safer-password-management
(defun fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))


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
(setq scroll-margin                   0
      scroll-conservatively           100000
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

;; display time globally, don't blink cursor, always show line numbers
(display-time-mode 1)
(blink-cursor-mode -1)
(global-display-line-numbers-mode)

;; hide toolbars and scrollbars
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar 0)

;; configure mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; set default font (this should already be a system font)
;; note: using Inconsolata version >= 3.001 requires emacs to be compiled --with-cairo
;;       instead of Xft. see https://github.com/googlefonts/Inconsolata/issues/42
(set-frame-font "Inconsolata")

;; custom key bindings
(global-set-key (kbd "C-`") 'auto-complete-mode)       ; toggle auto-complete with C-`
(global-set-key (kbd "C-1") 'flyspell-mode)            ; toggle spelling checker with C-1
(global-set-key (kbd "C-x C-b") 'buffer-menu)          ; buffer-menu instead of list-buffers
(global-set-key (kbd "C-S-<up>") 'move-line-up)        ; dragging lines up
(global-set-key (kbd "C-S-<down>") 'move-line-down)    ; dragging lines down
(global-set-key (kbd "C-x q") 'kill-buffer-and-window) ; kill buffer and quit window

;; navigating between multiple windows
(when (fboundp 'windmove-default-keybindings)       ; move point from window to window
  (windmove-default-keybindings 'meta))             ; using meta + arrow keys

;; always show git gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

;; highlight parenthesis
(use-package paren
  :config
  (show-paren-mode +1))

;; load and configure global theme.
;;
;; you should restart emacs after changing this. eval-buffer is not always enough.
;; i don't know why.
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


;; --------------------------------------------------------------------------------
;; language and version control configurations


;; org-mode
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

;; org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory "~/org")
  :config
  (org-roam-db-autosync-mode))

;; golang
(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

;; elm
(use-package elm-mode
  :ensure t
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))

;; elixir
(use-package elixir-mode
  :ensure t
  :config
  (add-hook
   'elixir-mode-hook (lambda () (add-hook 'before-save-hook
					  'elixir-format nil t))))

;; magit
(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))


;; --------------------------------------------------------------------------------
;; IRC


;; configure circe IRC client
(require 'circe-display-images)
(enable-circe-display-images)

(setq circe-network-options
      '(("timov.live"
	 :use-tls t :host "irc.timov.live" :port 6697
         :channels ("#goldencafe")
         :nickserv-password (lambda (server) (fetch-password
					      :user "gromug"
					      :machine "irc.timov.live")))
	("libera.chat"
	 :use-tls t :host "irc.libera.chat" :port 6697
         :channels ("#libera" "##moshpit")
         :nickserv-password (lambda (server) (fetch-password
					      :user "gromug"
					      :machine "irc.libera.chat")))))

(defun timov ()
  "Connect to timov.live IRC network."
  (interactive)
  (circe "timov.live"))

(defun libera ()
  "Connect to libera.chat IRC network."
  (interactive)
  (circe "libera.chat"))


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
 '(circe-default-directory "~/org/irc/")
 '(circe-default-nick "gromug")
 '(circe-default-part-message "bye o/")
 '(circe-default-quit-message "bye o/")
 '(circe-default-realname "william santos <w@018e6f.me>")
 '(circe-default-user "gromug")
 '(circe-extra-nicks '("wholeham" "william"))
 '(git-gutter:added-sign "a")
 '(git-gutter:deleted-sign "r")
 '(git-gutter:modified-sign "m")
 '(package-selected-packages
   '(circe ein verb org-roam magit doom-themes ess elixir-mode markdown-mode elm-mode go-mode auto-complete git-gutter haskell-mode treemacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
