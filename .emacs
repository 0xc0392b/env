;; william's emacs config~
;; last updated 5th april 2022.


;; --------------------------------------------------------------------------------
;; package management


;; MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; list of packages i use
(setq package-list
      '(treemacs use-package doom-themes git-gutter auto-complete
		 haskell-mode elixir-mode go-mode elm-mode ess
		 markdown-mode magit org-roam verb ein circe emms
		 vertico marginalia elfeed elfeed-org yaml-mode
		 dockerfile-mode exwm telephone-line))

;; activate all
(package-initialize)

;; refresh package archive
(unless package-archive-contents
  (package-refresh-contents))

;; install missing
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; enable use-package
(require 'use-package)


;; --------------------------------------------------------------------------------
;; exwm


(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

(setq exwm-randr-workspace-output-plist '(0 "DP-2" 1 "DP-0.8" 2 "DP-4.8"))

(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
            (start-process-shell-command
             "xrandr"
	     nil
	     "xrandr --output DP-2 --mode 2560x1440")
            (start-process-shell-command
             "xrandr"
	     nil
	     "xrandr --output DP-0.8 --mode 1920x1080 --above DP-2")
            (start-process-shell-command
             "xrandr"
	     nil
	     "xrandr --output DP-4.8 --mode 1920x1080 --rotate right --left-of DP-2")))

(exwm-randr-enable)
(exwm-config-default)


;; --------------------------------------------------------------------------------
;; misc


;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; automatically refresh buffers when files change on disk
(global-auto-revert-mode t)

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

;; telephone line
(use-package telephone-line
  :ensure t
  :init
  (setq telephone-line-height 28)
  (setq telephone-line-lhs
	'((accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
	'((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))))
  :config
  (telephone-line-mode 1))

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
  (windmove-default-keybindings 'super))             ; using super + arrow keys

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

;; use vertico for completions
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; add history to completions
(use-package savehist
  :after vertico
  :init
  (savehist-mode))

;; add marginalia info to completion buffers
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
			   marginalia-annotators-light nil))
  :init
  (marginalia-mode))


;; --------------------------------------------------------------------------------
;; language, spelling, version control


;; working with latex in org-mode
(setq exec-path (append exec-path '("/usr/bin/latex")))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

;; automatically enable flyspell on certain major modes
(use-package flyspell
  :ensure t
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  (add-hook 'magit-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'dockerfile-mode-hook 'flyspell-mode)
  (add-hook 'yaml-mode-hook 'flyspell-mode)
  (add-hook 'xml-mode-hook 'flyspell-mode))

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
  :after org
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
(use-package circe
  :ensure t
  :init
  (setq circe-network-options
	'(("timov.live" :use-tls t :host "irc.timov.live" :port 6697)
	  ("libera.chat" :use-tls t :host "irc.libera.chat" :port 6697)))

  (defun timov ()
    "Connect to timov.live IRC network."
    (interactive)
    (circe "timov.live"))

  (defun libera ()
    "Connect to libera.chat IRC network."
    (interactive)
    (circe "libera.chat"))
  :config
  (require 'circe-display-images)
  (enable-circe-display-images))


;; --------------------------------------------------------------------------------
;; RSS


;; use elfeed-org to read RSS feeds from an org file
(use-package elfeed-org
  :ensure t
  :init
  (setq rmh-elfeed-org-files (list "~/org/rss.org"))
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (elfeed-org))


;; --------------------------------------------------------------------------------
;; media


;; emms music player
(use-package emms
  :ensure t
  :init
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-browser-thumbnail-small-size 256)
  (setq emms-browser-thumbnail-medium-size 256)
  (setq emms-player-list '(emms-player-mplayer))
  (setq emms-browser-default-cover (list "~/org/emacs/no_cover.png" nil nil))
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))


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
 '(circe-default-directory "~/org/")
 '(circe-default-nick "gromug")
 '(circe-default-part-message "bye o/")
 '(circe-default-quit-message "bye o/")
 '(circe-default-realname "william santos <w@018e6f.me>")
 '(circe-default-user "gromug")
 '(circe-extra-nicks '("wholeham" "william" "will"))
 '(elfeed-search-date-format '("%d-%m-%Y %H-%M-%S" 20 :left))
 '(elfeed-search-title-max-width 60)
 '(elfeed-search-title-min-width 25)
 '(elfeed-user-agent "william >:)")
 '(git-gutter:added-sign "a")
 '(git-gutter:deleted-sign "r")
 '(git-gutter:modified-sign "m")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(telephone-line exwm dockerfile-mode yaml-mode marginalia vertico emms circe-display-images elfeed-org elfeed circe ein verb org-roam magit doom-themes ess elixir-mode markdown-mode elm-mode go-mode auto-complete git-gutter haskell-mode treemacs))
 '(scroll-down-aggressively nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
