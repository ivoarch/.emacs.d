;; init.el --- My personal Emacs configuration file

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
(provide 'init.el)
(require 'package)

(defvar init-file-location (concat user-emacs-directory "init.el"))
(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; Reload init file
(defun reload-init-file ()
  "Reload the init file."
  (interactive)
  (load-file init-file-location))
(global-set-key (kbd "C-c r") 'reload-init-file)

;; Open .emacs init
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file init-file-location))
(global-set-key (kbd "C-c i") 'open-init-file)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; personal info
(setq user-full-name "Ivaylo Kuzev"
      user-mail-address "ivkuzev@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; call the debugger on error
;;(setq debug-on-error t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Reduce the frequency of garbage collection by making it happen on
;; each 25MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 25000000)

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'use-file-dialog) (setq use-file-dialog nil))
(if (fboundp 'use-dialog-box) (setq use-dialog-box nil))

;; Maximize
(toggle-frame-maximized)

;; no fringes
;;(set-fringe-mode 0)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; Set font
;;(add-to-list 'default-frame-alist
;;             '(font . "Fira Mono-14:width=condensed:weight=light"))
(add-to-list 'default-frame-alist
             '(font . "Dejavu Sans Mono 12"))

;; cursor style
(setq-default cursor-type 'bar)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; Hide mouse cursor while typing.
(setq make-pointer-invisible t)

;; highlight the current line
(global-hl-line-mode +1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode t)
(setq show-paren-delay 0)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; show file size
(size-indication-mode t)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Do not break lines
;;(set-default 'truncate-lines t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; turned off the box around the mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; remove ^M symbols
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable copy/paste from emacs to other apps
(setq
 interprogram-cut-function 'x-select-text
 interprogram-paste-function 'x-selection-value
 save-interprogram-paste-before-kill t
 select-active-regions t
 select-enable-clipboard t
 select-enable-primary t)
(global-set-key (kbd "C-w") #'clipboard-kill-region)
(global-set-key (kbd "M-w") #'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") #'clipboard-yank)

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))

(defun termux-p ()
  "Check if Emacs is running under Termux."
  (string-match-p (regexp-quote "/com.termux/")
                  (expand-file-name "~")))

;; Browse URL functionality in Termux
(when (termux-p)
  (use-package browse-url :ensure nil
    :config
    (advice-add 'browse-url-default-browser :override
                (lambda (url &rest args)
                  (start-process-shell-command "open-url"
                                               nil
                                               (concat "am start -a android.intent.action.VIEW --user 0 -d "
                                                       url))))))

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") #'dired-jump) (autoload 'dired-jump "dired")

;; goto line
(global-set-key (kbd "C-x g") #'goto-line)

;; split-window
(global-set-key (kbd "M-3") #'split-window-horizontally)
(global-set-key (kbd "M-2") #'split-window-vertically)
(global-set-key (kbd "M-1") #'delete-other-windows)
(global-set-key (kbd "M-0") #'delete-window)

;; resize window
(global-set-key (kbd "C-c <up>") #'shrink-window)
(global-set-key (kbd "C-c <down>") #'enlarge-window)
(global-set-key (kbd "C-c <left>") #'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") #'enlarge-window-horizontally)

;; iBuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

;; Delete words with C-w and rebind kill region to C-x C-k
(global-set-key (kbd "C-w") #'backward-kill-word)
(global-set-key "\C-x\C-k" #'kill-region)
(global-set-key "\C-c\C-k" #'kill-region)

;; Files
(global-set-key (kbd "C-x s") #'save-buffer)
(global-set-key (kbd "C-x C-s") #'save-some-buffers)
(global-set-key (kbd "C-x c") #'save-buffers-kill-emacs)

;; Font size
(define-key global-map (kbd "C-+") #'text-scale-increase)
(define-key global-map (kbd "C--") #'text-scale-decrease)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; bring up help for key bindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Enable restarting Emacs from within Emacs
(use-package restart-emacs
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package auto-complete-clang
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :bind  ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
(recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  (progn
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

  ;; listing options
  (setq dired-listing-switches "-alhv --group-directories-first")

  (defun my-dired-browser-find-file ()
    "Dired function to view a file in a web browser"
    (interactive)
    (browse-url (browse-url-file-url (dired-get-filename))))
  ;; Bind a Key in Emacs's Dired-Mode to View a File in the Default Browser
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "b" 'my-dired-browser-find-file))))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this)
         ("M-," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package ido
  :ensure t
  :bind (("C-x f" . ido-find-file))
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

;;(use-package ido-ubiquitous
;;  :ensure t
;;  :config
;;  (ido-ubiquitous-mode +1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down)
  (ido-vertical-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode +1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"        . markdown-mode)
         ("\\.markdown$"  . markdown-mode)
         ("\\.mkd$"       . markdown-mode)
         ("\\README\\.md" . gfm-mode)))

(use-package yaml-mode
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :ensure t)

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config
  (setq css-indent-offset 2))

(use-package rainbow-mode
  :ensure t
  :config
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :config (progn
            (add-hook 'python-mode-hook 'anaconda-mode)
))

(use-package php-mode
  :ensure t
  :defer t)

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
        ("\\.json$" . js2-mode))
  :ensure t
  :init
  (autoload 'js2-mode "js2-mode" nil t))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  )

(use-package sh-script
  :ensure t
  :mode (("\\.zsh-template$" . shell-script-mode)
         ("\\.zsh$" . shell-script-mode)
         ("zsh\\.*" . shell-script-mode))
  :init (add-hook 'after-save-hook
                  ;; make shell script executable on save
                  'executable-make-buffer-file-executable-if-script-p)
  :config (setq sh-basic-offset 2
                sh-indentation 2
                sh-indent-comment t))

(use-package rpm-spec-mode
  :mode (("\\.spec" . rpm-spec-mode))
  :ensure t
  :init
  (autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t))

(use-package conf-mode
  :mode ("\\.*rc$" . conf-unix-mode))

(use-package ssh-config-mode
  :ensure t
  :init (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;;(use-package makefile-mode
;;  :init
;;  (add-hook 'makefile-gmake-mode-hook
;;            (lambda () (setq indent-tabs-mode nil))))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package org
  :mode (("\\.\\(org\\|txt\\)$" . org-mode))
  :defer t
  :config
  ;; fontify org mode code blocks
  (setq org-src-fontify-natively t
        org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C . t)
     (dot . t)
     (ditaa . t)
     (sh . t)
     (python . t)
     (R . t)
     (ruby . t)
     (emacs-lisp . t)
     (lisp . t)
     (scheme . t)
     (haskell . t)
     (perl . t)
     (js . t)
     ))

  ;; HTML and Publishing
  (setq org-html-validation-link nil)
  (setf org-html-postamble t)

  (setq org-publish-project-alist
      '(("enotes"
         :components ("enotes-content" "enotes-static"))
        ("enotes-content"
         ;; Directory for source files in org format
         :base-directory "~/Dropbox/Enotes/org/"
         :base-extension "org"
         ;; Path to exported HTML files
         :publishing-directory "~/Dropbox/Enotes/public_html/"
         ;;:publishing-function org-publish-org-to-html
         :publishing-function org-html-publish-to-html
         :html-html5-fancy t
         :auto-sitemap t
         :sitemap-title "Sitemap"
         :sitemap-filename "sitemap.org"
         :sitemap-sort-files anti-chronologically
         )
        ;; Path to Static files
        ("enotes-static"
         :base-directory "~/Dropbox/Enotes/files/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/Enotes/public_html/files/"
         :recursive t
         :publishing-function org-publish-attachment
         ))))

(use-package deft
  :ensure t
  :defer t
  :config
  (setq
     deft-use-filename-as-title t
     deft-extensions "org"
     deft-directory "~/Dropbox/Enotes/org"
     deft-text-mode 'org-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode 1)
 '(browse-url-generic-program "gnome-open")
 '(doc-view-continuous t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startupinhibit-startup-screen t)
 '(initial-scratch-message ";; scratch buffer created -- Happy Hacking ivo!!")
 '(package-selected-packages
   (quote
    (multiple-cursors yasnippet which-key toml-mode haskell-mode rust-mode php-mode auto-complete-clang markdown-preview-mode flymd zenburn-theme yaml-mode web-mode use-package super-save ssh-config-mode spacemacs-theme smex rpm-spec-mode rainbow-mode rainbow-delimiters powerline move-text markdown-mode magit js2-mode ido-vertical-mode ido-ubiquitous flyspell-correct-ivy flycheck flx-ido exec-path-from-shell easy-kill deft ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:foreground "dim gray" :slant italic)))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;;; init.el ends here
