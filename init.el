;;; init.el --- My personal Emacs configuration file

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
(require 'package)

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

(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

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

;; no fringes
(set-fringe-mode 0)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; set font for all windows
(add-to-list 'default-frame-alist
            '(font . "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

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

;; http://gergely.polonkai.eu/2016/11/10/edit-file-as-other-user-in-emacs/
(defun open-this-file-as-other-user (user)
  "Edit current file as USER, using `tramp' and `sudo'.  If the current
buffer is not visiting a file, prompt for a file name."
  (interactive "sEdit as user (default: root): ")
  (when (string= "" user)
    (setq user "root"))
  (let* ((filename (or buffer-file-name
                       (read-file-name (format "Find file (as %s): "
                                               user))))
         (tramp-path (concat (format "/sudo:%s@localhost:" user) filename)))
    (if buffer-file-name
        (find-alternate-file tramp-path)
      (find-file tramp-path))))

;; enable copy/paste from emacs to other apps
(setq
 interprogram-cut-function 'x-select-text
 interprogram-paste-function 'x-selection-value
 save-interprogram-paste-before-kill t
 select-active-regions t
 x-select-enable-clipboard t
 x-select-enable-primary t)
(global-set-key (kbd "C-w") #'clipboard-kill-region)
(global-set-key (kbd "M-w") #'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") #'clipboard-yank)

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))

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

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

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
  :ensure t
  :bind  ("C-x r" . ido-recentf-open)
  :config
  (progn
    (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
          recentf-max-saved-items 50
          recentf-max-menu-items 15
          ;; disable recentf-cleanup on Emacs start, because it can cause
          ;; problems with remote files
          recentf-auto-cleanup 'never)
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (recentf-mode +1)))

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

(use-package rpm-spec-mode
  :mode (("\\.spec" . rpm-spec-mode))
  :ensure t
  :init
  (autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t))

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

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

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

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"        . markdown-mode)
         ("\\.markdown$"  . markdown-mode)
         ("\\.mkd$"       . markdown-mode)
         ("\\README\\.md" . gfm-mode)))

(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :ensure t)

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config
  (setq css-indent-offset 2))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
        ("\\.json$" . js2-mode))
  :ensure t
  :init
  (autoload 'js2-mode "js2-mode" nil t))

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
         :base-directory "~/MEGA/Enotes/org/"
         :base-extension "org"
         ;; Path to exported HTML files
         :publishing-directory "~/MEGA/Enotes/public_html/"
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
         :base-directory "~/MEGA/Enotes/files/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/MEGA/Enotes/public_html/files/"
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
 '(linum ((t (:foreground "dim gray" :slant italic))) t))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
