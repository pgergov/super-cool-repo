;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  General configs  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; update package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Pavlin Gergov"
      user-mail-address "gergov.pavlin@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; osx specific
(when (eq system-type 'darwin)
  ;; use cmd as meta key
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))
  ;; disable the annoying bell ring
  (setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  GUI configuration  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no need for useles pop up window
(setq ns-pop-up-frames nil)

;; no need for toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; blinkless cursor
(blink-cursor-mode -1)

;; highlight the current line
(global-hl-line-mode +1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Newline at end of file
(setq require-final-newline t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; automatically refresh when file changes on disk
(global-auto-revert-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; split vertically instead of horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;
;;;;  packages  ;;;;
;;;;;;;;;;;;;;;;;;;;

;; handle package configuration with `use-package`
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; indicate loading activity
(setq use-package-verbose t)

(use-package dracula-theme
  :ensure t)

(use-package anaconda-mode
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (set-face-foreground 'git-gutter:modified "yellow"))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'ascii))

(use-package ag
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode +1)
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode))

(use-package prettier-js
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq-default js2-basic-offset 2))

(use-package rjsx-mode
  :ensure t)

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :ensure t
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (xref-js2 js2-refactor drag-stuff rjsx-mode dracula-theme prettier-js dumb-jump rainbow-delimiters projectile ag neotree git-gutter elpy anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  custom functions  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-python-breakpoint ()
  "Inserts python breakpoint where the cursor is."
  (interactive)
  (progn
    (insert "import ipdb; ipdb.set_trace()")
    (move-end-of-line 1)
    (comint-send-input)))

(defun my-insert-line-before (times)
  "Inserts a newline(s) above the line containing the cursor."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))

(defun my-rack-brackets-on-new-indented-line ()
  (interactive)
  (progn
    (newline-and-indent)
    (move-beginning-of-line 1)
    (newline)
    (previous-line)
    (indent-for-tab-command)))

(defun my-goto-definition ()
  "Elpy fails to find decorated python functions, dumb-jump-go doesn't."
  (interactive)
  (let ((initial_func (thing-at-point 'symbol)))
    (elpy-goto-definition)
    (when (not (equal initial_func (thing-at-point 'symbol)))
      (progn
	(dumb-jump-back)
	(dumb-jump-go)))))

(defun my-neotree-project-dir-toggle ()
  "Open NeoTree using project root via `find-file-in-project` or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
           ; (projectile-project-root)
           (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  key bindings  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-;") 'find-file)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "C-M-;") 'find-file-other-window)
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)

;; use `d` to delete characters
(global-set-key (kbd "C-M-d") 'delete-char)
(global-set-key (kbd "C-d") 'delete-backward-char)

;; copy current line
(global-set-key "\C-c\c" "\C-a\C- \C-n\M-w")
;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; custom funcs
(global-set-key (kbd "C-c i") 'my-python-breakpoint)
(global-set-key (kbd "C-c o") 'my-insert-line-before)
(define-key elpy-mode-map (kbd "M-.") 'my-goto-definition)
(global-set-key (kbd "C-c t") 'my-neotree-project-dir-toggle)
(global-set-key (kbd "C-c m") 'my-rack-brackets-on-new-indented-line)

;; packages
(global-set-key (kbd "C-c k") 'projectile-find-file)
(global-set-key (kbd "C-c j") 'projectile-switch-to-buffer)
