;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  General configuration  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Keep installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Update package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; Default credentials
(setq user-full-name "Pavlin Gergov"
      user-mail-address "gergov.pavlin@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Osx specific
(when (eq system-type 'darwin)
  ;; Use cmd as meta key
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))
  ;; Disable the annoying bell ring
  (setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  GUI configuration  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No need for useles pop up window
(setq ns-pop-up-frames nil)

;; Or welcome screen
(setq inhibit-startup-screen t)

;; No need for toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Blinkless cursor
(blink-cursor-mode -1)

;; Highlight the current line
(global-hl-line-mode +1)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Default font-size
(set-face-attribute 'default nil :height 160)

;; Line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Newline at end of file
(setq require-final-newline t)

;; More useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Automatically refresh when file changes on disk
(global-auto-revert-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Split vertically instead of horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;
;;;;  Packages  ;;;;
;;;;;;;;;;;;;;;;;;;;

;; Handle package configuration with `use-package`
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Indicate loading activity
(setq use-package-verbose t)

;; Blackish theme
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; Better completion
(use-package ivy
  :ensure t
  :config
    (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; The one and only silver search
(use-package ag
  :ensure t)

;; Navigate projects
(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :bind
  ("C-c k" . projectile-find-file)
  ("C-c j" . projectile-switch-to-buffer)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode +1))

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; Side project tree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'ascii))

;; Pretty git changes indicators
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (set-face-foreground 'git-gutter:modified "yellow"))

;; The Python must package
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'current)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

;; Python docs, lookups and completion
(use-package anaconda-mode
  :ensure t)

;; Automatically load the .python-version env when switching between projects
(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

;; Typescript setup (-_^)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook
        (lambda ()
          (setup-tide-mode))))

;; Spot brackets easier
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Drag regions
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; Edit yaml
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Pretty markdown files
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode yaml-mode drag-stuff rainbow-delimiters web-mode tide auto-virtualenv anaconda-mode elpy git-gutter neotree projectile ag ivy dracula-theme use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Custom functions  ;;;;
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
;;;;  Key bindings  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Find files
(global-set-key (kbd "C-;") 'find-file)
;; Find file in another window
(global-set-key (kbd "C-M-;") 'find-file-other-window)
;; Switch window
(global-set-key (kbd "C-o") 'other-window)
;; Previous opened window
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
;; Return one step back
(global-set-key (kbd "M-,") 'pop-tag-mark)
;; Use `d` to delete characters
(global-set-key (kbd "C-d") 'delete-backward-char)
(global-set-key (kbd "C-M-d") 'delete-char)
;; Copy current line
(global-set-key "\C-c\c" "\C-a\C- \C-n\M-w")
;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Custom functions bindings
(global-set-key (kbd "C-c i") 'my-python-breakpoint)
(global-set-key (kbd "C-c o") 'my-insert-line-before)
(global-set-key (kbd "C-c t") 'my-neotree-project-dir-toggle)
(global-set-key (kbd "C-c m") 'my-rack-brackets-on-new-indented-line)
