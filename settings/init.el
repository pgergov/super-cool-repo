;; package manager
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-install 'dracula-theme)
(package-install 'anaconda-mode)
(package-install 'drag-stuff)
(package-install 'elpy)
(package-install 'git-gutter)
(package-install 'neotree)
(package-install 'ag)
(package-install 'projectile)
(package-install 'rainbow-delimiters)
(package-install 'google-this)
;; default theme
(custom-set-variables
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("d9129a8d924c4254607b5ded46350d68cc00b6e38c39fc137c3cfb7506702c12" default)))
 '(inhibit-startup-screen t))
(custom-set-faces)
;; python <3
(elpy-enable)
(global-git-gutter-mode +1)
;; disable startup notifications
(setq inhibit-startup-screen t)
;; remove menu & bar from the top
(menu-bar-mode -1)
(tool-bar-mode -1)
;; blinkless cursor
(blink-cursor-mode -1)
;; highlight the current line
(global-hl-line-mode +1)
;; theme is purle so this cant be
(set-face-foreground 'git-gutter:modified "yellow")
;; projec tree settings
(setq neo-smart-open)
(setq neo-theme 'ascii)
(global-set-key [f2] 'neotree-toggle)
;; auto refresh when file changes on disk
(global-auto-revert-mode t)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; self-explanatory
(global-set-key [f3] 'ibuffer)
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-;") 'find-file)
(global-set-key (kbd "C-M-;") 'find-file-other-window)
(global-set-key (kbd "C-d") 'delete-backward-char)
(global-set-key (kbd "C-M-d") 'delete-char)
(global-set-key [(control shift up)] 'drag-stuff-up)
(global-set-key [(control shift down)] 'drag-stuff-down)
;; copy current line
(global-set-key "\C-c\c" "\C-a\C- \C-n\M-w")
(global-set-key (kbd "C-?") 'comment-region)

(defun my-python-breakpoint ()
  "Inserts python breakpoint where the cursor is."
  (interactive)
  (progn
    (insert "import ipdb; ipdb.set_trace()")
    (move-end-of-line 1)
    (comint-send-input)))
(global-set-key (kbd "C-c i") 'my-python-breakpoint)

(defun my-insert-line-before (times)
  "Inserts a newline(s) above the line containing the cursor."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))
(global-set-key (kbd "C-c o") 'my-insert-line-before)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; projectile
(projectile-global-mode +1)
(global-set-key (kbd "C-c j") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c k") 'projectile-find-file)
