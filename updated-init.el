;; package manager
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'dracula-theme)
(require 'anaconda-mode)
(require 'drag-stuff)
(require 'elpy)
(require 'git-gutter)
(require 'neotree)
(require 'ag)
(require 'projectile)
(require 'rainbow-delimiters)
(require 'google-this)
(require 'dumb-jump)
(require 'js2-mode)
;; default theme
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
    (org-present prettier-js js2-mode rjsx-mode yaml-mode dracula-theme emmet-mode rainbow-delimiters projectile neotree google-this git-gutter elpy dumb-jump drag-stuff anaconda-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; python <3
(elpy-enable)
(global-set-key (kbd "M-,") 'pop-tag-mark)
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
(setq neo-smart-open t)
(setq neo-theme 'ascii)
(global-set-key [f2] 'neotree-toggle)
;; auto refresh when file changes on disk
(global-auto-revert-mode t)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; split vertically instead of horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 120)
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
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(defun my-python-breakpoint ()
  "Inserts python breakpoint where the cursor is."
  (interactive)
  (progn
    (insert "import ipdb; ipdb.set_trace()")
    (move-end-of-line 1)
    (comint-send-input)))
(global-set-key (kbd "C-c i") 'my-python-breakpoint)

;; feature - just a function.
(defun my-insert-line-before (times)
  "Inserts a newline(s) above the line containing the cursor."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))
(global-set-key (kbd "C-c o") 'my-insert-line-before)

(defun my-rack-brackets-on-new-indented-line ()
  (interactive)
  (progn
    (newline-and-indent)
    (move-beginning-of-line 1)
    (newline)
    (previous-line)
    (indent-for-tab-command)))

(global-set-key (kbd "C-c m") 'my-rack-brackets-on-new-indented-line)



(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; projectile
(projectile-global-mode +1)
(global-set-key (kbd "C-c j") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c k") 'projectile-find-file)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))
(set-face-attribute 'default nil :height 125)
(add-hook 'html-mode-hook #'yas-minor-mode)
(dumb-jump-mode)

(defun my-goto-definition ()
  "Elpy (jedi) fails to find decorated python functions. Luckily `dumb-jump-go` doesn't.`"
  (interactive)
  (let ((initial_func (thing-at-point 'symbol)))
    (elpy-goto-definition)
    (when (not (equal initial_func (thing-at-point 'symbol)))
      (progn
	(dumb-jump-back)
	(dumb-jump-go)))))

(define-key elpy-mode-map (kbd "M-.") 'my-goto-definition)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) 
(setq-default js2-basic-offset 2)
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'prettier-js)
(setq prettier-js-args '(
  "--print-width" "80"
  "--tab-width" "2"
  "--single-quote" "true"
  "--trailing-comma" "none"
  "--no-bracket-spacing" "false"
  "--jsx-bracket-same-line" "true"
))
