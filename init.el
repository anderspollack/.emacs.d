;; Configuration for Emacs 29.4

;; Global visual settings
;; open emacs with a scratch buffer
(setq inhibit-startup-message t)
;; Show column number in modeline
(column-number-mode)
;; Disable soft line wrapping
(set-default 'truncate-lines t)
;; Show line numbers
;; (global-display-line-numbers-mode t)
;; Disable line numbers for certain modes
;; (dolist
;;     (mode
;;      '(
;;        eshell-mode-hook
;;        org-mode-hook
;;        shell-mode-hook
;;        term-mode-hook
;;        ))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; File settings
;; Stop making "~"backup files
(setq make-backup-files nil)

;; Keyboard settings
;; Answer yes-or-no prompts with a "y"/"n"
(setq use-short-answers t)
;; Enable Shift + arrow keys to move around window splits
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Ensure that pressing ESC a second time doesn't close all windows
(defun +keyboard-escape-quit-adv (fn)
  "Around advice for `keyboard-escape-quit' FUN. Preserve window configuration when pressing ESC." 
  (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit))) 
    (funcall fn)))
(advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)

;; Window Settings
;; Enable undo/redo of window splits with M-x winner-undo/redo
(winner-mode 1)

;; Package settings
;; As of Emacs 29.1, use-package is included by default
;; Add MELPA Stable package repository
(require 'package)
(add-to-list 'package-archives 
	     ;; '("melpa-stable" . "https://stable.melpa.org/packages/")
	     '("melpa" . "https://melpa.org/packages/"))
;; Set :ensure t for all use-package declarations to ensure they exist before using
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Update packages automatically
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  )

;; GUI-only settings (MacOS)
(when (string= window-system "ns")
  ;; disable scrollbars
  (scroll-bar-mode -1)
  ;; disable tool bar
  (tool-bar-mode -1)
  ;; disable tooltips on hover
  (tooltip-mode -1)
  ;; add margins
  (set-fringe-mode 12)
  ;; disable beeps
  (setq ring-bell-function 'ignore)
  ;; set default font attributes
  (set-face-attribute
   'default
   nil
   :font "SF Mono"
   :height 140
   )
  ;; Install and enable gruvbox-theme
  (use-package gruvbox-theme
    :config
    (load-theme 'gruvbox-light-medium t)
    )
  ;; Use Command + w to delete window
  (global-set-key (kbd "s-w") 'delete-window)
  ;; Use Command + Shift + w to delete frame
  (global-set-key (kbd "s-W") 'delete-frame)
  )

;; Vim keybindings
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer
    ders/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
   )
  (ders/leader-keys
    :keymaps 'normal
    "." 'find-file
    "SPC" 'projectile-find-file
    "/" 'projectile-ripgrep
    "gb" 'magit-blame
    "gg" 'magit-status
    "pa" 'projectile-add-known-project
    "pd" 'projectile-remove-known-project
    "pp" 'projectile-switch-project
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wo" 'delete-other-windows
    "ws" 'evil-window-split
    "wu" 'winner-undo
    "wU" 'winner-redo
    "wv" 'evil-window-vsplit
    "ww" 'evil-window-next
    "w=" 'balance-windows
   )
  )
(defun ders/evil-hook ()
  (dolist
      (mode
       '(
	 eshell-mode
	 shell-mode
	 term-mode
	 ))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package evil
  :custom
  (evil-want-integrate t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-want-fine-undo t)
  :hook
  (evil-mode . ders/evil-hook)
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))
(evil-mode 1)

;; Git
(use-package magit)

;; Completions
;; Replace C-x f (find-file), C-x b(view-buffer), M-x, and C-s
(use-package ivy
  :bind
  (
   ("C-s" . 'swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill)
   )
  :config
  (ivy-mode)
  )
(use-package swiper)

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-icon nil)
  )

;; Emacs Keybinding help
(use-package which-key
  :init (which-key-mode)
  :custom (which-key-idle-delay 0.3)
  )

;; Project management
;; Searching with projectile-ripgrep requires ripgrep executable, as well as launching emacs from the terminal with `emacs &`
(use-package projectile
  :custom
  ;; native is built in to emacs; it is slower, but it respects .projectile ignores
  (projectile-indexing-method 'native)
  :config
  (add-to-list
   'projectile-globally-ignored-directories
   "^\\node_modules$"
   )
  (projectile-mode)
  )

;; Searching
;; Install ripgrep package
(use-package rg)

;; Web syntax highlighting
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.webc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  )
(setq css-indent-offset 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(rg projectile web-mode evil-collection evil general magit which-key rainbow-delimiters gruvbox-theme doom-modeline counsel auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
