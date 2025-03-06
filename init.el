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
;; Refresh buffers whenever files change on disk
(global-auto-revert-mode)

;; Run emacs as a server
(server-start)

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
(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "http://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"     . 5)
        ("melpa"        . 0)))
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

;; Terminal-only settings
(when (display-graphic-p)
  ;; disable menu bar
  (menu-bar-mode -1)
  )

;; GUI-only settings
(when (display-graphic-p)
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
  ;; set fixed-pitch font attributes
  ;; emacs sometimes renders monospace stuff with a different `fixed-pitch` face. This makes it use the same as the default.
  (set-face-attribute
   'fixed-pitch
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

;; Indent with tabs with Conf mode (i.e. NGINX configuration)
(add-hook 'conf-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

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
    "pi" 'projectile-invalidate-cache
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
  (git-commit-mode . evil-insert-state)
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )
(use-package evil-collection
  :after
  evil
  :custom
  (evil-want-keybinding nil)
  :config
  (evil-collection-init)
  )
(use-package evil-surround
  :after
  evil
  :config
  (global-evil-surround-mode 1)
  )
(use-package undo-fu
  :after
  evil
  :custom
  (evil-undo-system 'undo-fu)
  )
(evil-mode 1)

;; Git
(use-package magit
  :pin melpa
  :custom
  ;; Open standard magit status in current window
  ;; See @kyleam's final suggested code snippet: https://github.com/magit/magit/issues/2541
  (magit-display-buffer-function
   (lambda (buffer)
     (display-buffer
      buffer (if (and (derived-mode-p 'magit-mode)
                      (memq (with-current-buffer buffer major-mode)
                            '(magit-process-mode
                              magit-revision-mode
                              magit-diff-mode
                              magit-stash-mode
                              magit-status-mode)))
                 nil
               '(display-buffer-same-window)))))
  )

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

;; Lua mode
(use-package lua-mode)

;; Web syntax highlighting
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  (indent-tabs-mode nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.webc\\'" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  )
(setq css-indent-offset 2)

;; Markdown
(use-package
  markdown-mode
  :pin "melpa-stable"
  )

;; Treesitter
;; see https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
(use-package treesit
  :ensure nil
  :mode (
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         )
  :preface
  (defun ap/setup-install-grammars ()
    "Install tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (make "https://github.com/alemuller/tree-sitter-make")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (ap/setup-install-grammars)
  )

;; LSP Mode
(use-package
  lsp-mode
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(lua-mode treesit markdown undo-fu evil-surround rg projectile web-mode evil-collection evil general magit which-key rainbow-delimiters gruvbox-theme doom-modeline counsel auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
