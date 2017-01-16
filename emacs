(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Package installation

(use-package noctilux-theme
  :ensure t
  :config
  (load-theme 'noctilux t))

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (setq ac-delay 0.0)
  (setq ac-quick-help-delay 0.5)
  (ac-config-default))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package popup
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package markdown-mode
  :ensure t)

;;(use-package magit
;;  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

;;(use-package rainbow-mode
;;  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config 
  (rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
  (setq nrepl-popup-stacktraces nil)
  (add-to-list 'same-window-buffer-names "<em>nrepl<em>")
  (add-to-list 'ac-modes 'cider-mode)
  (add-to-list 'ac-modes 'cider-repl-mode)
  (eval-after-load "cider"
    '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)))

;;(use-package exec-path-from-shell
;;  :ensure t
;;  :config
;;  (exec-path-from-shell-initialize))

(use-package ac-nrepl
  :ensure t  
  :config
  (require 'ac-nrepl)
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup))

;;(use-package edit
;;  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-hook 'local-write-file-hooks
            (lambda ()
               (delete-trailing-whitespace)
              nil)))

(use-package company
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package flycheck-flow
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq vc-handled-backends nil)
(global-set-key (kbd "C-x g") 'magit-status)

(setq-default indent-tabs-mode nil)

;; UTF-8 support
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(customize-set-variable 'visible-bell t)
(customize-set-variable 'blink-cursor-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable `inhibit-startup-screen t)

(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(global-set-key (kbd "C-;") 'mc/mark-all-like-this-dwim)

(show-paren-mode 1)
