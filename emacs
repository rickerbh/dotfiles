(require 'package)
(setq package-enabled-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Package installation 

(use-package paredit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-show-paths-function 'projectile-hashify-with-relative-paths))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package elm-mode
:ensure t)

(use-package noctilux-theme
  :ensure t
  :config
  (load-theme 'noctilux t))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent))))

(use-package magit
             :ensure t
             :config
             (setq vc-handled-backends nil)
             (global-set-key (kbd "C-x g") 'magit-status))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package web-mode
             :ensure t
             :config
             (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-enable-auto-pairing t)
             (setq web-mode-enable-current-element-highlight t)
             (setq web-mode-enable-current-column-highlight t)
             (add-hook 'local-write-file-hooks
                       (lambda ()
                               (delete-trailing-whitespace)
                               nil)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package markdown-preview-mode
  :ensure t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

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

(show-paren-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#292929" "#ff3333" "#aaffaa" "#aaeecc" "#aaccff" "#FF1F69" "#aadddd" "#999999"])
 '(background-color nil)
 '(background-mode dark)
 '(cursor-color nil)
 '(custom-safe-themes
   (quote
    ("4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" default)))
 '(foreground-color nil)
 '(package-selected-packages
   (quote
    (web-mode use-package rainbow-delimiters projectile paredit noctilux-theme markdown-preview-mode intero flycheck-pos-tip flycheck-flow flycheck-clojure exec-path-from-shell elm-mode aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
