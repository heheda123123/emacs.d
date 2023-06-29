;; 界面设置
(set-face-attribute 'default nil :font (font-spec :family "FiraCode NFM" :size 25))

;; 包前的设置
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 10)))
(setq mouse-wheel-progressive-speed nil)
(delete-selection-mode 1)
(global-auto-revert-mode 1) ;; 自动加载外部修改过的文件

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f12>") 'open-init-file)

(require 'recentf)
(recentf-mode 1)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; 包设置
(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.bfsu.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.bfsu.edu.cn/elpa/melpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;;(package-install 'keycast)
;;(keycast-mode-line-mode t)

;;(use-package popwin
  ;;:ensure t
  ;;:config (popwin-mode t))

(use-package vertico
  :ensure t
  :config (vertico-mode t))

(use-package orderless
  :ensure t
  :init (setq completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :config (marginalia-mode t))

(use-package embark
  :ensure t
  :init (setq prefix-help-command 'embark-prefix-help-command)
  :bind ("C-;" . embark-act))

(use-package consult
  :ensure t
  :config (global-set-key (kbd "C-s") 'consult-line))

(use-package winum
  :ensure t
  :config (winum-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package vundo
  :ensure t)
;;(add-to-list 'load-path "~/emacs-plugin/lsp-bridge")
;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(require 'lsp-bridge)
;;(global-lsp-bridge-mode)

;;(add-to-list 'load-path "~/emacs-plugin/blink-search")
;;(require 'blink-search)
;;(require 'benchmark)
;;(message "%s" (benchmark-elapse (blink-search-start-process) (message "Hello!")))

(defun consult-directory-externally (file)
  (interactive)
  (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
	    (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk)))
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))
  

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn 
    (evil-mode 1)
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (evil-set-leader 'normal (kbd ","))
    (evil-set-leader 'normal (kbd "SPC") t)
    (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
    (evil-define-key 'normal 'global (kbd "<leader>fc") 'my-open-current-directory)
    (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-line)
    (evil-define-key 'normal 'global (kbd "<leader>qq") 'consult-ripgrep)
    (evil-define-key 'normal 'global (kbd "<leader>rr") 'consult-recent-file)
    (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
    (evil-define-key 'normal 'global (kbd "<localleader>1") 'winum-select-window-1)
    (evil-define-key 'normal 'global (kbd "<localleader>2") 'winum-select-window-2)
    (evil-define-key 'normal 'global (kbd "<localleader>3") 'winum-select-window-3)
    (evil-define-key 'normal 'global (kbd "<localleader>4") 'winum-select-window-4)
    (evil-define-key 'normal 'global (kbd "<localleader>0") 'delete-other-windows)
    (evil-define-key 'normal 'global (kbd "<localleader>/") 'split-window-right)
    (evil-define-key 'normal 'global (kbd "<localleader>-") 'split-window-below)
    (evil-define-key '(normal visual) 'global "u" (lambda () (interactive) (if (not (fboundp 'vundo)) (evil-undo 1) (vundo) (vundo-backward 1))))
  ))
  

(use-package evil-nerd-commenter
  :ensure t
  :config (evilnc-default-hotkeys))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; (use-package magit
;;   :ensure t)

;;(use-package lsp-mode
  ;;:ensure t
  ;;:init
  ;;(setq lsp-keymap-prefix "C-c l")
  ;;:hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;;(python-mode . lsp)
         ;;(cc-mode . lsp)
         ;;(c-mode . lsp)
         ;;(lsp-mode . lsp-enable-which-key-integration))
  ;;:commands lsp
  ;;:config (setq lsp-clangd-binary-path "clangd.exe"))
;;(use-package lsp-ui
  ;;:ensure t
  ;;:config
  ;;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;;(setq lsp-ui-doc-position 'top))


;;(use-package company
  ;;:ensure t
  ;;:init (global-company-mode)
  ;;:config
  ;;(setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  ;;(setq company-tooltip-align-annotations t)
  ;;(setq company-idle-delay 0.0)
  ;;(setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  ;;(setq company-selection-wrap-around t)
  ;;(setq company-transformers '(company-sort-by-occurrence)))
;;(use-package lsp-pyright
  ;;:ensure t
  ;;:hook (python-mode . (lambda ()
                          ;;(require 'lsp-pyright)
                          ;;(lsp))))  ; or lsp-deferred
;;(use-package company-box
  ;;:ensure t
  ;;:if window-system
  ;;:hook (company-mode . company-box-mode))
;;(use-package flycheck
  ;;:ensure t
  ;;:config
  ;;(setq truncate-lines nil) ; 如果单行信息很长会自动换行
  ;;:hook
  ;;(prog-mode . flycheck-mode))
;;
(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (progn 
    (projectile-mode t)
    (setq projectile-mode-line "Projectile")
  ))

;;(use-package consult-projectile
  ;;:ensure t
  ;;:after (projectile)
  ;;:init (consult-projectile))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))
 '(package-selected-packages
   '(treesit-auto yasnippet vertico use-package orderless markdown-mode
		  marginalia keycast gcmh embark-consult diminish
		  counsel cnfonts)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
