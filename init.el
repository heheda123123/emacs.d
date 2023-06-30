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

(defun open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))


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

;; (use-package monokai-theme
;;   :ensure t
;;   :config (load-theme 'monokai t))

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t)
  )

(use-package recentf
  :defer 2
  :ensure nil
  :config (recentf-mode t))

;;(package-install 'keycast)
;;(keycast-mode-line-mode t)

;; (use-package popwin
;;   :ensure t
;;   :config (progn
;; 	    (popwin-mode t)
;; 	    (push '("^\\*helpful.*\\*$" :regexp t) popwin:special-display-config)
;; 	    ))

;; (use-package dashboard
;;   :ensure t
;;   :config (dashboard-setup-startup-hook)
;;   :init
;;   (progn
;;     (setq dashboard-banner-logo-title "hello world")
;;     (setq dashboard-startup-banner nil)
;;     (setq dashboard-center-content t)
;;     (setq dashboard-show-shortcuts nil)
;;     (setq dashboard-items '((recents  . 10)
;;                         (projects . 5)
;;                         ))
;;   ))

(use-package helpful
  :ensure t
  :bind (
	 ("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h a" . apropos)
	 )
  )
(use-package elisp-demos
  :ensure t
  :after (helpful)
  :config (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package vertico
  :defer 1
  :ensure t
  :config (vertico-mode t))

(use-package orderless
  :defer 1
  :ensure t
  :after (vertico)
  :init (setq completion-styles '(orderless)))

;; (use-package embark
;;   :defer 2
;;   :ensure t
;;   :init (setq prefix-help-command 'embark-prefix-help-command)
;;   :bind ("C-;" . embark-act))
(use-package marginalia
  :defer 3
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult
  :defer 1
  :ensure t
  :bind("C-s" . consult-line)
  )

(use-package winum
  :defer 1
  :ensure t
  :config (winum-mode))

;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))

(use-package vundo
  :defer 3
  :after (evil)
  :ensure t)


;; (add-to-list 'load-path "~/emacs-plugin/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)

;; (require 'yasnippet)
;; (yas-global-mode 1)
(use-package yasnippet
  :ensure t
  :defer 4
  :config (yas-global-mode 1))

(add-to-list 'load-path "~/emacs-plugin/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(add-hook 'lsp-bridge-ref-mode-hook '(lambda () (evil-emacs-state)))
(add-hook 'embark-collect-mode-hook '(lambda () (evil-emacs-state)))
(add-hook 'helpful-mode-hook '(lambda () (evil-emacs-state)))


;; (add-to-list 'load-path "~/emacs-plugin/blink-search")
;; (require 'blink-search)
;; ;;(blink-search-start-process)
;;   ;;"Start Blink-Search process if it isn't started."
;; (setq blink-search-is-starting t)
;; (blink-search-epc-live-p blink-search-epc-process)

;;     ;;start epc server and set `blink-search-server-port'
;;     (blink-search--start-epc-server)
;;      (setq blink-search-args (append
;;                                (list blink-search-python-file)
;;                                (list (number-to-string blink-search-server-port))
;;                                ))

;;       ;;Set process arguments.
;;       (if blink-search-enable-debug
;;           (progn
;;             (setq blink-search-internal-process-prog "gdb")
;;             (setq blink-search-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" blink-search-python-command) blink-search-args)))
;;         (setq blink-search-internal-process-prog blink-search-python-command)
;;         (setq blink-search-internal-process-args blink-search-args))

;;       ;;Start python process.
;; (setq process-connection-type (not (blink-search--called-from-wsl-on-windows-p)))

;;         (setq blink-search-internal-process
;;               (apply 'start-process
;;                      blink-search-name blink-search-name
;;                      blink-search-internal-process-prog blink-search-internal-process-args))

;;       (set-process-query-on-exit-flag blink-search-internal-process nil)






(defun consult-directory-externally (file)
  (interactive)
  (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
	    (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk)))
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))
  
(use-package avy
  :ensure t
  :bind ("C-'" . avy-goto-char-timer))

(use-package projectile
  :defer 3
  :commands (projectile-switch-project projectile-switch-to-buffer projectile-find-file)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

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
    (evil-define-key 'normal 'global (kbd "RET") 'open-newline-below)
    (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
    (evil-define-key 'normal 'global (kbd "<leader>fc") 'my-open-current-directory)
    (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-line)
    (evil-define-key 'normal 'global (kbd "<leader>sn") 'consult-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>qq") 'consult-ripgrep)
    (evil-define-key 'normal 'global (kbd "<leader>rr") 'consult-recent-file)
    (evil-define-key 'normal 'global (kbd "<leader>hf") 'helpful-callable)
    (evil-define-key 'normal 'global (kbd "<leader>hv") 'helpful-variable)
    (evil-define-key 'normal 'global (kbd "<leader>hk") 'helpful-key)
    (evil-define-key 'normal 'global (kbd "<leader>ha") 'apropos)
    (evil-define-key 'normal 'global (kbd "<leader>hb") 'embark-bindings)
    (evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
    (evil-define-key 'normal 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file)
    (evil-define-key 'normal 'global (kbd "<localleader>1") 'winum-select-window-1)
    (evil-define-key 'normal 'global (kbd "<localleader>2") 'winum-select-window-2)
    (evil-define-key 'normal 'global (kbd "<localleader>3") 'winum-select-window-3)
    (evil-define-key 'normal 'global (kbd "<localleader>4") 'winum-select-window-4)
    (evil-define-key 'normal 'global (kbd "<localleader>0") 'delete-other-windows)
    (evil-define-key 'normal 'global (kbd "<localleader>/") 'split-window-right)
    (evil-define-key 'normal 'global (kbd "<localleader>-") 'split-window-below)
    (define-key evil-normal-state-map (kbd "gd") 'lsp-bridge-find-def)
    (define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-find-def)
    (evil-define-key 'normal 'global (kbd "<leader>md") 'lsp-bridge-find-def)
    (evil-define-key 'normal 'global (kbd "<leader>mr") 'lsp-bridge-find-references)
    (evil-define-key 'normal 'global (kbd "<leader>mo") 'lsp-bridge-popup-documentation)
    (evil-define-key 'normal 'global (kbd "<leader>ma") 'lsp-bridge-diagnostic-list)
    (evil-define-key '(normal visual) 'global "u" (lambda () (interactive) (if (not (fboundp 'vundo)) (evil-undo 1) (vundo) (vundo-backward 1))))
  ))
  

(use-package evil-nerd-commenter
  :after (evil)
  :ensure t
  :config (evilnc-default-hotkeys))

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

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


;;(use-package lsp-pyright
  ;;:ensure t
  ;;:hook (python-mode . (lambda ()
                          ;;(require 'lsp-pyright)
                          ;;(lsp))))  ; or lsp-deferred
;;(use-package flycheck
  ;;:ensure t
  ;;:config
  ;;(setq truncate-lines nil) ; 如果单行信息很长会自动换行
  ;;:hook
  ;;(prog-mode . flycheck-mode))
;;



;; (use-package company
;;   :defer 5
;;   :ensure t
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (setq company-tooltip-align-annotations t ; 注释贴右侧对齐
;;         company-tooltip-limit 20            ; 菜单里可选项数量
;;         company-show-numbers t              ; 显示编号（然后可以用 M-数字 快速选定某一项）
;;         company-idle-delay .2               ; 延时多少秒后弹出
;;         company-minimum-prefix-length 1     ; 至少几个字符后开始补全
;;         ))

;; (use-package company-prescient
;;   :ensure t
;;   :after (company)
;;   :config (company-prescient-mode t))

;; (use-package esup
;;   :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))
 '(package-selected-packages
   '(epc treesit-auto yasnippet vertico use-package orderless
	 markdown-mode marginalia keycast gcmh embark-consult diminish
	 counsel cnfonts)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "gray80" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#008B45" :underline t)))))
