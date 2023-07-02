
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 10)))
(setq mouse-wheel-progressive-speed nil)
(delete-selection-mode 1)
(global-auto-revert-mode 1) ;; 自动加载外部修改过的文件

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)


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
(setq use-package-always-ensure t)
(require 'use-package)

(use-package cnfonts
  :ensure t
  :config (progn
	    (cnfonts-mode 1)
	    (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
	    (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
	    ))

;; (use-package dirvish
;;   :ensure t
;;   :config (dirvish-override-dired-mode))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	      history-length 1000
	      savehist-additional-variables '(mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring
					      extended-command-history)
	      savehist-autosave-interval 300)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t)
  )

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  :config (recentf-mode t))

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
  :ensure t
  :config (vertico-mode t))

(use-package orderless
  :defer 1
  :ensure t
  :after (vertico)
  :init (setq completion-styles '(orderless)))

(use-package marginalia
  :defer 3
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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


(use-package which-key
  :defer 5
  :ensure t
  :config
  (which-key-mode))

(use-package vundo
  :defer 3
  :after (evil)
  :ensure t)



(use-package yasnippet
  :ensure t
  :defer 4
  :config (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(add-to-list 'load-path "~/emacs-plugin/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(add-hook 'lsp-bridge-ref-mode-hook '(lambda () (evil-emacs-state t))) ;; j/k 可以直接跳转到下一项
;; (add-hook 'embark-collect-mode-hook '(lambda () (evil-emacs-state t)))
;; (add-hook 'helpful-mode-hook '(lambda () (evil-emacs-state t)))
;; (add-hook 'magit-mode-hook '(lambda () (evil-emacs-state t)))
;; (add-hook 'special-mode-hook '(lambda () (evil-emacs-state t)))


(use-package dumb-jump
  :defer 5
  :ensure t
  :config (dumb-jump-mode t)
  )

(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))


;; (add-to-list 'load-path "~/emacs-plugin/blink-search")
;; (require 'blink-search)



(defun consult-directory-externally (file)
  (interactive)
  (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
	    (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk)))
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))
  
(use-package avy
  :defer 3
  :ensure t
  :bind ("C-'" . avy-goto-char-timer))

(use-package projectile
  :ensure t
  :defer 3
  :commands (projectile-switch-project projectile-switch-to-buffer projectile-find-file)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

(use-package counsel
  :defer 3
  :ensure t
  :commands (counsel-apropos))

(use-package keycast
  :defer 5
  :ensure t
  )




(use-package evil
  :ensure t
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
    (evil-define-key 'normal 'global (kbd "<localleader>f") 'find-file)
    (evil-define-key 'normal 'global (kbd "<leader>fc") 'my-open-current-directory)
    (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
    (evil-define-key 'normal 'global (kbd "<localleader>b") 'switch-to-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>ss") 'consult-line)
    (evil-define-key 'normal 'global (kbd "<localleader>s") 'consult-line)
    (evil-define-key 'normal 'global (kbd "<leader>sn") 'consult-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>qq") 'consult-ripgrep)
    (evil-define-key 'normal 'global (kbd "<localleader>q") 'consult-ripgrep)
    (evil-define-key 'normal 'global (kbd "<leader>rr") 'consult-recent-file)
    (evil-define-key 'normal 'global (kbd "<localleader>r") 'consult-recent-file)
    (evil-define-key 'normal 'global (kbd "<leader>hf") 'helpful-callable)
    (evil-define-key 'normal 'global (kbd "<leader>hv") 'helpful-variable)
    (evil-define-key 'normal 'global (kbd "<leader>hk") 'helpful-key)
    (evil-define-key 'normal 'global (kbd "<leader>ha") 'counsel-apropos)
    (evil-define-key 'normal 'global (kbd "<localleader>a") 'counsel-apropos)
    (evil-define-key 'normal 'global (kbd "<leader>hb") 'embark-bindings)
    (evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
    (evil-define-key 'normal 'global (kbd "<localleader>p") 'projectile-switch-project)
    (evil-define-key 'normal 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file)
    (evil-define-key 'normal 'global (kbd "<leader>pr") 'projectile-ripgrep)
    (evil-define-key 'normal 'global (kbd "<leader>dd") 'dirvish)
    (evil-define-key 'normal 'global (kbd "<localleader>1") 'winum-select-window-1)
    (evil-define-key 'normal 'global (kbd "<localleader>2") 'winum-select-window-2)
    (evil-define-key 'normal 'global (kbd "<localleader>3") 'winum-select-window-3)
    (evil-define-key 'normal 'global (kbd "<localleader>4") 'winum-select-window-4)
    (evil-define-key 'normal 'global (kbd "<localleader>0") 'delete-other-windows)
    (evil-define-key 'normal 'global (kbd "<localleader>/") 'split-window-right)
    (evil-define-key 'normal 'global (kbd "<localleader>-") 'split-window-below)
    (define-key evil-normal-state-map (kbd "gd") 'lsp-bridge-jump)
    (define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-jump)
    (evil-define-key 'normal 'global (kbd "<leader>md") 'lsp-bridge-jump)
    (evil-define-key 'normal 'global (kbd "<localleader>d") 'lsp-bridge-jump)
    (evil-define-key 'normal 'global (kbd "<leader>mr") 'lsp-bridge-find-references)
    (evil-define-key 'normal 'global (kbd "<leader>mo") 'lsp-bridge-popup-documentation)
    (evil-define-key 'normal 'global (kbd "<leader>ma") 'lsp-bridge-diagnostic-list)
    (evil-define-key 'normal 'global (kbd "<leader>mr") 'quickrun)
    (evil-define-key '(normal visual) 'global "u" (lambda () (interactive) (if (not (fboundp 'vundo)) (evil-undo 1) (vundo) (vundo-backward 1))))
  ))
  

(use-package evil-nerd-commenter
  :after (evil)
  :ensure t
  :config (evilnc-default-hotkeys))

(use-package evil-surround
  :after (evil)
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :after (evil)
  :ensure t
  :config
  (progn
    (setq-default evil-escape-delay 0.3)
    (setq evil-escape-excluded-major-modes '(dired-mode))
    (setq-default evil-escape-key-sequence "kj")
    (evil-escape-mode 1)
))


(defun smart-q ()
    "Delete window in read-only buffers, otherwise record macro."
    (interactive)
    (if buffer-read-only
        (if (= 1 (count-windows))
            (bury-buffer)
          (delete-window))
      (call-interactively 'evil-record-macro)))
(define-key evil-normal-state-map (kbd "q") 'smart-q)

(use-package magit
  :commands (magit-status magit)
  :ensure t)


(use-package go-mode
  :ensure t 
  :mode ("\\.go\\'" . go-mode))


(use-package rust-mode
  :ensure t 
  :mode ("\\.rs\\'" . rust-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  )


;; (use-package esup
;;   :ensure t)

(use-package quickrun
  :commands (quickrun)
  :ensure t)


(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))
(run-with-idle-timer 4 nil #'my-cleanup-gc)

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
   '(evil-surround epc treesit-auto yasnippet vertico use-package
		   orderless markdown-mode marginalia keycast gcmh
		   embark-consult diminish counsel cnfonts)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "gray80" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#008B45" :underline t)))))
