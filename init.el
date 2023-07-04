(setq w32-get-true-file-attributes nil   ; decrease file IO workload
      w32-use-native-image-API t         ; use native w32 API
      w32-pipe-read-delay 0              ; faster IPC
      w32-pipe-buffer-size (* 64 1024))
(setq read-process-output-max (* 1024 1024))
(setq ffap-machine-p-known 'reject)
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

(use-package dirvish
  :commands (dirvish)
  :ensure t
  :config (dirvish-override-dired-mode))


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
  :config (doom-modeline-mode 1))


(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-duo-light)
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
  :ensure t
  :after (vertico)
  :init (setq completion-styles '(orderless)))

(use-package marginalia
  :after (vertico)
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


;; 多个#二次过滤，!筛选
(use-package consult
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

(use-package markdown-mode
  :ensure t)

(add-to-list 'load-path "~/emacs-plugin/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)



;; (use-package dumb-jump
;;   :ensure t
;;   :commands (dumb-jump-go)
;;   )

;; (defun lsp-bridge-jump ()
;;   (interactive)
;;   (cond
;;    ((eq major-mode 'emacs-lisp-mode)
;;     (let ((symb (function-called-at-point)))
;;       (when symb
;;         (find-function symb))))
;;    (lsp-bridge-mode
;;     (lsp-bridge-find-def))
;;    (t
;;     ;; (require 'dumb-jump)
;;     (dumb-jump-go))))


;; (add-to-list 'load-path "~/emacs-plugin/blink-search")
;; (require 'blink-search)


(add-to-list 'load-path "~/emacs-plugin/color-rg") ; add color-rg to your load-path
(require 'color-rg)

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
  :commands (projectile-switch-project projectile-switch-to-buffer projectile-find-file)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))


(use-package counsel
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
    (evil-set-leader '(normal visual) (kbd ","))
    (evil-set-leader '(normal visual) (kbd "SPC") t)
    (evil-define-key '(normal visual) 'global (kbd "RET") 'open-newline-below)
    (evil-define-key '(normal visual) 'global (kbd "<leader>ff") 'find-file)
    (evil-define-key '(normal visual) 'global (kbd "<leader>fc") 'my-open-current-directory)
    (evil-define-key '(normal visual) 'global (kbd "<leader>bb") 'switch-to-buffer)
    (evil-define-key '(normal visual) 'global (kbd "<leader>bk") 'kill-buffer)
    (evil-define-key '(normal visual) 'global (kbd "<leader>ss") 'consult-line)
    (evil-define-key '(normal visual) 'global (kbd "<leader>sn") 'consult-imenu)
    (evil-define-key '(normal visual) 'global (kbd "<leader>qq") 'consult-ripgrep)
    (evil-define-key '(normal visual) 'global (kbd "<leader>rr") 'consult-recent-file)
    (evil-define-key '(normal visual) 'global (kbd "<leader>hf") 'helpful-callable)
    (evil-define-key '(normal visual) 'global (kbd "<leader>hv") 'helpful-variable)
    (evil-define-key '(normal visual) 'global (kbd "<leader>hk") 'helpful-key)
    (evil-define-key '(normal visual) 'global (kbd "<leader>ha") 'counsel-apropos)
    (evil-define-key '(normal visual) 'global (kbd "<leader>hb") 'embark-bindings)
    (evil-define-key '(normal visual) 'global (kbd "<leader>pp") 'projectile-switch-project)
    (evil-define-key '(normal visual) 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)
    (evil-define-key '(normal visual) 'global (kbd "<leader>pf") 'projectile-find-file)
    ;; (evil-define-key '(normal visual) 'global (kbd "<leader>pq") 'projectile-ripgrep)
    (evil-define-key '(normal visual) 'global (kbd "<leader>dd") 'dirvish)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>1") 'winum-select-window-1)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>2") 'winum-select-window-2)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>3") 'winum-select-window-3)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>4") 'winum-select-window-4)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>9") 'kill-buffer-and-window)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>0") 'delete-other-windows)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>/") 'split-window-right)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>-") 'split-window-below)
    (evil-define-key '(normal visual) 'global (kbd "<localleader>'") 'avy-goto-char-timer)
    (define-key evil-normal-state-map (kbd "gd") 'lsp-bridge-jump)
    (define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-jump)
    (evil-define-key '(normal visual) 'global (kbd "<leader>md") 'lsp-bridge-find-def)
    (evil-define-key '(normal visual) 'global (kbd "<leader>mr") 'lsp-bridge-find-references)
    (evil-define-key '(normal visual) 'global (kbd "<leader>mo") 'lsp-bridge-popup-documentation)
    (evil-define-key '(normal visual) 'global (kbd "<leader>ma") 'lsp-bridge-diagnostic-list)
    (evil-define-key '(normal visual) 'global (kbd "<leader>mb") 'quickrun)
    (evil-define-key '(normal visual) 'global (kbd "<leader>mj") 'dumb-jump-go)
    (evil-define-key '(normal visual) 'global "u" (lambda () (interactive) (if (not (fboundp 'vundo)) (evil-undo 1) (vundo) (vundo-backward 1))))
    ))


(add-hook 'lsp-bridge-ref-mode-hook '(lambda () (evil-emacs-state))) ;; j/k 可以直接跳转到下一项
(add-hook 'embark-collect-mode-hook '(lambda () (evil-emacs-state)))
(add-hook 'helpful-mode-hook '(lambda () (evil-emacs-state)))
(add-hook 'magit-mode-hook '(lambda () (evil-emacs-state)))
(add-hook 'special-mode-hook '(lambda () (evil-emacs-state)))

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

;; (use-package citre
;;   :defer t
;;   :ensure t
;;   :init
;;   (require 'citre-config)
;;   (global-set-key (kbd "C-x c j") 'citre-jump)
;;   (global-set-key (kbd "C-x c J") 'citre-jump-back)
;;   (global-set-key (kbd "C-x c p") 'citre-ace-peek)
;;   (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
;;   :config
;;   (setq citre-project-root-function #'projectile-project-root)
;;   )


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


(add-to-list 'load-path "~/emacs-plugin/awesome-pair") ; add awesome-pair to your load-path
(require 'awesome-pair)
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook '(lambda () (awesome-pair-mode 1))))
(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
(define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
;; 写完括号里面的代码直接按右括号即可跳出括号，保证会配对
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)
;; 括号跳转
(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

(define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
(define-key awesome-pair-mode-map (kbd "RET") 'awesome-pair-newline)
;; 删文本并保持括号配对
(define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)
;; 自动给当前文本加括号
(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
;; 消括号
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)
;; 跳转
(define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

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
 '(package-selected-packages
   '(markdown-mode yasnippet-snippets winum which-key vundo vertico
		   rust-mode quickrun projectile orderless marginalia
		   magit lua-mode keycast helpful go-mode
		   evil-surround evil-nerd-commenter evil-escape
		   embark-consult elisp-demos ef-themes dumb-jump
		   doom-modeline counsel cnfonts avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
