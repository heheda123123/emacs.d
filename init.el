;; 禁止minibuffer和message显示无用消息，init.el最后会恢复
(setq message-log-max nil)
(setq inhibit-message t)

(setq w32-get-true-file-attributes nil   ; decrease file IO workload
      w32-use-native-image-API t         ; use native w32 API
      w32-pipe-read-delay 0              ; faster IPC
      w32-pipe-buffer-size (* 64 1024))
(setq read-process-output-max (* 1024 1024))
(setq confirm-kill-processes nil)
(setq ffap-machine-p-known 'reject)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 10)))
(setq mouse-wheel-progressive-speed nil)
(delete-selection-mode 1)
(global-auto-revert-mode 1) ;; 自动加载外部修改过的文件

(set-default-coding-systems 'utf-8)

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
;; (setq package-archives '(("gnu"    . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
;;                          ("nongnu" . "http://mirrors.bfsu.edu.cn/elpa/nongnu/")
;;                          ("melpa"  . "http://mirrors.bfsu.edu.cn/elpa/melpa/")))
(setq package-archives '(("gnu" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                         ("melpa" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")))
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

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq inable-recursive-minibuffers t ; Allow commands in minibuffers
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

;; (use-package quelpa
;;   :ensure t
;;   :commands (quelpa)
;;   :init
;;   (setq quelpa-checkout-melpa-p nil)
;;   (setq quelpa-update-melpa-p nil))


(use-package winum
  :defer 1
  :ensure t
  :config (winum-mode))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package vundo
  :defer 3
  :after (evil)
  :ensure t)



(use-package yasnippet
  :defer 8
  :ensure t
  :init
  (setq yas-verbosity 0)
  :config (yas-global-mode t)
  )
(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package markdown-mode
  :ensure t)


;; (use-package citre
;;   :ensure t
;;   :init
;;   (require 'citre-config)
;;   )

(add-to-list 'load-path "~/emacs-plugin/lsp-bridge")
(require 'lsp-bridge)
(setq acm-enable-doc nil)
(global-lsp-bridge-mode)

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
;;     (xref-find-definitions))))

(add-to-list 'load-path "~/emacs-plugin/auto-save") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
(setq auto-save-disable-predicates
      '((lambda ()
	  (string-suffix-p
	   "gpg"
	   (file-name-extension (buffer-name)) t))))

;; (use-package counsel-etags
;;   :ensure t
;;   :bind (("C-]" . counsel-etags-find-tag-at-point))
;;   :init
;;   (lambda ()
;;     (add-hook 'after-save-hook
;;               'counsel-etags-virtual-update-tags 'append 'local))
;;   :config
;;   (setq counsel-etags-update-interval 60)
;;   (push "build" counsel-etags-ignore-directories))


;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   )


(add-to-list 'load-path "~/emacs-plugin/blink-search")
;; (require 'blink-search)
(use-package blink-search
  :ensure nil
  :commands (blink-search))


(add-to-list 'load-path "~/emacs-plugin/color-rg") ; add color-rg to your load-path
(require 'color-rg)

(defun consult-directory-externally (file)
  (interactive)
  (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
									   (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk)))
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))

; 选中后按?还有其他功能
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
  :ensure t
  :commands (keycast-header-line-mode)
  )

(defun smart-q ()
  "Delete window in read-only buffers, otherwise record macro."
  (interactive)
  (if buffer-read-only
      (if (= 1 (count-windows))
          (bury-buffer)
        (delete-window))
    (call-interactively 'evil-record-macro)))


(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn
    (evil-mode 1)
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (evil-set-leader '(normal visual motion) (kbd ","))
    (evil-set-leader '(normal visual motion) (kbd "SPC") t)
    (setq evil-move-beyond-eol t)
    (evil-define-key '(normal visual motion) 'global (kbd "RET") 'open-newline-below)
    ;; file
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>ff") 'find-file)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>fc") 'my-open-current-directory)
    ;; buffer
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>bb") 'switch-to-buffer)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>bo") 'switch-to-buffer-other-window)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>be") (lambda () (interactive) (revert-buffer-with-coding-system 'utf-8)))
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>bk") 'kill-buffer)
    ;; bookmark
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>ba") 'consult-bookmark)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>bd") 'bookmark-delete)
    ;;
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>ss") 'consult-line)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>sa") 'blink-search)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>sn") 'consult-imenu)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>qq") 'consult-ripgrep)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>rr") 'consult-recent-file)
    ;; help
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>hf") 'helpful-callable)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>hv") 'helpful-variable)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>hk") 'helpful-key)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>ha") 'counsel-apropos)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>hi") 'counsel-info)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>hb") 'embark-bindings)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>hs") 'shortdoc)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>he") 'find-function)
    ;; project
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>pp") 'projectile-switch-project)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>pf") 'projectile-find-file)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>tt") 'counsel-etags-find-tag)    ;; list all tags
    ;; (evil-define-key '(normal visual motion) 'global (kbd "<leader>pq") 'projectile-ripgrep)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>dd") 'dirvish)
    ;; window
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>1") 'winum-select-window-1)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>2") 'winum-select-window-2)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>3") 'winum-select-window-3)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>4") 'winum-select-window-4)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>0") 'kill-buffer-and-window)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>o") 'delete-other-windows)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>/") 'split-window-right)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>-") 'split-window-below)
    (evil-define-key '(normal visual motion) 'global (kbd "<localleader>'") 'avy-goto-char-timer)
    ;; lsp
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>md") 'lsp-bridge-find-def)    ;; go def use lsp-bridge
    (define-key evil-normal-state-map (kbd "gd") 'lsp-bridge-find-def)
    (define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-find-def)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>mr") 'lsp-bridge-find-references)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>mo") 'lsp-bridge-popup-documentation)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>ma") 'lsp-bridge-diagnostic-list)
    ;; (evil-define-key '(normal visual motion) 'global (kbd "<leader>mt") 'counsel-etags-find-tag-at-point)    ;; go def use counsel-etags
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>mb") 'quickrun)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>ms") 'lsp-bridge-workspace-list-symbols)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>mf") 'lsp-bridge-code-format)
    ;; (evil-define-key '(normal visual motion) 'global (kbd "<leader>mj") 'xref-find-definitions)    ;; go def use dumb-jump
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>yy") 'consult-yank-from-kill-ring)
    (evil-define-key '(normal visual motion) 'global "u" (lambda () (interactive) (if (not (fboundp 'vundo)) (evil-undo 1) (vundo) (vundo-backward 1))))
    (evil-define-key '(normal visual motion) 'global "n" 'evil-backward-paragraph)
    (evil-define-key '(normal visual motion) 'global "m" 'evil-forward-paragraph)
    (define-key evil-normal-state-map (kbd "<tab>") 'evil-switch-to-windows-last-buffer)
    (define-key evil-normal-state-map (kbd "q") 'smart-q)
    (evil-define-key '(normal visual motion) 'global (kbd "<leader>e") 'one-key-menu-thing-edit)
    ))


(define-key lisp-interaction-mode-map (kbd "M-q") 'nil)
;; (global-set-key (kbd "s-f") 'find-file)
;; (global-set-key (kbd "M-q bb") 'switch-to-buffer)
;; (global-set-key (kbd "M-q bb") 'switch-to-buffer)

(use-package evil-nerd-commenter
  :after (evil)
  :ensure t
  :config (evilnc-default-hotkeys))

;; (use-package evil-quickscope
;;   :ensure t
;;   :config (global-evil-quickscope-always-mode 1))



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

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-default-alignment 'right)
  )

(use-package popper
  :ensure t
  :bind (("M-`"   . popper-toggle-latest)
	 ("C-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '(
	  ;; "\\*Messages\\*"
          ;; "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
	  "^\\*helpful.*\\*$"
	  "\\*color-rg\\*"
          ;; "^\\*eshell.*\\*$" eshell-mode ;; eshell as a popup
          ;; "^\\*shell.*\\*$"  shell-mode  ;; shell as a popup
          (compilation-mode . hide)
          )
        )
  (setq popper-display-control nil)
  ;; :config
  (popper-mode +1)
  (popper-echo-mode +1)
  )


(add-hook 'lsp-bridge-ref-mode-hook '(lambda () (evil-emacs-state))) ;; j/k 可以直接跳转到下一项
(add-hook 'color-rg-mode-hook '(lambda () (evil-emacs-state))) ;; j/k 可以直接跳转到下一项
;; (add-hook 'embark-collect-mode-hook '(lambda () (evil-emacs-state)))
;; (add-hook 'helpful-mode-hook '(lambda () (evil-emacs-state)))
;; (add-hook 'magit-mode-hook '(lambda () (evil-emacs-state)))
;; (add-hook 'special-mode-hook '(lambda () (evil-emacs-state)))





;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'consult-citre)


;; (use-package magit
;;   :commands (magit-status magit)
;;   :ensure t)

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

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


;; (use-package treesit-auto
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq treesit-auto-install 'prompt)
;;   (global-treesit-auto-mode))


;; wait replace by fingertip
;; https://manateelazycat.github.io/2021/11/26/grammatical-edit/
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
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

(define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
(define-key awesome-pair-mode-map (kbd "RET") 'awesome-pair-newline)

(define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

(define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)


(add-to-list 'load-path "~/emacs-plugin/one-key")
(add-to-list 'load-path "~/emacs-plugin/thing-edit")
(setq one-key-items-per-line 3)
(require 'one-key)
(require 'thing-edit)
(one-key-create-menu
 "THING-EDIT"
 '(
   ;; Copy.
   (("w" . "Copy Word") . thing-copy-word)
   (("s" . "Copy Symbol") . thing-copy-symbol)
   (("m" . "Copy Email") . thing-copy-email)
   (("f" . "Copy Filename") . thing-copy-filename)
   (("u" . "Copy URL") . thing-copy-url)
   (("x" . "Copy Sexp") . thing-copy-sexp)
   (("g" . "Copy Page") . thing-copy-page)
   (("t" . "Copy Sentence") . thing-copy-sentence)
   (("o" . "Copy Whitespace") . thing-copy-whitespace)
   (("i" . "Copy List") . thing-copy-list)
   (("c" . "Copy Comment") . thing-copy-comment)
   (("h" . "Copy Function") . thing-copy-defun)
   (("p" . "Copy Parentheses") . thing-copy-parentheses)
   (("l" . "Copy Line") . thing-copy-line)
   (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
   (("e" . "Copy To Line End") . thing-copy-to-line-end)
   ;; Cut.
   (("W" . "Cut Word") . thing-cut-word)
   (("S" . "Cut Symbol") . thing-cut-symbol)
   (("M" . "Cut Email") . thing-cut-email)
   (("F" . "Cut Filename") . thing-cut-filename)
   (("U" . "Cut URL") . thing-cut-url)
   (("X" . "Cut Sexp") . thing-cut-sexp)
   (("G" . "Cut Page") . thing-cut-page)
   (("T" . "Cut Sentence") . thing-cut-sentence)
   (("O" . "Cut Whitespace") . thing-cut-whitespace)
   (("I" . "Cut List") . thing-cut-list)
   (("C" . "Cut Comment") . thing-cut-comment)
   (("H" . "Cut Function") . thing-cut-defun)
   (("P" . "Cut Parentheses") . thing-cut-parentheses)
   (("L" . "Cut Line") . thing-cut-line)
   (("A" . "Cut To Line Begin") . thing-cut-to-line-beginning)
   (("E" . "Cut To Line End") . thing-cut-to-line-end)
   )
 t)



;; (use-package esup
;;   :ensure t)

;; (use-package auto-highlight-symbol
;;   :ensure t
;;   )


(use-package quickrun
  :commands (quickrun)
  :ensure t)


(setq message-log-max 1000)
(setq inhibit-message nil)

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
   '(flymake-elisp-config whitespace4r zenburn-theme yasnippet-snippets
			  winum which-key vundo vertico treesit-auto
			  smartparens shackle rust-mode restart-emacs
			  rainbow-delimiters quickrun
			  projectile-ripgrep popper php-mode
			  paredit-everywhere orderless meow
			  markdown-mode marginalia magit lua-mode
			  keycast helpful go-mode evil-surround
			  evil-nerd-commenter evil-escape
			  embark-consult elisp-demos elisp-benchmarks
			  ef-themes dumb-jump doom-modeline dirvish
			  counsel-etags cnfonts citre ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
