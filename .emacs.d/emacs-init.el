(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)
  )
)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(defun duplicate-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
      (duplicate-line)
  )
)

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
  )
)
;; make zap-to-char act like zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  \(fn arg char)"
    'interactive)

(defun zap-to-char-save (arg char)
  "Zap to a character, but save instead of kill."
  (interactive "p\ncZap to char: ")
  (save-excursion
    (zap-up-to-char arg char)
    (yank)))


; Make join line not leave a space.
(defun join-previous-line ()
  (interactive)
  (join-line -1)
  (delete-char 1)
)

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key "\C-xp" 'other-window-backward)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char-save)
(global-set-key (kbd "M-j") 'join-previous-line)
(global-set-key (kbd "C-c j") 'json-format)
(global-set-key (kbd "M-`") 'isearch-forward-symbol-at-point)
(if (eq system-type 'darwin)
    (global-set-key (kbd "s-M") 'toggle-max-frame)
)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "s-/") 'comment-dwim)

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(f6)] 'clipboard-kill-ring-save)
(global-set-key [(f7)] 'clipboard-yank)
(global-set-key [(f9)] 'recompile)
(global-set-key [f10] 'compile)
(global-set-key [(f11)] 'my-toggle-fullscreen)
(global-set-key [F12] 'recompile)

(global-set-key (kbd "<XF86AudioPlay>") 'spotify-playpause)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c `") 'org-agenda)

(setq inhibit-splash-screen t)
(column-number-mode)
(require 'server)
(unless (server-running-p) (server-start))
(ido-mode t)
(setq ido-case-fold  t)
(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;Buffer settings
(setq confirm-kill-emacs #'yes-or-no-p)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
(blink-cursor-mode (- (*) (*) (*)))
;(toggle-show-tabs-show-ws)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)
(setq scss-compile-at-save nil)
(setq bookmark-save-flag 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq vc-make-backup-files t)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(defvar local-directory
  (concat user-emacs-directory ".local/")
  "This variable dictates where to put auto saves"
)

(setq auto-save-list-file-prefix nil)
(setq backup-directory-alist `((".*" . ,local-directory)))
(setq auto-save-file-name-transforms  `((".*" ,local-directory t)))
(setq recentf-save-file (expand-file-name "recentf" local-directory))
(setq compilation-scroll-output t)
(setq gc-cons-threshold 40000000)
(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t) (sh . t) (C . t)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(defun flymake-display-warning (warning) 
  "Display a warning to the user, using lwarn"
  (message warning))
(setq 
 gdb-many-windows t
 gdb-show-main t
)

(setenv "GOPATH" "/home/david/src/go")
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
)

(use-package zenburn-theme :ensure t)

(setq-default scroll-up-aggressively 0)
(setq-default scroll-down-aggressively 0)
(setq-default scroll-margin 3)

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("SOMEDAY"   :foreground "goldenrod"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold)
              ("QUOTE"     :foreground "hotpink"      :weight bold)
              ("QUOTED"    :foreground "indianred1"   :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
              ("REJECTED"  :foreground "olivedrab"    :weight bold)
              ("OPEN"      :foreground "magenta"      :weight bold)
              ("CLOSED"    :foreground "forest green" :weight bold))))

(defun fontify-frame (frame)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(mode-line ((t (:background "#7F7F7F" :foreground "#8FB28F" :box nil)))) 
   '(js2-error ((((class color)) (:underline "red"))))
   '(js2-external-variable ((((class color)) (:underline "red"))))
   '(flymake-errline ((((class color)) (:underline "red"))))
   '(flymake-warnline ((((class color)) (:underline "yellow")))))
  (if (eq system-type 'darwin)
      (progn
        (set-frame-parameter frame 'font '"Source Code Pro for Powerline 13")
        (set-default-font "Source Code Pro for Powerline 13")
        )
    (set-default-font "Source Code Pro for Powerline 9")
    (set-frame-parameter frame 'font '"Source Code Pro for Powerline 9")
  )
)

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

(if (eq system-type 'darwin)
    (set-default-font "Source Code Pro for Powerline 13")
  (if (string= system-name "checkers")
      (set-default-font "Monospace 11")
    (set-default-font "Source Code Pro for Powerline 9")
    )
)

(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default cursor-type 'box)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#7F7F7F" :foreground "#8FB28F" :box nil)))) 
 '(js2-error ((((class color)) (:underline "red"))))
 '(js2-external-variable ((((class color)) (:underline "red"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  :config
  (projectile-global-mode)
  :bind (
    :map global-map
    ("C-x C-S-p" . 'projectile-find-file-dwim)
    ("C-x C-p" . 'projectile-find-file)
    :map projectile-mode-map
    ("C-c p" . 'projectile-command-map)
  )
)
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :init
  (setq projectile-completion-system 'helm)
)

(defun remove-helm-functions ()
  (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
  ;; 2015-07-01 The following function was also remaining in the hook.
  ;; This hook was added 14 days ago coinciding breakage.
  ;; https://github.com/emacs-helm/helm/commit/ff7c54d39501d894fdb06e049828b291327540e6
  (remove-hook 'post-command-hook 'helm--update-header-line))

(use-package helm
  :after (projectile)
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-echo-input-in-header-line t
        helm-ag-command-option "--smart-case --ignore=node_modules --ignore=elm-stuff --ignore=static"
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-in-side-p t
        helm-always-two-windows t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
  )
  :ensure t
  :config
  (helm-mode 1)
  ;(add-hook 'pre-command-hook 'remove-helm-functions)
  :bind (
    :map global-map
    ("M-x" . 'helm-M-x)
    ("M-y" . 'helm-show-kill-ring)
    ("C-x C-f" . 'helm-find-files)
    ("C-x b" . 'helm-buffers-list)
    :map helm-map
    ("<tab>" . 'helm-execute-persistent-action) ; rebind tab to run persistent action
    ("C-z" . 'helm-select-action) ; list actions using C-z
  )
)

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
)

(use-package helm-ag
  :ensure t
  :after (helm ag)
  :bind (
    :map global-map
    ("C-c p w" . 'helm-do-ag)
  )
)

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil)
)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun dbu-rebase ()
  (interactive)
  (smartscan-mode 0)
  )

(defun my-magit-find-master ()
  "Visit a file in master brnach"
  (interactive)
  (let ((file (magit-read-file-from-rev "master" "Find file in master" buffer-file-name)))
    (magit-find-file "master" file))
  )

(defun my-magit-find-master-this ()
  "Visit this file in master brnach"
  (interactive)
  (magit-find-file "master" buffer-file-name)
  )

(defun my-magit-ediff-master-this ()
  "Ediff this file against master brnach"
  (interactive)
  (magit-ediff-compare nil "master" (magit-current-file) (magit-current-file))
  )

(defun my-magit-merge-preview ()
  "Preview result of merging REV into the current branch."
  (interactive)
  (setq rev "origin/master")
  (magit-mode-setup #'my-magit-merge-preview-mode rev))

(define-derived-mode my-magit-merge-preview-mode magit-diff-mode "Magit Merge"
  "Mode for previewing a merge."
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun my-magit-merge-preview-refresh-buffer (rev)
  (let* ((branch rev)
         (head (magit-get-current-branch)))
    (magit-set-header-line-format (format "Preview merge of %s into %s"
                                          (or branch "HEAD")
                                          rev))
    (magit-insert-section (diffbuf)
      (magit-git-wash #'magit-diff-wash-diffs
        "merge-tree" (magit-git-string "merge-base" rev head) rev head))))

(defun my-browse-url (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (eq system-type 'darwin)
      (apply #'browse-url-default-macosx-browser url args)
    (apply #'browse-url url args)
  )
)

(use-package magit
  :after (projectile helm)
  :ensure t
  :init
  :config
  (advice-add 'magit-pull-from-upstream ; This is `F u'.
            :after #'run-projectile-invalidate-cache)
  (add-hook 'git-rebase-mode-hook 'dbu-rebase)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :bind (
    :map global-map
    ("C-S-g" . 'magit-status)
    ("C-x g" . 'magit-status)
    ("C-c m f" . 'magit-find-file)
    ("C-c m m" . 'my-magit-find-master)
    ("C-c m t" . 'my-magit-find-master-this)
    ("C-c m e" . 'my-magit-ediff-master-this)
    ("C-c m E" . 'magit-ediff-compare)
    ("C-c m b" . 'my-magit-merge-preview)
    :map magit-status-mode-map
    ("q" . 'magit-quit-session)
    :map magit-process-mode-map
    ("C-c C-o" . 'my-browse-url)
  )
)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

(use-package undo-tree
  :ensure t
  :config
  (undo-tree-mode 1)
)

(defun dbu-prettier()
  (interactive)
  (when (eq major-mode 'js2-mode) (prettier-js))
  (when (eq major-mode 'rjsx-mode) (prettier-js))
  )

(use-package prettier-js
  :ensure t
  :init
  (setq prettier-js-command
        "/home/david/.nvm/versions/node/v11.10.1/lib/node_modules/prettier/bin-prettier.js"
        )
  (setq prettier-js-args '(
                           "--single-quote"
                           "--no-semi"
                           ))
)

(defun dbu-js-settings()
  (add-hook 'before-save-hook 'dbu-prettier)
  (setq mode-name "JS2")
  (smartparens-mode 1)
  (js2-imenu-extras-mode)
  (flycheck-mode)
  (company-mode)
)

(use-package rjsx-mode
  :ensure t
  :after prettier-js
  :config
  (add-hook 'rjsx-mode-hook 'dbu-js-settings)
)

(use-package js2-mode
  :ensure t
  :after rjsx-mode
  :init
  (setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-idle-timer-delay 0.5 ;; could not be too big for real time syntax check
      js2-indent-on-enter-key t
      js2-skip-preprocessor-directives t
      js2-auto-indent-p t
      js2-strict-missing-semi-warning nil
      js2-basic-offset 2
      js2-bounce-indent-p t)
  :config
  (add-hook 'js2-mode-hook 'dbu-js-settings)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  :bind (
    ("M-j" . nil)
    ("C-c ." . 'ac-js2-jump-to-definition)
    ("RET" . 'newline-and-indent)
  )
)

(use-package smartscan :ensure t
  :config
  (global-smartscan-mode 1)
)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'c-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'python-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'js2-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'js-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-mode 1)))
  (add-hook 'go-mode-hook (lambda () (smartparens-mode 1)))
)

(use-package jump-char
  :ensure t
  :bind
  ("M-m" . jump-char-forward)
  )

(use-package move-text
  :ensure t
  :bind (
    :map global-map
    ([M-S-down] . 'move-text-down)
    ([M-S-up] . 'move-text-up)
  )
)

(use-package expand-region
  :ensure t
  :bind (
    :map global-map
    ("M-s-SPC" . 'er/expand-region)
    ("C-c a" . 'er/expand-region)
  )
  )

(use-package jumplist
  :ensure t
  :config
  (setq jumplist-hook-commands
   '(move-beginning-of-line
     end-of-visual-line
     beginning-of-defun end-of-defun
     end-of-buffer beginning-of-buffer
     sp-forward-sexp sp-backward-sexp
     helm-swoop helm-imenu helm-find-files helm-multi-files
     helm-projectile-switch-project helm-projectile-find-file
     find-function find-variable
     mark-defun mark-whole-buffer
     avy-goto-char avy-goto-char-2
     helm-gtags-find-pattern helm-gtags-find-tag-adapter helm-gtags-find-rtag-adapter
     helm-ag-select-directory
     ensime-edit-definition
     ensime-edit-definition-with-fallback
     isearch-forward))
  (setq jumplist-ex-mode t)
  :bind (
    :map global-map
    ("C->" . 'jumplist-next)
    ("C-<" . 'jumplist-previous)
  )
)

(use-package multiple-cursors
  :ensure t
  :bind (
    :map global-map
    ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
    ("C-?" . 'mc/mark-next-like-this)
    ("M-?" . 'mc/mark-all-like-this-dwim)
    ("M-!" . 'mc/mark-next-symbol-like-this)
    ("C-M-SPC" . 'set-rectangular-region-anchor)
  )
)

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/dotfiles/.emacs.d/yasnippet")
)

(use-package auto-complete
  :ensure t
  :init
  (setq ac-auto-show-menu    0.2)
  (setq ac-delay             0.2)
  (setq ac-fuzzy-enable      t)
  (setq ac-menu-height       20)
  (setq ac-auto-start t)
  (setq ac-show-menu-immediately-on-auto-complete t)
  :config
  (add-to-list 'ac-dictionary-directories "~/dotfiles/.emacs.d/ac-dict")
  (ac-flyspell-workaround)
  ;; (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  ;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;; (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;; (add-hook 'css-mode-hook 'ac-css-mode-setup)
  ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  :bind (:map ac-complete-mode-map
    ("C-n" . ac-next)
    ("C-p" . ac-previous)
    )
)

(use-package company :ensure t
  :config
  (defun my-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))
  (setq company-require-match nil)
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 20)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
  (setq company-minimum-prefix-length 1)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  ;; (define-key company-active-map [tab] 'company-complete-selection)
  :custom-face
  (company-template-field ((t (:foreground "#DFAF8F" :background "#2B2B2B"))))
  :bind (:map company-active-map
    ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1)))
    ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
    ;; ("C-f" . (lambda () (interactive)
    ;;            (if (= company-candidates-length 1)
    ;;                (company-complete-selection)
    ;;              (company-abort)
    ;;                )
    ;;            )
    ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
    ("TAB" . (lambda () (interactive) (company-complete-selection)))
    ([tab] . (lambda () (interactive) (company-complete-selection)))
    ("C-h" . nil)
    ;:map company-search-map
    ;([?\t] . company-complete-selection)
  )
)

(use-package lsp-mode
  :ensure t
  :diminish
  :config
  (setq lsp-clients-go-format-tool "gofmt")
)

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends)
)

(use-package anzu
  :ensure t
  :diminish
  :bind (:map global-map
    ("M-%" . #'anzu-query-replace-regexp)
  )
  )

(use-package pyfmt :ensure t)

(use-package jedi :ensure t)

(defun dbu-python-settings ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (jedi-mode 1)
  ;(auto-complete-mode 1)
  (company-mode 1)
  (subword-mode 1)
  ; do not breakline on comments
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))
  (yas/minor-mode-on)
  (flycheck-mode 1)
  (smartparens-mode 1)
)

(use-package python-mode
  :ensure t
  :after (company-tabnine pyfmt jedi)
  :config
  (add-hook 'python-mode-hook 'dbu-python-settings)
  (add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("^BUILD$" . python-mode))
  (setq python-shell-interpreter "ipython")
  :bind (
    :map python-mode-map
    ("C-c ," . 'xref-pop-marker-stack)
    ("C-c ." . 'jedi:goto-definition)
    ("\C-m" . 'newline-and-indent)
  )
)

(use-package go-guru :ensure t
  :config
  (defun go-guru-set-current-package-as-main ()
    (interactive)
    (let* ((filename (buffer-file-name))
           (gopath-src-path (concat (file-name-as-directory (go-guess-gopath)) "src"))
           (relative-package-path (directory-file-name (file-name-directory (file-relative-name filename gopath-src-path)))))
      (setq go-guru-scope relative-package-path)))
)


(use-package go-dlv :ensure t)

(use-package go-impl
  :ensure t
)

(use-package go-rename
  :ensure t
)

(use-package gotest
  :ensure t
)

(use-package gorepl-mode
  :ensure t
)

(use-package flycheck :ensure t
  :init
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-checker-error-threshold 10000)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-disabled-checkers
   (append flycheck-disabled-checkers
	   '(javascript-jshint)))
  (setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(json-jsonlist)))
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-pylintrc "~/dotfiles/pylintrc")
)

(load-file "~/dotfiles/.emacs.d/gotests.el")
(load-file "~/dotfiles/.emacs.d/go-autocomplete.el")

(defun dbu-go-settings ()
  (subword-mode 1)
  (setq-local flycheck-disabled-checkers '(go-unconvert go-golint go-megacheck go-errcheck))
  (flycheck-mode 1)
  (auto-complete-mode 0)
  (company-mode 1)
  (set (make-local-variable 'ac-sources) (cons '(ac-source-go) '()))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq show-trailing-whitespace t)
  (set (make-local-variable 'semantic-mode) nil)
)

(use-package go-mode
  :init
  :ensure t
  :after (go-guru flycheck gorepl-mode go-impl go-rename gotest company-tabnine)
  :config
  (add-hook 'go-mode-hook 'dbu-go-settings)
  :bind (
    :map go-mode-map
    ("C-c ," . xref-pop-marker-stack)
    ("C-c ." . godef-jump)
    ("C-c u" . go-guru-referrers)
    ("C-c t" . gotests-region)
    ("C-m" . 'newline-and-indent)
    ;("C-c a" . 'go-guru-expand-region)
    ("C-x c i" . 'helm-imenu)
    ("C-x c r" . 'go-rename)
    ("C-x c t" . 'go-test-current-test)
    ("C-x c f" . 'go-test-current-file)
    :map global-map
    ("C-'" . 'forward-or-backward-sexp)
  )
)

(use-package gradle-mode
  :ensure t
  )

(use-package groovy-mode
  :ensure t
  )

(defun dbu-elm-settings ()
  (setq elm-format-on-save t)
  (setq-local company-backends '(company-tabnine))
  (flycheck-mode)
  (company-mode)
  (flycheck-elm-setup)
)

(use-package flycheck-elm
  :ensure t
)

(use-package elm-mode
  :ensure t
  :after (flycheck-elm)
  :config
  (add-hook 'elm-mode-hook 'dbu-elm-settings)
)

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;C++ Settings.
(defun my-cpp-settings ()
  (subword-mode 1)
  (setq show-trailing-whitespace t)
  (c-set-offset 'substatement-open '0)
  (setq c++-tab-always-indent t)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-continued-statement-offset 4)
  (setq c++-empty-arglist-indent 4)
  (setq tab-width 4 indent-tabs-mode nil)
  (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c++-mode-map "\C-ce" 'c-comment-edit)
  (setq c++-auto-hungry-initial-state 'none)
  (setq c++-delete-function 'backward-delete-char)
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-gccsense-member) 
                          '(ac-source-gccsense-static-member)))
  (auto-complete-mode 1)
)
(add-hook 'c++-mode-hook 'my-cpp-settings)

(defun my-c-settings ()
  (c-set-style "ellemtel")
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (subword-mode 1)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil)
  (let ((offset 4))
    (setq tab-width offset)
    (setq c-basic-offset offset)
    (setq c-brace-offset (* -1 offset))
    (setq c-continued-statement-offset (* 2 offset))
    (setq c-label-offset (* -1 offset))
    (setq c-argdecl-indent 0)
    (setq c-indent-level offset)
  )
  (setq c-tab-always-indent t)
  (company-mode 1)
  ;(auto-complete-mode 1)
  (smartparens-mode 1)
  (electric-indent-mode 0)
)

;; BSD-ish indentation style
(add-hook 'c-mode-hook 'my-c-settings)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c g") 'ff-find-other-file) 
            (local-set-key (kbd "C-c .") 'helm-cscope-find-symbol)
            ))

;; See http://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html
(defvar c-elements-to-align-with-spaces
  (list 'func-decl-cont
	'topmost-intro-cont
	'arglist-cont
	'arglist-cont-nonempty
	'statement-cont
	'c
	'inher-cont
	'member-init-cont
	'template-args-cont
	'objc-method-args-cont
	'objc-method-call-cont)
  "List of syntactic elements that should be aligned with spaces.
If you find an element you want to align with spaces but is not handled here,
find the syntactic element with C-c C-s or M-x c-show-syntactic-information
and simply add it to the list.")


(defun c-context-continuation-p (context)
  "Returns t if the given context is part of a continuation, i.e.
it should be aligned with spaces. The syntactic elements defined
as being a part of a continuation is defined by the variable
c-elements-to-align-with-spaces."
  (let ((continuation nil))
    (dolist (elem c-elements-to-align-with-spaces continuation)
      (when (assq elem context)
	(setq continuation t)))))


(defun c-indent-align-with-spaces-hook ()
  "If indent-tabs-mode is nil this function does nothing. If
indent-tabs-mode is enabled and if current indentation is an
alignment operation, this function will format the line so that
tabs are used until the indent level of the previous line and use
spaces for the rest (the aligment)."
  (interactive)
  (when indent-tabs-mode
    (let ((context c-syntactic-context)
	  (curr-indent (current-indentation))
	  (base-indent nil))
      (when (c-context-continuation-p context)
	(save-excursion
	  ;; Find indentation of nearest not-continuation context
	  (do ()
	      ((not (c-context-continuation-p context)))
	    (goto-char (c-langelem-pos (car context)))
	    (setq context (c-guess-basic-syntax)))
	  (setq base-indent (current-indentation)))
	;; Untabify region between base indent and current indent
	(let ((end (point)))
	  (save-excursion
	    (while (> (current-column) base-indent)
	      (backward-char))
	    (untabify (point) end)))
	;; We might need to adjust the marker to a more correct/practical
	;; position.
	(when (= (current-column) base-indent)
	  (back-to-indentation))))))

(defun stianse-c-mode-hook ()
  (interactive)
  (c-set-style "ellemtel")

  ;; make underscore a part of the word (M-b, M-f skips underscores)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)

  ;; (turn-on-auto-fill)

  ;;(setup-completion-ui)

  ;; indentation and whitespace
  ;(setq c-basic-offset 2)
  (setq tab-width 2)
  (setq fill-column 78)
  (toggle-truncate-lines 1) ;; Truncate lines, wrapping lines is confusing
  ;; FIXME: Should look for "tandberg" in path instead of system name
  ;;(if (or (string= system-name "sselnesm55") (string= system-name "SSelnesT500") (string= system-name "stiaseln-mac"))
  ;;    (setq indent-tabs-mode nil)
  ;; (setq indent-tabs-mode t))
  (setq indent-tabs-mode nil)

  ;; Show whitespaces, but reduce what to show. Too much is disturbing.
  (setq whitespace-style '(tabs
			   spaces
			   trailing
			   space-before-tab
			   empty
			   ;;lines-tail
			   ;;indentation
			   tab-mark
			   space-mark))
  (whitespace-mode -1)
  
  ;;(add-to-list 'before-save-hook 'whitespace-cleanup)
  ;; (setq c-special-indent-hook nil)
  ;; (add-hook 'c-special-indent-hook 'c-indent-align-with-spaces-hook)

  ;; TODO: Tetris specific, make a special indentation mode
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-cont-nonempty '++)
  ;; (c-set-offset 'arglist-cont-nonempty '+)

  ;;switch/case:  make each case line indent from switch
  (c-set-offset 'case-label '+)
  ;;make open-braces after a case: statement indent to 0 (default was '+)
  (c-set-offset 'statement-case-open 0)
  ;;(c-set-offset 'case-label 0)

  ;;syntax-highlight aggressively
  ;;  (setq font-lock-support-mode 'lazy-lock-mode)
  ;;  (setq lazy-lock-defer-contextually t)
  ;;  (setq lazy-lock-defer-time 0)

  ;;make DEL take all previous whitespace with i
  ;;(c-toggle-hungry-state 1)

  ;;make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  ;;do not impose restriction that all lines not top-level be indented at leas
  ;;1 (was imposed by gnu style by default)
  (setq c-label-minimum-indentation 0)

  ;; Set compile command
  (setq compilation-ask-about-save nil)
  (setq compilation-read-command t)
  (setq compilation-scroll-output t)
  ;; (setq compilation-window-height 10)

  )
(add-hook 'c++-mode-hook 'stianse-c-mode-hook)
(add-hook 'c-mode-hook 'stianse-c-mode-hook)

(load-file "~/dotfiles/.emacs.d/spotify.el")

(defun my-protobuf-settings ()
  (setq c-basic-offset 4)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 8)
)

(use-package protobuf-mode :ensure t
  :config
  (add-hook 'protobuf-mode-hook 'my-protobuf-settings)
  :bind (
    ("M-j" . nil)
    ("C-m" . 'newline-and-indent)
  )
)

(use-package diminish :ensure t
  :config
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "paredit" '(diminish 'paredit-mode))
  (eval-after-load "tagedit" '(diminish 'tagedit-mode))
  (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
  (eval-after-load "skewer-mode" '(diminish 'skewer-mode))
  (eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
  (eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
  (eval-after-load "smartparens" '(diminish 'smartparens-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))
  ;; (eval-after-load "ace-isearch" '(diminish 'ace-isearch-mode))
  (eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode))
  (eval-after-load "helm" '(diminish 'helm-mode))
  (eval-after-load "flymake" '(diminish 'flymake-mode))
  (eval-after-load "twisted-dev" '(diminish 'twisted-dev-mode))
  (eval-after-load "company" '(diminish 'company-mode))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "tern" '(diminish 'tern-mode))
)

(use-package perspective
  :ensure t
  :config
  (persp-mode)
  )

(use-package spray
  :ensure t
  :bind (
    :map global-map
    ("C-c s" . 'spray-mode)
    )
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
)

(use-package yaml-mode
  :ensure t
)

(use-package restclient :ensure t
  :commands (restclient-mode)
  )

(defun rest ()
  (interactive)
  (switch-to-buffer (make-temp-name "*restclient* "))
  (restclient-mode)
  )

(defun my-lisp-settings ()
  (company-mode 1)
  (setq-local company-backends '(company-elisp))
)

(use-package lisp-mode
  :bind (
    ("C-c ." . 'xref-find-definitions)
    ("C-c ," . 'xref-pop-marker-stack)
    )
  :hook
  (lisp-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  (emacs-lisp-mode . my-lisp-settings)
)

(use-package avy
  :ensure t
  :bind (
    :map global-map
    ("C-;" . 'avy-goto-char-2)
  )
)

(use-package company-tabnine
  :ensure t
  :config
  (setq custom-tabnine-always-trigger nil)
  (setq company-tabnine-insert-arguments nil)
  :init
  (push 'company-tabnine company-backends)
)

(use-package irony
  :ensure t
  :defer t
  :init
  (dolist (a-mode-hook '(c-mode-hook objc-mode-hook c++-mode-hook))
    (add-hook a-mode-hook 'irony-mode))
  :config
  (use-package company-irony
    :ensure t
    :config
    (defun my-company-irony-hook()
      (add-to-list 'company-backends 'company-irony)
      (define-key company-mode-map [remap hippie-expand]
        'company-complete))
    (add-hook 'irony-mode-hook 'my-company-irony-hook))
  (use-package flycheck-irony
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(use-package which-key :ensure t
  :commands (which-key-mode)
  :diminish
  )

(which-key-mode)

(use-package org-caldav
  :ensure t
  :config
  (setq org-caldav-url 'google
    org-caldav-resume-aborted 'always
    org-icalendar-timezone "Europe/Berlin"
    org-icalendar-include-body nil
  )
  (setq org-caldav-calendars
  '((:calendar-id "david.buchmann@gmail.com" :files ("~/Dropbox/org/calendar.org")
     :inbox "~/Dropbox/org/fromhome.org")
   ))
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq org-caldav-oauth2-available t)
)
(use-package oauth2
  :ensure t
)

(use-package terraform-mode
  :ensure t
)

(use-package python-black
 :ensure t
 :config
 (add-hook 'python-mode-hook #'python-black-on-save-mode)
)

(use-package py-isort
 :ensure t
 :config
 (add-hook 'before-save-hook #'py-isort-before-save)
)

(use-package filladapt
  :ensure t
  :config
  (setq-default filladapt-mode t)
)

(use-package dockerfile-mode
  :ensure t
)

(use-package poetry
  :ensure t
)

(defun dbu-rust-settings ()
  (subword-mode 1)
  (flycheck-mode 1)
  (auto-complete-mode 0)
  (company-mode 1)
  (setq show-trailing-whitespace t)
  (set (make-local-variable 'semantic-mode) nil)
  (setq-local company-backends '(company-tabnine company-lsp))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t)
)

(use-package flycheck-inline
  :ensure t
)

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
)

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'dbu-rust-settings)
  :hook (rust-mode . lsp)
)



(let
    ((work "~/Dropbox/org/work.el"))
  (when
      (file-exists-p work)
    (load-file work)
  )
)
