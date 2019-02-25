(global-set-key [F12] 'recompile)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key "\C-xp" 'other-window-backward)
(use-package move-text
  :ensure t
  :bind (
    :map global-map
    ([M-S-down] . 'move-text-down)
    ([M-S-up] . 'move-text-up)
  )
)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "s-/") 'comment-dwim)

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

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char-save)
(global-set-key (kbd "M-j") 'join-previous-line)
(global-set-key (kbd "M-`") 'isearch-forward-symbol-at-point)


(if (eq system-type 'darwin)
    (global-set-key (kbd "s-M") 'toggle-max-frame)
)





;; (require 'ace-isearch)
;; (define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch)

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(f6)] 'clipboard-kill-ring-save)
(global-set-key [(f7)] 'clipboard-yank)
(global-set-key [(f9)] 'recompile)
(global-set-key [(shift f9)] 'twisted-dev-debug-tests)
(global-set-key [f10] 'compile)
(global-set-key [(f11)] 'my-toggle-fullscreen)

(global-set-key (kbd "<XF86AudioPlay>") 'spotify-playpause)

(provide 'keybindings)
