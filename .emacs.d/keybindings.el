(global-set-key [F12] 'recompile)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key "\C-xp" 'other-window-backward)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "s-/") 'comment-dwim)
(global-set-key (kbd "M-s-SPC") 'er/expand-region)
(global-set-key (kbd "C-c a") 'er/expand-region)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-?") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "M-!") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)
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
