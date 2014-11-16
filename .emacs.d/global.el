
(setq inhibit-splash-screen t)
(column-number-mode)
(server-start)
(ido-mode t)
(setq ido-case-fold  t)
(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;Buffer settings
(setq confirm-kill-emacs #'yes-or-no-p)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(require 'auto-install)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/src")))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.15)
 '(enable-local-variables :all)
 '(revert-without-query (quote (".*\\.pdf"))))

(setq org-todo-keywords (quote ((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)")
                                (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))


(blink-cursor-mode (- (*) (*) (*)))
;(toggle-show-tabs-show-ws)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(unless (require 'el-get nil t) 
  (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") 
    (goto-char (point-max)) 
    (eval-print-last-sexp)))
(el-get 'sync)

;(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

; Needs to be initialised after rope.
(global-set-key "\C-xp" 'other-window-backward)

(require 'helm-find-files-in-project)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-x C-p") 'helm-projectile)
(setq ring-bell-function 'ignore)
(setq helm-buffers-fuzzy-matching t)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-S-d") 'duplicate-line)

(global-set-key (kbd "C-S-g") 'magit-status)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up] 'move-text-up)
(electric-pair-mode)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-SPC") 'er/expand-region)
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
