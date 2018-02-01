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

(require 'helm-find-files-in-project)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq ring-bell-function 'ignore)
(setq helm-buffers-fuzzy-matching t)

(defun duplicate-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
      (duplicate-line)
  )
)
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
(require 'helm-config)
(helm-mode 1)
(setq helm-ag-command-option "--smart-case --ignore=node_modules")
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(delete-selection-mode 1)
(setq compile-command "~/src/mcu/buildtools/pexbuildv2 configure build install -p")

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
(require 'javascript)

(if (eq system-type 'darwin)
    (global-set-key (kbd "s-M") 'toggle-max-frame)
)
(setq scss-compile-at-save nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq bookmark-save-flag 1)
(require 'smartscan)
(global-smartscan-mode 1)

(require 'auto-complete)
(setq ac-auto-show-menu    0.2)
(setq ac-delay             0.2)
(setq ac-fuzzy-enable      t)
(setq ac-menu-height       20)
(setq ac-auto-start t)
(setq ac-show-menu-immediately-on-auto-complete t)
(show-paren-mode 1)
(global-auto-revert-mode 1)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(defun runtest-pex ()
  (interactive)
  (let ((bnds  (bounds-of-thing-at-point 'line)))
    (shell-command-on-region (car bnds) (cdr bnds) "sh ~/bin/runtest.sh &")
  )
)
;(global-set-key (kbd "C-\\") 'runtest-pex)
;; (require 'ace-isearch)
;; (global-ace-isearch-mode +1)
;; (setq ace-isearch-function-from-isearch 'helm-occur-from-isearch)

;; (custom-set-variables
;;  '(ace-isearch-input-length 12)
;;  '(ace-isearch-jump-delay 0.9)
;; )

(load-file "~/dotfiles/.emacs.d/pexip.el")
;(require 'setup-paredit)
(require 'smartparens)
(require 'smartparens-config)
(add-hook 'c-mode-hook (lambda () (smartparens-mode 1)))
(add-hook 'python-mode-hook (lambda () (smartparens-mode 1)))
(add-hook 'js2-mode-hook (lambda () (smartparens-mode 1)))
(add-hook 'js-mode-hook (lambda () (smartparens-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-mode 1)))

(defun sp--my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-with-modes '(c-mode c++-mode js-mode js2-mode java-mode
                        typescript-mode perl-mode)
  (sp-local-pair "{" nil :post-handlers
                 '((sp--my-create-newline-and-enter-sexp "RET"))))

(require 'keybindings)
(require 'dbu-diminish)
(smartparens-global-mode 1)
(require 'magit_settings)
(setq compilation-scroll-output t)
(setq gc-cons-threshold 20000000)

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))

(advice-add 'magit-pull-from-upstream ; This is `F u'.
            :after #'run-projectile-invalidate-cache)


(defun flymake-display-warning (warning) 
  "Display a warning to the user, using lwarn"
  (message warning))

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t) (sh . t) (C . t)))

(el-get)
(package-initialize)
