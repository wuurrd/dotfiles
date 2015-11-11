
(setq inhibit-splash-screen t)
(column-number-mode)
(server-start)
(ido-mode t)
(setq ido-case-fold  t)
(fset 'yes-or-no-p 'y-or-n-p)

;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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
(setq helm-ag-command-option "--smart-case")
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(delete-selection-mode 1)
(setq compile-command "~/src/mcu/buildtools/pexbuildv2 configure build install -p")

(global-set-key [F12] 'recompile)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)
(global-set-key "\C-xp" 'other-window-backward)
(global-set-key (kbd "C-S-g") 'magit-status)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up] 'move-text-up)

(global-set-key (kbd "M-s-SPC") 'er/expand-region)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "s-/") 'comment-dwim)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "M-?") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-p") 'projectile-find-file-in-known-projects)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c p q") 'helm-do-ag)

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

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char-save)

; Make join line not leave a space.
(defun join-previous-line ()
  (interactive)
  (join-line -1)
  (delete-char 1)
)
(global-set-key (kbd "M-j") 'join-previous-line)
(require 'javascript)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; full screen magit-status
(require 'magit)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(setq git-commit-summary-max-length 100)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun dbu-rebase ()
  (interactive)
  (smartscan-mode 0)
)
(add-hook 'git-rebase-mode-hook 'dbu-rebase)


(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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
(global-set-key (kbd "C-c p .") 'projectile-test-gstreamer)

(defun get-gstreamer-test-name ()
  "Find the gstreamer test name near point"
  (save-excursion
    (beginning-of-defun)
    (let (res)
      (save-match-data
	(re-search-forward "GST_START_TEST *\(\\(.\+\\)\)")
	(setq res (match-string 1)))
      res)))

(defun get-gstreamer-test-command ()
  "Find the gstreamer command to run test near point"
  (let ((test-name (get-gstreamer-test-name))
	(test-program)
	(test-command))
    (when (not (equal "" test-name))
      ;;; Assume that test-name starts with the name of the test-program (not always true)
      ; (string-match "\\(.\+?\\)_" test-name)
      ; (setq test-program (match-string 1 test-name))
      ;; Assume that filename has the same name as the test-program (not always true)
      (setq test-program (file-name-base (buffer-file-name)))
      (setq test-command (concat
			  "GST_CHECKS=" test-name
			  " make -C .build/linux-x86_64/__root__/$PWD/media/gst-plugins-pex/tests " ;; FIXME
			  test-program ".check")))
    test-command))

(defun projectile-test-gstreamer ()
  "Will prefill the command to run the current gstreamer test and
   call projectile-test-project"
  (interactive)
  (let ((test-command (get-gstreamer-test-command)))
    (puthash (projectile-project-root) test-command  projectile-test-cmd-map)
    (call-interactively 'projectile-test-project))
)
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(require 'ace-isearch)
(global-ace-isearch-mode +1)
(setq ace-isearch-function-from-isearch 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch)
