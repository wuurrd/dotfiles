(server-start)

(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq confirm-kill-emacs #'yes-or-no-p)

(require 'color-theme)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)


(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter nil 'width 82)
           (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen)
    (my-fullscreen)))

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Ubuntu Mono 11"))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

(set-default-font "Ubuntu Mono 11")
(setq-default tab-width 4 indent-tabs-mode nil)

(ido-mode t)
(setq 
   ido-case-fold  t                 ; be case-insensitive
)

(column-number-mode)
(setq inhibit-splash-screen t)

;(run-with-idle-timer 0.1 nil 'my-toggle-fullscreen)
;(my-toggle-fullscreen)
(color-theme-initialize)
(color-theme-pok-wob)
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
(require 'auto-install)
(require 'flymake)
(require 'flymake-cursor)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

;This needs to be run after rope is initialised
;(global-set-key "\C-xp" 'other-window-backward)
(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(f6)] 'clipboard-kill-ring-save)
(global-set-key [(f7)] 'clipboard-yank)
(global-set-key [(f9)] 'recompile)
(global-set-key [(shift f9)] 'twisted-dev-debug-tests)
(global-set-key [(f11)] 'my-toggle-fullscreen)

;Autocompletion
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c C-e"))

(yas/initialize)
(yas/load-directory "~/dotfiles/.emacs.d/yasnippet/snippets")

(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#303030")

(require 'auto-complete)

;(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/dotfiles/.emacs.d/ac-dict")
(ac-config-default)
;(global-auto-complete-mode t)
;(when (require 'auto-complete nil t)
;  (require 'auto-complete-python)
;  (require 'auto-complete-css)
;  (require 'auto-complete-cpp)
;  (require 'auto-complete-emacs-lisp)
;  (require 'auto-complete-gtags)
;  (require 'auto-complete-yasnippet)
;
;  (global-auto-complete-mode t)
;  (setq ac-auto-start 3)
;  (setq ac-dwim t)
;  (set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer ac-source-files-in-current-dir ac-source-symbols))
;)
;
(defun remove-tail (item)
  (car (split-string item ":"))
)

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

;(add-hook 'python-mode-hook
;          (lambda ()
;
;                 (set (make-local-variable 'ac-sources)
;                      (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;          )
;)

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

(defun build_host (target)
  "Nonce function"

  (compile (format "ionice -c3 nice build -t %s -j8 --no-tar --nocolor" target)))

(global-set-key [f12] 'build_host)
(blink-cursor-mode (- (*) (*) (*)))
(toggle-show-tabs-show-ws)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(load-file "~/dotfiles/.emacs.d/spotify.el")
(unless (require 'el-get nil t) 
  (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") 
(goto-char (point-max)) 
(eval-print-last-sexp)))  

(el-get 'sync)
;(require 'audel)

;(setq load-path (cons "/home/dbu/.emacs.d/twisted-dev.el" load-path))
;(require 'gccsense)


;(global-ede-mode 1)
;(require 'semantic/sb)
;(semantic-mode 1)
;(ecb-minor-mode 1)

;(require 'color-theme-almost-monokai)
;(color-theme-almost-monokai)
;(require 'google-weather)
