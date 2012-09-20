;;; Commentary:
;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;
;;     "Emacs is like a laser guided missile. It only has to be slightly
;;      mis-configured to ruin your whole day."
;;                                                            [Sean McGrath]
;;
;;     "While any text editor can save your files, only Emacs can save your
;;      soul."
;;                                                          [Per Abrahamsen]
;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq confirm-kill-emacs #'yes-or-no-p)

(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/emacs-goodies-el/"))
(require 'color-theme)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

(server-start)

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

;(tool-bar-mode nil)
;(scroll-bar-mode nil)
;(menu-bar-mode nil)
(ido-mode t)
(setq 
   ido-case-fold  t                 ; be case-insensitive
)

(column-number-mode)
(setq inhibit-splash-screen t)
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/auto-install"))

(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
(add-to-list 'auto-mode-alist '("genmake\\.def\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(setq cscope-do-not-update-database t)
(load-file "/usr/share/emacs/site-lisp/xcscope.el")
(require 'xcscope)
(cscope-set-initial-directory "/home/dbu/src/main/product/apollo/charlie/host/complete/_build/charlie.host")

(require 'audel)

;(setq load-path (cons "/home/dbu/.emacs.d/twisted-dev.el" load-path))
(require 'twisted-dev)
;(require 'gccsense)
(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#303030")

;Autocompletion
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c C-e"))

(yas/initialize)
(yas/load-directory "~/dotfiles/.emacs.d/yasnippet/snippets")

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

;(global-set-key "\C-xp" 'other-window-backward)
(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(f6)] 'clipboard-kill-ring-save)
(global-set-key [(f7)] 'clipboard-yank)
(global-set-key [(f9)] 'recompile)
(global-set-key [(shift f9)] 'twisted-dev-debug-tests)
(global-set-key [(f11)] 'my-toggle-fullscreen)

;Python settings.
(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
      (local-file (file-relative-name
                   temp-file
                   (file-name-directory buffer-file-name))))
     (list "epylint" (list local-file))))

 (add-to-list 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pylint-init)))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-confirm-saving nil
      ropemacs-guess-project t
      ropemacs-enable-autoimport t
      )

(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(global-set-key "\C-xp" 'other-window-backward)

(defun my-python-settings ()
  (twisted-dev-mode 1)
;  (ropemacs-mode "on")
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (auto-complete-mode 1)
  (subword-mode 1)
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))
)
(add-hook 'python-mode-hook 'my-python-settings)

;C++ Settings.
(defun my-cpp-settings ()
  (subword-mode 1)
  (setq show-trailing-whitespace t)
  (c-set-offset 'substatement-open '0)
  (global-set-key [f10] 'compile)
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

(defun my-c-settings ()
  (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (setq show-trailing-whitespace t)
  (subword-mode 1)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq c-continued-statement-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 0)
  (setq c-label-offset -4)
  (setq c-auto-hungry-initial-state 'none)
  (setq c-delete-function 'backward-delete-char)
  (setq c-tab-always-indent t)
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-gccsense-member) 
                          '(ac-source-gccsense-static-member)))
  (auto-complete-mode 1)
)
;; BSD-ish indentation style

(add-hook 'c++-mode-hook 'my-cpp-settings)
(add-hook 'c-mode-hook 'my-c-settings)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c g") 'ff-find-other-file) 
;            (flymake-mode)
;            (gccsense-flymake-setup)
            ))
;(add-hook 'c++-mode-hook '(lambda () (flymake-mode)))
;(add-hook 'c-mode-hook '(lambda () (flymake-mode)))

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


;(run-with-idle-timer 0.1 nil 'my-toggle-fullscreen)
;(my-toggle-fullscreen)
(color-theme-initialize)
(color-theme-pok-wob)
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

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

(add-to-list 'auto-mode-alist '("\\.template$" . html-mode))

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
          '(
            ("irc.rd.tandberg.com" "#qat" "#hackers_corner" "#slackers_corner"
             "#itvmtest" "#acdc")
           )
)

(defun irc ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.rd.tandberg.com" :port 6667
                :nick "dbu" :full-name "David Buchmann")
  )

(defun notify-desktop (title message &optional duration &optional icon)
  "Pop up a message on the desktop with an optional duration (forever otherwise)"
  (pymacs-exec "import pynotify")
  (pymacs-exec "pynotify.init('Emacs')")
  (if icon 
      (pymacs-exec (format "msg = pynotify.Notification('%s','%s','%s')"
                           title message icon))
    (pymacs-exec (format "msg = pynotify.Notification('%s','%s')" title message))
    ) 
  (if duration 
      (pymacs-exec (format "msg.set_timeout(%s)" duration))
    )
  (pymacs-exec "msg.show()")
  )

;; Notify me when someone wants to talk to me.
;; Heavily based off of ErcPageMe on emacswiki.org, with some improvements.
;; I wanted to learn and I used my own notification system with pymacs
;; Delay is on a per user, per channel basis now.
(defvar erc-page-nick-alist nil
  "Alist of 'nickname|target' and last time they triggered a notification"
  )
(defun erc-notify-allowed (nick target &optional delay)
  "Return true if a certain nick has waited long enough to notify"
  (unless delay (setq delay 30))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc (format "%s|%s" nick target) erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons (format "%s|%s" nick target) cur-time) erc-page-nick-alist)
      t)
    )
  )
(defun erc-notify-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
    (target (car (erc-response.command-args parsed)))
    (msg (erc-response.contents parsed)))
    ;;Handle true private/direct messages (non channel)
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (erc-current-nick-p target)
           (erc-notify-allowed nick target)
           )
      ;Do actual notification
      (ding)
      (notify-desktop (format "%s - %s" nick
                              (format-time-string "%b %d %I:%M %p"))
                      msg 0 "gnome-emacs")
      )
    ;;Handle channel messages when my nick is mentioned
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (string-match (erc-current-nick) msg)
               (erc-notify-allowed nick target)
           )
      ;Do actual notification
      (ding)
      (notify-desktop (format "%s - %s" target
                              (format-time-string "%b %d %I:%M %p"))
                      (format "%s: %s" nick msg) 0 "gnome-emacs")
      )
    )
      
  )

(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-PRIVMSG)
(toggle-show-tabs-show-ws)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(load-file "~/dotfiles/.emacs.d/spotify.el")

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "template"))
(multi-web-global-mode 1)

;(global-ede-mode 1)
;(require 'semantic/sb)
;(semantic-mode 1)
;(ecb-minor-mode 1)

(add-to-list 'load-path "~/dotfiles/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t) 
  (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") 
(goto-char (point-max)) 
(eval-print-last-sexp)))  

(el-get 'sync)
;(require 'color-theme-almost-monokai)
;(color-theme-almost-monokai)
;(require 'google-weather)
