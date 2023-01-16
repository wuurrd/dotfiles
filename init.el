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

;(package-initialize)
;(setq package-check-signature nil)
(setq mac-option-key-is-meta t)
(setq mac-option-modifier 'meta)

(if (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
  )
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/emacs-goodies-el/"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/auto-install"))

;; (require 'el-get)
;; (el-get)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


(use-package exec-path-from-shell :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  (if (eq system-type 'darwin)
      (progn
        (exec-path-from-shell-copy-env "PATH")
        (exec-path-from-shell-copy-env "LANG"))
  )
)

(defun my/org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
  If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (if (gnus-alive-p)
                (gnus-with-article-headers
                 (mail-extract-address-components
                  (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))



(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

(use-package org-present
  :ensure t
  :config
  :after org
  (progn)
  :bind (
    :map org-mode-map
    ("C-c C-q" . 'org-agenda)
  ))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org :ensure t
  :commands org-mode
  :after org-bullets
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :init
  (setq
       org-hide-emphasis-markers t
       org-pretty-entities t
       org-startup-indented t
       org-startup-with-inline-images t
       org-startup-with-latex-preview t
  )
  (setq org-hide-leading-stars t)
  (setq org-expiry-inactive-timestamps t)
  (setq org-clock-idle-time nil)
  (setq org-log-done 'time)
  (setq org-clock-continuously nil)
  (setq org-clock-persist t)
  (setq org-clock-in-switch-to-state "STARTED")
  (setq org-clock-in-resume nil)
  ;; (setq org-show-notification-handler nil)
  (setq org-clock-report-include-clocking-task t)
  (setq org-startup-indented nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-latex-listings 'minted)
  (setq org-log-done 'time)
  (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "BACKLOG(t)"
           "SELECTED-FOR-DEVELOPMENT(t)"
           "STARTED(s)"
           "IN-PROGRESS(s)"
           "WAITING(w@/!)"
           "ONGOING(o)"
           "|"
           "DONE(d)"
           "CANCELLED(c@)"
           ))
        )
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
          "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
          "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))
  (setq org-default-notes-file "~/Dropbox/org/organizer.org")
  (setq org-capture-templates
        `(("t" "Tasks" entry
           (file+headline "~/organizer.org" "Inbox")
           ,my/org-basic-task-template)
          ("T" "Quick task" entry
           (file+headline "~/organizer.org" "Inbox")
           "* TODO %^{Task}\nSCHEDULED: %t\n"
           :immediate-finish t)
          ("i" "Interrupting task" entry
           (file+headline "~/organizer.org" "Inbox")
           "* STARTED %^{Task}"
           :clock-in :clock-resume)
          ("e" "Emacs idea" entry
           (file+headline "~/organizer.org" "Emacs")
           "* TODO %^{Task}"
           :immediate-finish t)
          ("p" "People task" entry
           (file+headline "~/organizer.org" "Tasks")
           ,my/org-basic-task-template)
          ("j" "Journal entry" plain
           (file+datetree "~/journal.org")
           "%K - %a\n%i\n%?\n"
           :unnarrowed t)
          ("J" "Journal entry with date" plain
           (file+datetree+prompt "~/journal.org")
           "%K - %a\n%i\n%?\n"
           :unnarrowed t)
          ("s" "Journal entry with date, scheduled" entry
           (file+datetree+prompt "~/journal.org")
           "* \n%K - %a\n%t\t%i\n%?\n"
           :unnarrowed t)
          ("dp" "Done - People" entry
           (file+headline "~/organizer.org" "Tasks")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
          ("dt" "Done - Task" entry
           (file+headline "~/organizer.org" "Inbox")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
          ("q" "Quick note" item
           (file+headline "~/organizer.org" "Quick notes"))
          ("n" "Daily note" table-line (file+olp "~/organizer.org" "Inbox")
           "| %u | %^{Note} |"
           :immediate-finish t)
          ("r" "Notes" entry
           (file+datetree "~/organizer.org")
           "* %?\n\n%i\n%U\n"
           )))
  :config
  (require 'ox-beamer)
  (add-hook 'org-mode-hook (lambda ()
                             (auto-revert-mode 1)
                             ))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (if (eq system-type 'darwin)
      (progn
        (add-to-list 'org-file-apps '(system . "open \"%s\""))
        (add-to-list 'org-file-apps '(t . "open \"%s\"")))
      (progn
        (add-to-list 'org-file-apps '(system . "xdg-open \"%s\""))
        (add-to-list 'org-file-apps '(t . "xdg-open \"%s\"")))
  )
  :bind (
    :map org-mode-map
    ("C-'" . 'forward-or-backward-sexp)
    ("C-c `" . 'org-agenda)
    :map global-map
    ("C-c c" . 'org-capture)
    ("C-c `" . 'org-agenda)
  )
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :ensure t
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (defun efs/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 80
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :ensure t
;;   :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
        )
  :config
  (progn
    (setq org-projectile-projects-file
          "~/Dropbox/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(org-babel-load-file "~/dotfiles/.emacs.d/emacs-init.org")
;(load-file "~/dotfiles/.emacs.d/fullscreen.el")
;(load-file "~/dotfiles/.emacs.d/irc_mode.el")
;(load-file "~/dotfiles/.emacs.d/user-kube.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-input-length 12)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" default))
 '(ecb-options-version "2.40")
 '(ecb-source-path '("~/src"))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.15)
 '(enable-local-variables :all)
 '(fci-rule-color "#383838")
 '(helm-ag-base-command "rg --no-heading")
 '(helm-ag-success-exit-status '(0 2))
 '(magit-pull-arguments '("--rebase"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files
   '("/home/david/Dropbox/org/organizer-work.org" "/home/david/Dropbox/org/calendar.org" "/home/david/Dropbox/org/fromhome.org" "/home/david/Dropbox/org/organizer.org"))
 '(package-selected-packages
   '(straight lsp-jedi csv-mode python-pytest graphql-mode jsonnet-mode forge org-jira cargo flycheck-rust flycheck-rus racer racer-mode flycheck-inline rust-mode poetry syntax-subword dockerfile-mode filladapt py-isort python-black terraform-mode oauth2 org-caldav rjsx-mode gnu-elpa-keyring-update yasnippet pager))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(revert-without-query '(".*\\.pdf"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(company-template-field ((t (:foreground "#DFAF8F" :background "#2B2B2B"))))
 '(flymake-errline ((((class color)) (:underline "red"))) t)
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))) t)
 '(js2-error ((((class color)) (:underline "red"))))
 '(js2-external-variable ((((class color)) (:underline "red"))))
 '(mode-line ((t (:background "#7F7F7F" :foreground "#8FB28F" :box nil)))))
