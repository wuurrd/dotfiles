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

(defun open-org()
  (interactive)
  (find-file "~/Dropbox/org/organizer.org")
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


(use-package org-bullets :ensure t
  )

(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

(use-package org :ensure t
  :commands org-mode
  :after org-bullets
  :init
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-expiry-inactive-timestamps t)
  (setq org-clock-idle-time nil)
  (setq org-log-done 'time)
  (setq org-clock-continuously nil)
  (setq org-clock-persist t)
  (setq org-clock-in-switch-to-state "STARTED")
  (setq org-clock-in-resume nil)
  (setq org-show-notification-handler 'message)
  (setq org-clock-report-include-clocking-task t)
  (setq org-startup-indented nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-latex-listings 'minted)
  (setq org-log-done 'time)
  (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "STARTED(s)"
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
  (setq org-default-notes-file "~/organizer.org")
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
                             (org-bullets-mode 1)
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
    ("C-c o" . 'open-org)
    ("C-c c" . 'org-capture)
    ("C-c `" . 'org-agenda)
  )
)

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
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/src")))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.15)
 '(enable-local-variables :all)
 '(magit-pull-arguments (quote ("--rebase")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/calendar.org" "~/src/exploit/nsm2/report.org" "~/Dropbox/org/fromhome.org" "~/Dropbox/sørkedalsveien/styremote.org" "~/Dropbox/org/organizer.org")))
 '(package-selected-packages
   (quote
    (terraform-mode oauth2 org-caldav rjsx-mode gnu-elpa-keyring-update yasnippet pager)))
 '(revert-without-query (quote (".*\\.pdf"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-template-field ((t (:foreground "#DFAF8F" :background "#2B2B2B"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(js2-error ((((class color)) (:underline "red"))))
 '(js2-external-variable ((((class color)) (:underline "red"))))
 '(mode-line ((t (:background "#7F7F7F" :foreground "#8FB28F" :box nil)))))
