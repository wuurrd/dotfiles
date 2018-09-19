(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
)

(defun open-org()
  (interactive)
  (find-file "~/organizer.org")
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

(use-package org-timeline :ensure t)

(use-package org :ensure t
  :commands org-mode
  :after (org-timeline)
  :init
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
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
  (add-hook 'org-mode-hook (lambda () (auto-revert-mode 1)))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-file-apps '(system . "xdg-open \"%s\""))
  (add-to-list 'org-file-apps '(t . "xdg-open \"%s\""))
  :bind (
    :map global-map
    ("C-c o" . 'open-org)
    ("C-c c" . 'org-capture)
    ("C-c `" . 'org-agenda)
  )
)
