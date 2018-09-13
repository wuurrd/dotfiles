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

(use-package org :ensure t
  :commands org-mode
  :init
  (setq org-startup-indented nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-latex-listings 'minted)
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n)"
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
  :config
  (add-hook 'org-mode-hook (lambda () (auto-revert-mode 1)))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-file-apps '(system . "xdg-open \"%s\""))
  (add-to-list 'org-file-apps '(t . "xdg-open \"%s\""))
  :bind (
    :map global-map
    ("C-c o" . 'open-org)
    ("C-c c" . 'org-capture)
  )
)
