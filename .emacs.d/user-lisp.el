(defun my-lisp-settings ()
  (company-mode 1)
  (setq-local company-backends '(company-elisp))
)

(use-package lisp-mode
  :bind (
    ("C-c ." . 'xref-find-definitions)
    ("C-c ," . 'xref-pop-marker-stack)
    )
  :hook
  (lisp-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  (emacs-lisp-mode . my-lisp-settings)
)
