(use-package company-tabnine
  :ensure t
  :config
  (setq custom-tabnine-always-trigger nil)
  :init
  (push 'company-tabnine company-backends)
)

(use-package irony
  :ensure t
  :defer t
  :init
  (dolist (a-mode-hook '(c-mode-hook objc-mode-hook c++-mode-hook))
    (add-hook a-mode-hook 'irony-mode))
  :config
  (use-package company-irony
    :ensure t
    :config
    (defun my-company-irony-hook()
      (add-to-list 'company-backends 'company-irony)
      (define-key company-mode-map [remap hippie-expand]
        'company-complete))
    (add-hook 'irony-mode-hook 'my-company-irony-hook))
  (use-package flycheck-irony
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook 'irony-eldoc)))
