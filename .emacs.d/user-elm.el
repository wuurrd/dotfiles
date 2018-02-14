(use-package elm-mode
  :ensure t
  :after (flycheck-elm)
  :init
  (setq elm-format-on-save t)
  (setq company-backends '(company-elm))
  :config
  (flycheck-mode)
  (company-mode)
  (elm-oracle-setup-completion)
  (flycheck-elm-setup)
)
