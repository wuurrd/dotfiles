(defun dbu-elm-settings ()
  (setq elm-format-on-save t)
  (setq company-backends '(company-elm))
  (flycheck-mode)
  (company-mode)
  (flycheck-elm-setup)
)

(use-package flycheck-elm
  :ensure t
)

(use-package elm-mode
  :ensure t
  :after (flycheck-elm)
  :config
  (add-hook 'elm-mode-hook 'dbu-elm-settings)
)
