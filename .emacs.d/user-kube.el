(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :bind (
    :map global-map
    ("C-S-k" . 'kubernetes-overview)
  )
)
