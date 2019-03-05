(use-package perspective
  :ensure t
  :commands persp-mode
  )

(use-package spray
  :ensure t
  :bind (
    :map global-map
    ("C-c s" . 'spray-mode)
    )
  )
