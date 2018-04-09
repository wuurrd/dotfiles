(defun my-protobuf-settings ()
  (setq c-basic-offset 4)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 8)
)

(use-package protobuf-mode :ensure t
  :config
  (add-hook 'protobuf-mode-hook 'my-protobuf-settings)
  :bind (
    ("M-j" . nil)
    ("C-m" . 'newline-and-indent)
  )
)
