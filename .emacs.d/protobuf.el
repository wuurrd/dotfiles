(defun my-protobuf-settings ()
  (setq c-basic-offset 4)
)

(use-package protobuf-mode :ensure t
  :config
  (add-hook 'protobuf-mode-hook 'my-protobuf-settings)
  :bind (
    ("M-j" . nil)
    ("C-m" . 'newline-and-indent)
  )
)
