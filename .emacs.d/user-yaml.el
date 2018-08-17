(use-package yaml-mode
  :ensure t
)

(use-package restclient :ensure t
  :commands (restclient-mode)
  )

(defun rest ()
  (interactive)
  (switch-to-buffer (make-temp-name "*restclient* "))
  (restclient-mode)
  )
