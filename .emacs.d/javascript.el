(defun dbu-prettier()
  (interactive)
  (when (eq major-mode 'js2-mode) (prettier-js)))

(defun dbu-js-settings()
  (add-hook 'before-save-hook 'dbu-prettier)
  (setq mode-name "JS2")
  (smartparens-mode 1)
  (js2-imenu-extras-mode)
  (flycheck-mode)
)

(use-package prettier-js
  :ensure t
  :init
  (setq prettier-js-command
        "/Users/david/.nvm/versions/node/v13.0.1/lib/node_modules/prettier/bin-prettier.js"
        )
  (setq prettier-js-args '(
                           "--single-quote"
                           "--no-semi"
                           ))
)

(use-package js2-mode
  :ensure t
  :after prettier-js
  :init
  (setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-idle-timer-delay 0.5 ;; could not be too big for real time syntax check
      js2-indent-on-enter-key t
      js2-skip-preprocessor-directives t
      js2-auto-indent-p t
      js2-strict-missing-semi-warning nil
      js2-basic-offset 2
      js2-bounce-indent-p t)
  :config
  (add-hook 'js2-mode-hook 'dbu-js-settings)
  :bind (
    ("M-j" . nil)
    ("C-c ." . 'ac-js2-jump-to-definition)
    ("RET" . 'newline-and-indent)
  )
)

(provide 'javascript)
