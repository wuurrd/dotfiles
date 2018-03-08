;; may be in an arbitrary order
(eval-when-compile (require 'cl))

(require 'prettier-js)
(setq prettier-js-command
   "/home/david/src/go/src/repo.jazznetworks.com/jazz/main/frontend/node_modules/prettier/bin-prettier.js"
)

(setq prettier-js-args '(
  "--single-quote"
  "--no-semi"
))

(defun dbu-js-settings()
  (add-hook 'before-save-hook 'prettier-js 'local)
  (setq mode-name "JS2")
  (smartparens-mode 1)
  (js2-imenu-extras-mode)
  (flycheck-mode)
)

(use-package js2-mode
  :ensure t
  :init
  (setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-idle-timer-delay 0.5 ;; could not be too big for real time syntax check
      js2-indent-on-enter-key t
      js2-skip-preprocessor-directives t
      js2-auto-indent-p t
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
