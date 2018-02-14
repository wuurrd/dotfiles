;; may be in an arbitrary order
(eval-when-compile (require 'cl))

(require 'prettier-js)
(setq prettier-js-command
   "/home/david/src/go/src/repo.jazznetworks.com/jazz/main/frontend/node_modules/prettier/bin/prettier.js"
)

(setq prettier-js-args '(
  "--single-quote"
  "--no-semi"
))

(use-package ac-js2 :ensure t)

(use-package js2-mode
  :ensure t
  :after ac-js2
  :init
  (setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-idle-timer-delay 0.5 ;; could not be too big for real time syntax check
      js2-indent-on-enter-key t
      js2-skip-preprocessor-directives t
      js2-auto-indent-p t
      js2-bounce-indent-p t)
  :config
  (prettier-js-mode)
  (setq mode-name "JS2")
  (electric-indent-mode 0)
  (local-set-key (kbd "C-c .") 'ac-js2-jump-to-definition)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (js2-imenu-extras-mode)
  (ac-js2-mode)
  (flycheck-mode)
  :bind (
    ("M-j" . nil)
  )
)

(provide 'javascript)
