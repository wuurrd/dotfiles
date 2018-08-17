(use-package auto-complete
  :ensure t
  :config
  (ac-flyspell-workaround)
  :init
  (setq ac-auto-show-menu    0.2)
  (setq ac-delay             0.2)
  (setq ac-fuzzy-enable      t)
  (setq ac-menu-height       20)
  (setq ac-auto-start t)
  (setq ac-show-menu-immediately-on-auto-complete t)
  :config
  (add-to-list 'ac-dictionary-directories "~/dotfiles/.emacs.d/ac-dict")
  (ac-config-default)
)

(use-package company :ensure t
  :config
  (defun my-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (setq company-require-match nil)
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 20)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))
  (setq company-minimum-prefix-length 1)
)
