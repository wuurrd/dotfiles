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
  :bind (:map ac-complete-mode-map
    ("C-n" . ac-next)
    ("C-p" . ac-previous)
    )
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
  (setq company-minimum-prefix-length 0)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  ;; (define-key company-active-map [tab] 'company-complete-selection)
  :custom-face
  (company-template-field ((t (:foreground "#DFAF8F" :background "#2B2B2B"))))
  :bind (:map company-active-map
    ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1)))
    ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
    ("C-h" . nil)
    ;:map company-search-map
    ;([?\t] . company-complete-selection)
  )
)
