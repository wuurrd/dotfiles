(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  :config
  (projectile-global-mode)
  (global-set-key (kbd "C-x C-S-p") 'projectile-find-file-dwim)
  (global-set-key (kbd "C-x C-p") 'projectile-find-file)
  )

(defun remove-helm-functions ()
  (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
  ;; 2015-07-01 The following function was also remaining in the hook.
  ;; This hook was added 14 days ago coinciding breakage.
  ;; https://github.com/emacs-helm/helm/commit/ff7c54d39501d894fdb06e049828b291327540e6
  (remove-hook 'post-command-hook 'helm--update-header-line))

(use-package helm
  :after (projectile)
  :init
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-ag-command-option "--smart-case --ignore=node_modules --ignore=elm-stuff")
  :ensure t
  :config
  (helm-mode 1)
  ;(add-hook 'pre-command-hook 'remove-helm-functions)
  :bind (
    :map global-map
    ("M-x" . 'helm-M-x)
    ("M-y" . 'helm-show-kill-ring)
    ("C-x C-f" . 'helm-find-files)
    ("C-x b" . 'helm-buffers-list)
    :map helm-map
    ("<tab>" . 'helm-execute-persistent-action) ; rebind tab to run persistent action
    ("C-z" . 'helm-select-action) ; list actions using C-z
  )
)

(use-package ag
  :ensure t
)

(use-package helm-ag
  :ensure t
  :after (helm ag)
  :bind (
    :map global-map
    ("C-c p w" . 'helm-do-ag)
  )
)
  
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :init
  (setq projectile-completion-system 'helm)
)

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil)
)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun dbu-rebase ()
  (interactive)
  (smartscan-mode 0)
  )

(use-package magit
  :after (projectile helm)
  :ensure t
  :init
  :config
  (advice-add 'magit-pull-from-upstream ; This is `F u'.
            :after #'run-projectile-invalidate-cache)
  (global-set-key (kbd "C-S-g") 'magit-status)
  (add-hook 'git-rebase-mode-hook 'dbu-rebase)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :bind (
    :map magit-status-mode-map
    ("q" . 'magit-quit-session)
  )
)
