(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  :config
  (projectile-global-mode)
  :bind (
    :map global-map
    ("C-x C-S-p" . 'projectile-find-file-dwim)
    ("C-x C-p" . 'projectile-find-file)
    :map projectile-mode-map
    ("C-c p" . 'projectile-command-map)
  )
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
  (setq helm-ag-command-option "--smart-case --ignore=node_modules --ignore=elm-stuff --ignore=static")
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

(defun my-magit-find-master ()
  "Visit a file in master brnach"
  (interactive)
  (let ((file (magit-read-file-from-rev "master" "Find file in master" buffer-file-name)))
    (magit-find-file "master" file))
  )

(defun my-magit-find-master-this ()
  "Visit this file in master brnach"
  (interactive)
  (magit-find-file "master" buffer-file-name)
  )

(defun my-magit-ediff-master-this ()
  "Ediff this file against master brnach"
  (interactive)
  (magit-ediff-compare nil "master" (magit-current-file) (magit-current-file))
  )

(defun my-magit-merge-preview ()
  "Preview result of merging REV into the current branch."
  (interactive)
  (setq rev "origin/master")
  (magit-mode-setup #'my-magit-merge-preview-mode rev))

(define-derived-mode my-magit-merge-preview-mode magit-diff-mode "Magit Merge"
  "Mode for previewing a merge."
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun my-magit-merge-preview-refresh-buffer (rev)
  (let* ((branch rev)
         (head (magit-get-current-branch)))
    (magit-set-header-line-format (format "Preview merge of %s into %s"
                                          (or branch "HEAD")
                                          rev))
    (magit-insert-section (diffbuf)
      (magit-git-wash #'magit-diff-wash-diffs
        "merge-tree" (magit-git-string "merge-base" rev head) rev head))))

(defun my-browse-url (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (eq system-type 'darwin)
      (apply #'browse-url-chrome url args)
    (apply #'browse-url url args)
  )
)

(use-package magit
  :after (projectile helm)
  :ensure t
  :init
  :config
  (advice-add 'magit-pull-from-upstream ; This is `F u'.
            :after #'run-projectile-invalidate-cache)
  (add-hook 'git-rebase-mode-hook 'dbu-rebase)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :bind (
    :map global-map
    ("C-S-g" . 'magit-status)
    ("C-x g" . 'magit-status)
    ("C-c m f" . 'magit-find-file)
    ("C-c m m" . 'my-magit-find-master)
    ("C-c m t" . 'my-magit-find-master-this)
    ("C-c m e" . 'my-magit-ediff-master-this)
    ("C-c m E" . 'magit-ediff-compare)
    ("C-c m b" . 'my-magit-merge-preview)
    :map magit-status-mode-map
    ("q" . 'magit-quit-session)
    :map magit-process-mode-map
    ("C-c C-o" . 'my-browse-url)
  )
)
