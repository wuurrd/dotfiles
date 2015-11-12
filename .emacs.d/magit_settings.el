;; full screen magit-status
(require 'magit)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun dbu-rebase ()
  (interactive)
  (smartscan-mode 0)
  )
(add-hook 'git-rebase-mode-hook 'dbu-rebase)


(global-set-key (kbd "C-S-g") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'magit_settings)
