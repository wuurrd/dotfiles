(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-copy-env "GOPATH"))


(require 'flymake)

(defun flymake-go-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "goflymake" (list temp-file))))
(push '(".+\\.go$" flymake-go-init) flymake-allowed-file-name-masks)
(add-hook 'go-mode-hook 'flymake-mode)


(defun dbu-go-settings ()
  (setq show-trailing-whitespace t)
  (local-set-key (kbd "C-c .") 'godef-jump)
  (local-set-key (kbd "C-c ,") 'pop-tag-mark)
  (subword-mode 1)
  (flymake-mode 1)
)


(add-hook 'go-mode-hook 'dbu-go-settings)


;;;
;;; Handle *helm* buffer not found issue 2014-09-24, 2015-07-01
;;
;; Further information 2015-10-12
;;  https://github.com/emacs-helm/helm/issues/1126
;;  https://github.com/emacs-ess/ESS/issues/215
;; This is an ESS bug. ess-noweb-make-variable-permanent-local disables
;; removal of the helm commands from post-command-hook.
;;
;; Following errors were seen in *Messages* after opening *.Rnw file
;;
;; Error in post-command-hook (helm--update-header-line): (error "No buffer named *helm*")
;; Error in post-command-hook (helm--maybe-update-keymap): (error "No buffer named *helm*")
;; Error in post-command-hook (helm--update-header-line): (error "No buffer named *helm*")
;; Error in post-command-hook (helm--maybe-update-keymap): (error "No buffer named *helm*")
;;
;; These were caused by helm functions unnecessarily remaining in post-command-hook.
;; These functions requires *helm* buffer to be present.
;; They should be remove-hook'ed, but it does not happen when doing *.Rnw editing.
;; The main problem is emacsclient hits this error and die.
;; Magit uses emacsclient for COMMIT messages, so it does not work.
;;
;; helm-internal function is supposed to add-hook and remove-hook these
;; Buffer: File (grep): ~/.emacs.d/elpa/helm-20150630.1150/helm.el
;; 1989                  (add-hook 'post-command-hook 'helm--maybe-update-keymap)
;; 1990                  (add-hook 'post-command-hook 'helm--update-header-line)
;; 2004       (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
;; 2005       (remove-hook 'post-command-hook 'helm--update-header-line)
;; 2144           (unless (cl-loop for h in post-command-hook
;; 2147             (add-hook 'post-command-hook 'helm--maybe-update-keymap)
;; 2148             (add-hook 'post-command-hook 'helm--update-header-line))
;; 3421 ;; Now we run this in post-command-hook, it is
;; 3879   ;; This should be used in `post-command-hook',
;;
;; Define a function to remove helm functions from post-command-hook
(defun remove-helm-functions ()
  (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
  ;; 2015-07-01 The following function was also remaining in the hook.
  ;; This hook was added 14 days ago coinciding breakage.
  ;; https://github.com/emacs-helm/helm/commit/ff7c54d39501d894fdb06e049828b291327540e6
  (remove-hook 'post-command-hook 'helm--update-header-line))
;;
;; 2015-07-01
;; This function itself is not remaining in the post-command-hook?
;;
;; Candidate hooks for making this happen.
;; server-done-hook	Hook run when done editing a buffer for the Emacs server.
;; server-mode-hook	Hook run after entering or leaving `server-mode'.
;; server-switch-hook	Hook run when switching to a buffer for the Emacs server.
;; server-visit-hook	Hook run when visiting a file for the Emacs server.
;;
;; (add-hook 'server-done-hook   'remove-helm--maybe-update-keymap)
;; (add-hook 'server-mode-hook   'remove-helm--maybe-update-keymap)
;; (add-hook 'server-switch-hook 'remove-helm--maybe-update-keymap)
;; (add-hook 'server-visit-hook  'remove-helm--maybe-update-keymap)
;;
;; This hacky universal solution works.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Overview.html#Command-Overview
;; (add-hook 'post-command-hook 'remove-helm-functions)
;; 2015-07-01 Changed to the following.
(add-hook 'pre-command-hook 'remove-helm-functions)
