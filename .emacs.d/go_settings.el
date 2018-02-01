(setenv "GOPATH" "/home/david/src/go")
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
  ;(setenv "GOPATH" "/Users/david/src/go")
  ;(setenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/david/bin:/Users/david/Library/Android/sdk/platform-tools/:/Users/david/Library/Android/sdk/tools:/usr/local/go/bin:/Users/david/src/go/bin:/Users/david/src/go/src/github.com/jazznetworks/main/users/dbu:/usr/local/opt/go/libexec/bin")
)


(require 'flymake)
(require 'flycheck)
(require 'gotests)
(require 'go-guru)

;; (defun flymake-go-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "/Users/david/src/go/bin/goflymake" (list temp-file)))
;;   )
;; (push '(".+\\.go$" flymake-go-init) flymake-allowed-file-name-masks)
;; (add-hook 'go-mode-hook 'flymake-mode)


(defun dbu-go-settings ()
  (set (make-local-variable 'semantic-mode) nil)
  (local-set-key (kbd "C-c ,") 'pop-tag-mark)
  (local-set-key (kbd "C-c m") 'pop-tag-mark)
  (local-set-key (kbd "C-c .") 'godef-jump)
  (local-set-key (kbd "C-c u") 'go-guru-referrers)
  (local-set-key (kbd "C-c t") 'gotests-region)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq show-trailing-whitespace t)
  (subword-mode 1)
  ;; (flymake-mode 1)
  (flycheck-mode 1)
  ;; (auto-complete-mode 0)
)
 


(add-hook 'go-mode-hook 'dbu-go-settings)


(flycheck-define-checker go-gofmt
  "A Go syntax and style checker using the gofmt utility."
  :command ("goimports" "-e" source)
  :error-patterns ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes 'go-mode
  :next-checkers 'go-build
)
(add-to-list 'flycheck-checkers 'gofmt)


(flycheck-define-checker go-build
  "A Go syntax and type checker using the `go build' command.
See URL `http://golang.org/cmd/go'."
  ;; We need to use `temporary-file-name' instead of `null-device', because Go
  ;; can't write to the null device.  It's “too magic”.  See
  ;; https://code.google.com/p/go/issues/detail?id=4851 for details.
  :command ("go" "build" "-o" temporary-file-name)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes go-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))
)
(add-to-list 'flycheck-checkers 'go-build)

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

;; (require 'flycheck-gometalinter)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))
(setq flycheck-display-errors-delay 0.1)
(setq flycheck-highlighting-mode 'lines)


(defun go-instrument-returns ()
  (interactive)
  (save-excursion
    (save-restriction
      (let ((cnt 0))
        (narrow-to-defun)
        (beginning-of-defun)
        (while (re-search-forward "^[[:space:]]+return")
          (setq cnt (1+ cnt))
          (beginning-of-line)
          (open-line 1)
          (funcall indent-line-function)
          (insert (format "log.Println(\"return statement %d\") /* RETURN INSTRUMENT */" cnt))
          (forward-line 2))))))

(defun go-deinstrument-returns ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (beginning-of-defun)
      (while (re-search-forward "^.+/\\* RETURN INSTRUMENT \\*/\n" nil t)
        (replace-match "" nil nil)))))

(defun go-neat-struct ()
  (interactive)
  (save-excursion
    (search-forward "{")
    (let ((start-level (go-paren-level))
          (start-pos (point)))
      (reindent-then-newline-and-indent)
      (while (and (>= (go-paren-level) start-level)
                  (search-forward "," nil t))
        (if (= (go-paren-level) start-level)
            (reindent-then-newline-and-indent)))
      (goto-char (1- start-pos))
      (forward-list)
      (backward-char)
      (insert ",")
      (reindent-then-newline-and-indent))))


(defun go-guru--find-enclosing (elems)
  (let* ((enclosing (go-guru--enclosing)))
    (cl-find-if (lambda (el)
                  (member (cdr (assoc 'desc el)) elems))
                enclosing)))

(defun go-extract-variable (&optional identifier)
  "Extract the function call under point into its own variable."
  (interactive)
  (let* ((undo-inhibit-record-point t)
         (call (or (go-guru--find-enclosing '("function call (or conversion)"))
                   (go-guru--find-enclosing '("selector")))))
    (when call
      (let* ((start (1+ (cdr (assoc 'start call))))
             (end (1+ (cdr (assoc 'end call))))
             (code (buffer-substring-no-properties start end))
             prev
             found)
        (if (not identifier)
            (setq identifier (read-string (format "Extract %s into variable: " code))))
        (delete-region start end)
        (save-excursion
          (insert identifier)
          (goto-char start)
          (dolist (el (append (go-guru--enclosing) nil))
            (when (and (not found)
                       (or
                        (equal (cdr (assoc 'desc el)) "block")
                        (equal (cdr (assoc 'desc el)) "case clause")
                        (equal (cdr (assoc 'desc el)) "communication clause")))
              (goto-char (1+ (cdr (assoc 'start prev))))
              (insert (format "%s := %s\n" identifier code))
              (funcall indent-line-function)
              (setq found t))
            (setq prev el)))))))

(setq flycheck-checker-error-threshold 1000)

(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))
