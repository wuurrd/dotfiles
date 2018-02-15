(setenv "GOPATH" "/home/david/src/go")
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
)



(use-package go-guru :ensure t
  :config
  (defun go-guru-set-current-package-as-main ()
    (interactive)
    (let* ((filename (buffer-file-name))
           (gopath-src-path (concat (file-name-as-directory (go-guess-gopath)) "src"))
           (relative-package-path (directory-file-name (file-name-directory (file-relative-name filename gopath-src-path)))))
      (setq go-guru-scope relative-package-path)))
)

(use-package flycheck :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-disabled-checkers
   (append flycheck-disabled-checkers
	   '(javascript-jshint)))
  (setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(json-jsonlist)))
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-pylintrc "~/dotfiles/pylintrc")
)

(load-file "~/dotfiles/.emacs.d/gotests.el")

(defun dbu-go-settings ()
  (subword-mode 1)
  (flycheck-mode 1)
  (push 'ac-source-yasnippet ac-sources)
  (add-hook 'before-save-hook #'gofmt-before-save)
)


(use-package go-mode
  :init
  (setq show-trailing-whitespace t)
  (set (make-local-variable 'semantic-mode) nil)
  :ensure t
  :after (go-guru flycheck)
  :config
  (add-hook 'go-mode-hook 'dbu-go-settings)
  :bind (
    :map go-mode-map
    ("C-c ," . pop-tag-mark)
    ("C-c m" . pop-tag-mark)
    ("C-c ." . godef-jump)
    ("C-c u" . go-guru-referrers)
    ("C-c t" . gotests-region)
    ("C-m" . 'newline-and-indent)
    ("C-c a" . 'go-guru-expand-region)
  )
)

(use-package flycheck :ensure t
  :init
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-checker-error-threshold 10000)
  :config
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
)

;; (require 'flycheck-gometalinter)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))


