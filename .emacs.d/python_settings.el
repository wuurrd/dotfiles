;; (require 'pymacs)
(require 'twisted-dev)
(require 'fill-column-indicator)

(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
(add-to-list 'auto-mode-alist '("^BUILD$" . python-mode))

(setq python-shell-interpreter "ipython")
(defun dbu-python-settings ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (local-set-key (kbd "C-c ,") 'xref-pop-marker-stack)
  (local-set-key (kbd "C-c .") 'xref-find-definitions)
  ;(auto-complete-mode 1)
  (lsp)
  (company-mode 1)
  (subword-mode 1)
  ; do not breakline on comments
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))
  (yas/minor-mode-on)
  (flycheck-mode 1)
  (smartparens-mode 1)
)

(add-hook 'python-mode-hook 'dbu-python-settings)
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . actionscript-mode))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
  )
)

(use-package "pyfmt" :ensure t)
