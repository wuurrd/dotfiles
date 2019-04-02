;; (require 'pymacs)
(require 'twisted-dev)
(require 'fill-column-indicator)

(defun dbu-python-settings ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (jedi-mode 1)
  ;(auto-complete-mode 1)
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

(use-package pyfmt :ensure t)

(use-package jedi :ensure t)

(use-package python-mode
  :ensure t
  :after (company-tabnine pyfmt lsp-mode jedi)
  :config
  (add-hook 'python-mode-hook 'dbu-python-settings)
  (add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("^BUILD$" . python-mode))
  (setq python-shell-interpreter "ipython")
  :bind (
    :map python-mode-map
    ("C-c ," . 'xref-pop-marker-stack)
    ("C-c ." . 'jedi:goto-definition)
    ("\C-m" . 'newline-and-indent)
  )
)

(add-to-list 'auto-mode-alist '("\\.mxml\\'" . actionscript-mode))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
  )
)

