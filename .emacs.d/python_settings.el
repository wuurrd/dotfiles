(require 'pymacs)
(require 'twisted-dev)
(require 'fill-column-indicator)

(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))

;Flymake settings.
(setq pylint "epylint")
(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
      (local-file (file-relative-name
                   temp-file
                   (file-name-directory buffer-file-name))))
     (list "/usr/local/bin/epylint" (list local-file))))

 (add-to-list 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pylint-init)))


(defun dbu-python-settings ()
  (twisted-dev-mode 1)
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (auto-complete-mode 1)
  (subword-mode 1)
  (auto-fill-mode 1)
  ; do not breakline on comments
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))
  (setq fci-rule-column 80)
  (fci-mode 1)
  (flymake-mode 1)
  (setq jedi:setup-keys t)
  (jedi:setup)
  (push 'ac-source-yasnippet ac-sources)
)

(add-hook 'python-mode-hook 'dbu-python-settings)

(package-initialize)
