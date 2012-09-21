(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
; For our build system
(add-to-list 'auto-mode-alist '("genmake\\.def\\'" . python-mode))

;Python settings.
(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
      (local-file (file-relative-name
                   temp-file
                   (file-name-directory buffer-file-name))))
     (list "epylint" (list local-file))))

 (add-to-list 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pylint-init)))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-confirm-saving nil
      ropemacs-guess-project t
      ropemacs-enable-autoimport t
      )

(require 'twisted-dev)

(defun my-python-settings ()
  (twisted-dev-mode 1)
;  (ropemacs-mode "on")
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (auto-complete-mode 1)
  (subword-mode 1)
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))
)
(add-hook 'python-mode-hook 'my-python-settings)


