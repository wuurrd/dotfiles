;; (require 'pymacs)
(require 'twisted-dev)
(require 'fill-column-indicator)

(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
(add-to-list 'auto-mode-alist '("^BUILD$" . python-mode))
(setq autopep8-path "/usr/local/lib/python2.7/site-packages/autopep8.py")
(setq autopep8-args " - ")

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

(setq python-shell-interpreter "ipython")
(defun dbu-python-settings ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (auto-complete-mode 1)
  (subword-mode 1)
  ; do not breakline on comments
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))
  (setq jedi:setup-keys t)
  (jedi:setup)
  (yas-minor-mode-on)
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-yasnippet)))
  (flymake-mode 1)
  ;(push 'ac-source-yasnippet ac-sources)
  (flymake-mode 1)
)

(add-hook 'python-mode-hook 'dbu-python-settings)
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . actionscript-mode))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
  )
)
