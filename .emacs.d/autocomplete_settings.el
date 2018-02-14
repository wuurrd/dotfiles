(require 'flymake)
(require 'flymake-cursor)
;This needs to be run after rope is initialised
;(global-set-key "\C-xp" 'other-window-backward)

;Autocompletion
(use-package yasnippet :ensure t
  :init
  (setq yas/trigger-key (kbd "C-c C-e"))
  :config
  (yas/load-directory "~/dotfiles/.emacs.d/yasnippet/snippets")
)

(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/dotfiles/.emacs.d/ac-dict")
(ac-config-default)
;(global-auto-complete-mode t)
;(when (require 'auto-complete nil t)
;  (require 'auto-complete-python)
;  (require 'auto-complete-css)
;  (require 'auto-complete-cpp)
;  (require 'auto-complete-emacs-lisp)
;  (require 'auto-complete-gtags)
;  (require 'auto-complete-yasnippet)
;
;  (global-auto-complete-mode t)
;  (setq ac-auto-start 3)
;  (setq ac-dwim t)
;  (set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer ac-source-files-in-current-dir ac-source-symbols))
;)
;
(defun remove-tail (item)
  (car (split-string item ":"))
)

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))
