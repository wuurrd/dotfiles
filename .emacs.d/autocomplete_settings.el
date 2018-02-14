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

(use-package auto-complete
  :ensure t
  :init
  (setq ac-auto-show-menu    0.2)
  (setq ac-delay             0.2)
  (setq ac-fuzzy-enable      t)
  (setq ac-menu-height       20)
  (setq ac-auto-start t)
  (setq ac-show-menu-immediately-on-auto-complete t)
  :config
  (add-to-list 'ac-dictionary-directories "~/dotfiles/.emacs.d/ac-dict")
  (ac-config-default)
)
