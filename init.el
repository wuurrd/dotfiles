;;; Commentary:
;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;
;;     "Emacs is like a laser guided missile. It only has to be slightly
;;      mis-configured to ruin your whole day."
;;                                                            [Sean McGrath]
;;
;;     "While any text editor can save your files, only Emacs can save your
;;      soul."
;;                                                          [Per Abrahamsen]
;;
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/emacs-goodies-el/"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/auto-install"))
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(load-file "~/dotfiles/.emacs.d/global.el")
(load-file "~/dotfiles/.emacs.d/color_settings.el")
(load-file "~/dotfiles/.emacs.d/fullscreen.el")
(load-file "~/dotfiles/.emacs.d/autocomplete_settings.el")
(load-file "~/dotfiles/.emacs.d/python_settings.el")
;(load-file "~/dotfiles/.emacs.d/ruby_settings.el")
(load-file "~/dotfiles/.emacs.d/cpp.el")
(load-file "~/dotfiles/.emacs.d/c_mode.el")
(load-file "~/dotfiles/.emacs.d/irc_mode.el")
(load-file "~/dotfiles/.emacs.d/html.el")
(load-file "~/dotfiles/.emacs.d/audio.el")

; Needs to be initialised after rope.
(global-set-key "\C-xp" 'other-window-backward)
(require 'helm-find-files-in-project)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-x C-p") 'helm-projectile)
(setq ring-bell-function 'ignore)
