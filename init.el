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

;(package-initialize)

(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/emacs-goodies-el/"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/dotfiles/.emacs.d/auto-install"))

;; (require 'el-get)
;; (el-get)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


(use-package exec-path-from-shell :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  (if (eq system-type 'darwin)
      (exec-path-from-shell-copy-env "PATH")
  )
)

(load-file "~/dotfiles/.emacs.d/global-helm.el")
(load-file "~/dotfiles/.emacs.d/global.el")
(load-file "~/dotfiles/.emacs.d/color_settings.el")
(load-file "~/dotfiles/.emacs.d/fullscreen.el")
(load-file "~/dotfiles/.emacs.d/autocomplete_settings.el")
(load-file "~/dotfiles/.emacs.d/python_settings.el")
(load-file "~/dotfiles/.emacs.d/go_settings.el")
(load-file "~/dotfiles/.emacs.d/java_settings.el")
(load-file "~/dotfiles/.emacs.d/user-elm.el")
;(load-file "~/dotfiles/.emacs.d/ruby_settings.el")
(load-file "~/dotfiles/.emacs.d/cpp.el")
(load-file "~/dotfiles/.emacs.d/c_mode.el")
(load-file "~/dotfiles/.emacs.d/irc_mode.el")
(load-file "~/dotfiles/.emacs.d/html.el")
(load-file "~/dotfiles/.emacs.d/audio.el")
(load-file "~/dotfiles/.emacs.d/protobuf.el")
(load-file "~/dotfiles/.emacs.d/dbu-diminish.el")
(load-file "~/dotfiles/.emacs.d/user-markdown.el")
(load-file "~/dotfiles/.emacs.d/user-yaml.el")
(load-file "~/dotfiles/.emacs.d/user-lisp.el")
(load-file "~/dotfiles/.emacs.d/user-scala.el")
(load-file "~/dotfiles/.emacs.d/user-movement.el")
(load-file "~/dotfiles/.emacs.d/user-autocomplete.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-input-length 12)
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/src")))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.15)
 '(enable-local-variables :all)
 '(magit-pull-arguments (quote ("--rebase")))
 '(package-selected-packages (quote (pager)))
 '(revert-without-query (quote (".*\\.pdf"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(js2-error ((((class color)) (:underline "red"))))
 '(js2-external-variable ((((class color)) (:underline "red"))))
 '(mode-line ((t (:background "#7F7F7F" :foreground "#8FB28F" :box nil)))))
