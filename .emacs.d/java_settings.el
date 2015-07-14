(require 'auto-complete-config)
(require 'ac-emacs-eclim-source)
(require 'eclimd)
(defun dbu-java-settings ()
  (custom-set-variables
  '(eclim-eclipse-dirs '("~/Desktop/eclipse"))
  '(eclim-executable "~/Desktop/eclipse/eclim"))
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (eclim-mode 1)
  (ac-config-default)
  (ac-emacs-eclim-config)
  ;(start-eclimd "/Users/david/Desktop/workspace")
)

(add-hook 'java-mode-hook 'dbu-java-settings)

(package-initialize)
