(require 'color-theme)
;(color-theme-initialize)
;(color-theme-pok-wob)
;(require 'color-theme-almost-monokai)
;(color-theme-almost-monokai)
(load-theme 'zenburn t)

;; (require 'highlight-current-line)
;; (highlight-current-line-on t)
;; (set-face-background 'highlight-current-line-face "#303030")

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "yellow"       :weight bold)
              ("SOMEDAY"   :foreground "goldenrod"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold)
              ("QUOTE"     :foreground "hotpink"      :weight bold)
              ("QUOTED"    :foreground "indianred1"   :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
              ("REJECTED"  :foreground "olivedrab"    :weight bold)
              ("OPEN"      :foreground "magenta"      :weight bold)
              ("CLOSED"    :foreground "forest green" :weight bold))))

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Ubuntu Mono 11"))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

(set-default-font "Monospace 14")
(if (eq system-type 'darwin)
    (set-default-font "Sauce Code Pro for Powerline 13")
)
(setq-default tab-width 4 indent-tabs-mode nil)

(require 'powerline)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(setq-default cursor-type 'bar)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#7F7F7F" :foreground "#8FB28F" :box nil)))) 
 '(js2-error ((((class color)) (:underline "red"))))
 '(js2-external-variable ((((class color)) (:underline "red"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))
