(defvar my-packages '(ack-and-a-half actionscript-mode ag ample-regexps auto-complete cl-lib color-theme company-mode ctable dash deferred duplicate-line el-get elpy epc epl exec-path-from-shell expand-region find-file-in-project fiplr fuzzy git-modes grizzl helm highlight-current-line highlight-indentation idomenu jedi keyfreq lua-mode magit move-text pkg-info popup projectile pymacs python-environment pyvenv s sublimity xcscope yasnippet))

(dolist (p my-packages)
  (el-get-install p))
