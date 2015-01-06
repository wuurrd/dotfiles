(defvar my-packages
      '(actionscript-mode ag ample-regexps auto-complete cl-lib color-theme company-mode ctable dash deferred duplicate-line el-get elpy epc epl exec-path-from-shell expand-region fuzzy git-modes helm highlight-current-line highlight-indentation idomenu jedi keyfreq lua-mode magit move-text multiple-cursors pkg-info popup projectile pymacs python-environment pyvenv s sublimity xcscope yasnippet))

(el-get 'sync my-packages)
