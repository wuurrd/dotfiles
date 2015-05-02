(defvar my-packages
      '(actionscript-mode ac-js2 ag ample-regexps auto-complete change-inner cl-lib color-theme company-mode ctable dash deferred duplicate-line el-get elpy epc epl exec-path-from-shell expand-region frame-cmds fuzzy git-modes helm helm-ag highlight-current-line highlight-indentation idomenu jedi js2-mode js2-refactor keyfreq lua-mode magit move-text multiple-cursors pkg-info popup projectile pymacs python-environment pyvenv s smartscan sublimity tagedit xcscope yasnippet ws-butler))

(el-get 'sync my-packages)
