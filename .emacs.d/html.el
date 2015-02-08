(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "template"))
(multi-web-global-mode 1)

;For flask / django templates
(add-to-list 'auto-mode-alist '("\\.template$" . html-mode))

;Use js2 mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
