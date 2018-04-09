(fset 'renumber-pb
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 2 backspace 24 11 9 1 14] 11 "%d")) arg)))
