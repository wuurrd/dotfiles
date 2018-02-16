(defun my-c-settings ()
  (c-set-style "ellemtel")
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (subword-mode 1)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil)
  (let ((offset 4))
    (setq tab-width offset)
    (setq c-basic-offset offset)
    (setq c-brace-offset (* -1 offset))
    (setq c-continued-statement-offset (* 2 offset))
    (setq c-label-offset (* -1 offset))
    (setq c-argdecl-indent 0)
    (setq c-indent-level offset)
  )
  (setq c-tab-always-indent t)
  (company-mode 1)
  (auto-complete-mode 1)
  (paredit-mode 1)
  (semantic-mode 1)
  (electric-indent-mode 0)
)

;; BSD-ish indentation style
(add-hook 'c-mode-hook 'my-c-settings)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c g") 'ff-find-other-file) 
            (local-set-key (kbd "C-c .") 'helm-cscope-find-symbol)
            ))

;; See http://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html
(defvar c-elements-to-align-with-spaces
  (list 'func-decl-cont
	'topmost-intro-cont
	'arglist-cont
	'arglist-cont-nonempty
	'statement-cont
	'c
	'inher-cont
	'member-init-cont
	'template-args-cont
	'objc-method-args-cont
	'objc-method-call-cont)
  "List of syntactic elements that should be aligned with spaces.
If you find an element you want to align with spaces but is not handled here,
find the syntactic element with C-c C-s or M-x c-show-syntactic-information
and simply add it to the list.")


(defun c-context-continuation-p (context)
  "Returns t if the given context is part of a continuation, i.e.
it should be aligned with spaces. The syntactic elements defined
as being a part of a continuation is defined by the variable
c-elements-to-align-with-spaces."
  (let ((continuation nil))
    (dolist (elem c-elements-to-align-with-spaces continuation)
      (when (assq elem context)
	(setq continuation t)))))


(defun c-indent-align-with-spaces-hook ()
  "If indent-tabs-mode is nil this function does nothing. If
indent-tabs-mode is enabled and if current indentation is an
alignment operation, this function will format the line so that
tabs are used until the indent level of the previous line and use
spaces for the rest (the aligment)."
  (interactive)
  (when indent-tabs-mode
    (let ((context c-syntactic-context)
	  (curr-indent (current-indentation))
	  (base-indent nil))
      (when (c-context-continuation-p context)
	(save-excursion
	  ;; Find indentation of nearest not-continuation context
	  (do ()
	      ((not (c-context-continuation-p context)))
	    (goto-char (c-langelem-pos (car context)))
	    (setq context (c-guess-basic-syntax)))
	  (setq base-indent (current-indentation)))
	;; Untabify region between base indent and current indent
	(let ((end (point)))
	  (save-excursion
	    (while (> (current-column) base-indent)
	      (backward-char))
	    (untabify (point) end)))
	;; We might need to adjust the marker to a more correct/practical
	;; position.
	(when (= (current-column) base-indent)
	  (back-to-indentation))))))

(defun stianse-c-mode-hook ()
  (interactive)
  (c-set-style "ellemtel")

  ;; make underscore a part of the word (M-b, M-f skips underscores)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)

  ;; (turn-on-auto-fill)

  ;;(setup-completion-ui)

  ;; indentation and whitespace
  ;(setq c-basic-offset 2)
  (setq tab-width 2)
  (setq fill-column 78)
  (toggle-truncate-lines 1) ;; Truncate lines, wrapping lines is confusing
  ;; FIXME: Should look for "tandberg" in path instead of system name
  ;;(if (or (string= system-name "sselnesm55") (string= system-name "SSelnesT500") (string= system-name "stiaseln-mac"))
  ;;    (setq indent-tabs-mode nil)
  ;; (setq indent-tabs-mode t))
  (setq indent-tabs-mode nil)

  ;; Show whitespaces, but reduce what to show. Too much is disturbing.
  (setq whitespace-style '(tabs
			   spaces
			   trailing
			   space-before-tab
			   empty
			   ;;lines-tail
			   ;;indentation
			   tab-mark
			   space-mark))
  (whitespace-mode -1)
  
  ;;(add-to-list 'before-save-hook 'whitespace-cleanup)
  ;; (setq c-special-indent-hook nil)
  ;; (add-hook 'c-special-indent-hook 'c-indent-align-with-spaces-hook)

  ;; TODO: Tetris specific, make a special indentation mode
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-cont-nonempty '++)
  ;; (c-set-offset 'arglist-cont-nonempty '+)

  ;;switch/case:  make each case line indent from switch
  (c-set-offset 'case-label '+)
  ;;make open-braces after a case: statement indent to 0 (default was '+)
  (c-set-offset 'statement-case-open 0)
  ;;(c-set-offset 'case-label 0)

  ;;syntax-highlight aggressively
  ;;  (setq font-lock-support-mode 'lazy-lock-mode)
  ;;  (setq lazy-lock-defer-contextually t)
  ;;  (setq lazy-lock-defer-time 0)

  ;;make DEL take all previous whitespace with i
  ;;(c-toggle-hungry-state 1)

  ;;make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  ;;do not impose restriction that all lines not top-level be indented at leas
  ;;1 (was imposed by gnu style by default)
  (setq c-label-minimum-indentation 0)

  ;; Set compile command
  (setq compilation-ask-about-save nil)
  (setq compilation-read-command t)
  (setq compilation-scroll-output t)
  ;; (setq compilation-window-height 10)

  )
(add-hook 'c++-mode-hook 'stianse-c-mode-hook)
(add-hook 'c-mode-hook 'stianse-c-mode-hook)
