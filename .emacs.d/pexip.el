(defun get-gstreamer-test-name ()
  "Find the gstreamer test name near point"
  (save-excursion
    (beginning-of-defun)
    (let (res)
      (save-match-data
	(re-search-forward "GST_START_TEST *\(\\(.\+\\)\)")
	(setq res (match-string 1)))
      res)))

(defun get-gstreamer-test-command ()
  "Find the gstreamer command to run test near point"
  (let ((test-name (get-gstreamer-test-name))
	(test-program)
	(test-command))
    (when (not (equal "" test-name))
      ;;; Assume that test-name starts with the name of the test-program (not always true)
      ; (string-match "\\(.\+?\\)_" test-name)
      ; (setq test-program (match-string 1 test-name))
      ;; Assume that filename has the same name as the test-program (not always true)
      (setq test-program (file-name-base (buffer-file-name)))
      (setq test-command (concat
			  "GST_CHECKS=" test-name
			  " make -C .build/linux-x86_64/__root__/$PWD/media/gst-plugins-pex/tests " ;; FIXME
			  test-program ".check")))
    test-command))

(defun projectile-test-gstreamer ()
  "Will prefill the command to run the current gstreamer test and
   call projectile-test-project"
  (interactive)
  (let ((test-command (get-gstreamer-test-command)))
    (puthash (projectile-project-root) test-command  projectile-test-cmd-map)
    (call-interactively 'projectile-test-project))
)

(defun get-pexcellent-test-name ()
  "Find the gstreamer test name near point"
  (save-excursion
    (python-nav-backward-up-list)
    (python-nav-backward-up-list)
    (python-nav-backward-up-list)
    (python-nav-backward-up-list)
    (python-nav-backward-up-list)
    (python-nav-backward-up-list)
    (python-nav-backward-up-list)
    (let (res)
      (save-match-data
	(re-search-forward "class *\\(.\+\\)\(")
	(setq res (match-string 1)))
      res)))

(defun get-pexcellent-test-command ()
  "Find the gstreamer command to run test near point"
  (let ((test-name (get-pexcellent-test-name))
	(test-program)
	(test-command))
    (when (not (equal "" test-name))
      ;;; Assume that test-name starts with the name of the test-program (not always true)
      ; (string-match "\\(.\+?\\)_" test-name)
      ; (setq test-program (match-string 1 test-name))
      ;; Assume that filename has the same name as the test-program (not always true)
      (setq test-program (file-name-base (buffer-file-name)))
      (setq test-command (concat
			  "DONT_RESTART_NM=1 ./buildtools/pextest -vvv --target=pexcellent --name=" test-name)))
    test-command))


(defun projectile-test-pexcellent ()
  "Will prefill the command to run the current pexcellent test and
   call projectile-test-project"
  (interactive)
  (let ((test-command (get-pexcellent-test-command)))
    (puthash (projectile-project-root) test-command  projectile-test-cmd-map)
    (call-interactively 'projectile-test-project))
)

(global-set-key (kbd "C-c p .") 'projectile-test-gstreamer)
(global-set-key (kbd "C-c p ,") 'projectile-test-pexcellent)
