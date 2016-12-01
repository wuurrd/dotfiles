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
			  "~/bin/ssh_session GST_CHECKS=" test-name
			  " make -C .build/linux-x86_64/__root__/pexip/media/gst-plugins-pex/tests " ;; FIXME
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
			  "~/bin/ssh_session /pexip/buildtools/pextest -vvv --target=pexcellent --name=" test-name)))
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

(setq build-prefix "")
(setq build-directory-prefix "make -C /home/vagrant/.build/linux-x86_64/__root__/home/dbu/src/mcu/")

(if (eq system-type 'darwin)
    (setq build-prefix "export GOPATH=/Users/david/src/go && "
          build-directory-prefix "~/bin/ssh_session make -C /home/vagrant/.build/linux-x86_64/__root__/pexip/")
)

;; (defun pexip-test (&optional args)
;;   (interactive (list (pexip-build-menu-arguments)))
;;   (message "The args are %s" args)
;;   (puthash (projectile-project-root)
;;            (concat build-prefix "./buildtools/pextest " (mapconcat 'identity args " "))
;;                   projectile-test-cmd-map)
;;   (call-interactively 'projectile-test-project)
;; )

;; (defun pexip-build (&optional args)
;;   (interactive (list (pexip-build-menu-arguments)))
;;   (message "The args are %s" args)
;;   (puthash (projectile-project-root)
;;            (concat build-prefix "./buildtools/pexbuildv2 configure build install --target=" (mapconcat 'identity args ",") " --no-deps -p")
;;                   projectile-test-cmd-map)
;;   (call-interactively 'projectile-test-project)
;; )

;; (require 'magit-popup)
;; (magit-define-popup pexip-build-menu-popup
;;   "Lolcats"
;;   :actions  '((?b "Build"  pexip-build)
;;               (?t "Test"  pexip-test)
;;               )
;;   :options '()
;; )

;; (magit-define-popup-switch 'pexip-build-menu-popup ?p "gst-plugins-pex" "gst-plugins-pex")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?s "si" "si,ci")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?y "gst-python" "pygobject,gobject-introspection,gst-python")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?g "gstreamer" "gstreamer")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?b "gst-plugins-bad" "gst-plugins-bad")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?v "gst-plugins-good" "gst-plugins-good")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?r "rtmpserver" "rtmpserver")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?t "unittest" "--target=si.unittest -vvv --si-unittest=si.media.tests")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?x "pexcellent" "--target=pexcellent -vvv --name=RTMP_Basic_Call")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?w "pylint" "-vvv --target=si.pylint,pexcellent.pylint")
;; (magit-define-popup-switch 'pexip-build-menu-popup ?q "slow" "-vvv --target=slow.gst.pex")

;; (global-set-key (kbd "M-=") 'pexip-build-menu-popup)

(defhydra pexip-build-menu (:color blue
                             :hint nil)
  "
Build different pexip modules

^Standard^                   ^Gstreamer^            ^Tests^
^^^^^^^^------------------------------------------------------------
_p_: Compile protobuf                             _a_: agent
                                                _b_: backend

"
  ("a" (lambda ()
         (interactive)
         (puthash (projectile-project-root)
                  (concat build-prefix "./agent/test.sh")
                  projectile-test-cmd-map)
         (call-interactively 'projectile-test-project)))
  ("b" (lambda ()
         (interactive)
         (puthash (projectile-project-root)
                  (concat build-prefix "./backend/test.sh")
                  projectile-test-cmd-map)
         (call-interactively 'projectile-test-project)))
  ("p" (lambda ()
         (interactive)
         (puthash (projectile-project-root)
                  (concat build-prefix "./lib/circuit/api/compile.sh")
                  projectile-test-cmd-map)
         (call-interactively 'projectile-test-project)))
  ("q" hydra-keyboard-quit "quit" :color blue))

(global-set-key (kbd "M-=") 'pexip-build-menu/body)
