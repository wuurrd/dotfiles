(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
          '(
            ("irc.rd.tandberg.com" "#qat" "#hackers_corner" "#slackers_corner"
             "#itvmtest" "#acdc")
           )
)

(defun irc ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.rd.tandberg.com" :port 6667
                :nick "dbu" :full-name "David Buchmann")
  )

(defun notify-desktop (title message &optional duration &optional icon)
  "Pop up a message on the desktop with an optional duration (forever otherwise)"
  (pymacs-exec "import pynotify")
  (pymacs-exec "pynotify.init('Emacs')")
  (if icon 
      (pymacs-exec (format "msg = pynotify.Notification('%s','%s','%s')"
                           title message icon))
    (pymacs-exec (format "msg = pynotify.Notification('%s','%s')" title message))
    ) 
  (if duration 
      (pymacs-exec (format "msg.set_timeout(%s)" duration))
    )
  (pymacs-exec "msg.show()")
  )

;; Notify me when someone wants to talk to me.
;; Heavily based off of ErcPageMe on emacswiki.org, with some improvements.
;; I wanted to learn and I used my own notification system with pymacs
;; Delay is on a per user, per channel basis now.
(defvar erc-page-nick-alist nil
  "Alist of 'nickname|target' and last time they triggered a notification"
  )
(defun erc-notify-allowed (nick target &optional delay)
  "Return true if a certain nick has waited long enough to notify"
  (unless delay (setq delay 30))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc (format "%s|%s" nick target) erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons (format "%s|%s" nick target) cur-time) erc-page-nick-alist)
      t)
    )
  )
(defun erc-notify-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
    (target (car (erc-response.command-args parsed)))
    (msg (erc-response.contents parsed)))
    ;;Handle true private/direct messages (non channel)
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (erc-current-nick-p target)
           (erc-notify-allowed nick target)
           )
      ;Do actual notification
      (ding)
      (notify-desktop (format "%s - %s" nick
                              (format-time-string "%b %d %I:%M %p"))
                      msg 0 "gnome-emacs")
      )
    ;;Handle channel messages when my nick is mentioned
    (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
               (string-match (erc-current-nick) msg)
               (erc-notify-allowed nick target)
           )
      ;Do actual notification
      (ding)
      (notify-desktop (format "%s - %s" target
                              (format-time-string "%b %d %I:%M %p"))
                      (format "%s: %s" nick msg) 0 "gnome-emacs")
      )
    )
      
  )

(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-PRIVMSG)
