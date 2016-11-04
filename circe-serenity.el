;;; circe-serenity.el --- Clean up Circe buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Tom Willemse

;; Author: Tom Willemse <chelys@drd>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an extension module for Circe.  It creates what I think is
;; a more minimalistic view of Circe buffers.  It right-aligns all the
;; nicks, right-justifies and simplifies certain messages.  It takes
;; care of both `fill-prefix' and `wrap-prefix' so it should be usable
;; with and without `lui-fill-type'.

;; The name circe-serenity was chosen because I think the buffers feel
;; cleaner and more serene with this setup.

;;; Code:

(defvar circe-serenity-longest-nick 0
  "The longest known nick.")
(make-variable-buffer-local 'circe-serenity-longest-nick)

(defvar circe-serenity-original-format-say nil
  "The original value of `circe-format-say'.")
(defvar circe-serenity-original-format-self-say nil
  "The original value of `circe-format-self-say'.")
(defvar circe-serenity-original-format-action nil
  "The original value of `circe-format-action'.")
(defvar circe-serenity-original-format-self-action nil
  "The original value of `circe-format-self-action'.")
(defvar circe-serenity-original-format-server-message nil
  "The original value of `circe-format-server-message'.")
(defvar circe-serenity-original-format-server-join-in-channel nil
  "The original value of `circe-format-server-join-in-channel'.")
(defvar circe-serenity-original-format-server-join nil
  "The original value of `circe-format-server-join'.")
(defvar circe-serenity-original-format-server-quit nil
  "The original value of `circe-format-server-quit'.")
(defvar circe-serenity-original-format-server-quit-channel nil
  "The original value of `circe-format-server-quit-channel'.")
(defvar circe-serenity-original-format-server-part nil
  "The original value of `circe-format-server-part'.")
(defvar circe-serenity-original-format-server-nick-change nil
  "The original value of `circe-format-server-nick-change'.")
(defvar circe-serenity-original-format-server-topic nil
  "The original vlaue of `circe-format-server-topic'.")

(defun circe-serenity--fill-string ()
  (make-string (+ circe-serenity-longest-nick 3) ?\s))

(defun circe-serenity--update-longest-nick (keywords)
  (let* ((nick (plist-get keywords :nick))
         (len (length nick)))
    (when (> len circe-serenity-longest-nick)
      (setq circe-serenity-longest-nick len)
      (when lui-fill-type
        (setq-local lui-fill-type (circe-serenity--fill-string))))))

(defun circe-serenity-say-formatter (&rest keywords)
  (circe-serenity--update-longest-nick keywords)
  (propertize
   (lui-format (format "{nick:%ds}   {body}" circe-serenity-longest-nick)
               keywords)
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-self-say-formatter (&rest keywords)
  (propertize (format (format "%%%ds   %%s" circe-serenity-longest-nick)
                      ">" (plist-get keywords :body))
              'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-action-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} {body}" circe-serenity-longest-nick)
    (plist-put keywords :intro "*"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-message-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {body}" circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-join-in-channel-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} joined {channel}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro ">>>"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-join-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} logged on"
            circe-serenity-longest-nick)
    (plist-put keywords :intro ">>>"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-quit-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} logged off"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "<<<"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-quit-channel-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} left {channel}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "<<<"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-part-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} parted from {channel}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-nick-change-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {old-nick} is now know as {new-nick}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))

(defun circe-serenity-server-topic-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} changed topic to: {new-topic}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))

;;;###autoload
(defun enable-circe-serenity ()
  (interactive)
  (if (null circe-serenity-original-format-say)
      (setq circe-serenity-original-format-say
            circe-format-say))
  (setq circe-format-say #'circe-serenity-say-formatter)

  (if (null circe-serenity-original-format-self-say)
      (setq circe-serenity-original-format-self-say
            circe-format-self-say))
  (setq circe-format-self-say #'circe-serenity-say-formatter)

  (if (null circe-serenity-original-format-action)
      (setq circe-serenity-original-format-action
            circe-format-action))
  (setq circe-format-action #'circe-serenity-action-formatter)

  (if (null circe-serenity-original-format-self-action)
      (setq circe-serenity-original-format-self-action
            circe-format-self-action))
  (setq circe-format-self-action #'circe-serenity-action-formatter)

  (if (null circe-serenity-original-format-server-message)
      (setq circe-serenity-original-format-server-message
            circe-format-server-message))
  (setq circe-format-server-message #'circe-serenity-server-message-formatter)

  (if (null circe-serenity-original-format-server-join-in-channel)
      (setq circe-serenity-original-format-server-join-in-channel
            circe-format-server-join-in-channel))
  (setq circe-format-server-join-in-channel
        #'circe-serenity-server-join-in-channel-formatter)

  (if (null circe-serenity-original-format-server-join)
      (setq circe-serenity-original-format-server-join
            circe-format-server-join))
  (setq circe-format-server-join #'circe-serenity-server-join-formatter)

  (if (null circe-serenity-original-format-server-quit)
      (setq circe-serenity-original-format-server-quit
            circe-format-server-quit))
  (setq circe-format-server-quit #'circe-serenity-server-quit-formatter)

  (if (null circe-serenity-original-format-server-quit-channel)
      (setq circe-serenity-original-format-server-quit-channel
            circe-format-server-quit-channel))
  (setq circe-format-server-quit-channel
        #'circe-serenity-server-quit-channel-formatter)

  (if (null circe-serenity-original-format-server-part)
      (setq circe-serenity-original-format-server-part
            circe-format-server-part))
  (setq circe-format-server-part #'circe-serenity-server-part-formatter)

  (if (null circe-serenity-original-format-server-nick-change)
      (setq circe-serenity-original-format-server-nick-change
            circe-format-server-nick-change))
  (setq circe-format-server-nick-change
        #'circe-serenity-server-nick-change-formatter)

  (if (null circe-serenity-original-format-server-topic)
      (setq circe-serenity-original-format-server-topic
            circe-format-server-topic))
  (setq circe-format-server-topic
        #'circe-serenity-server-topic-formatter))

(defun disable-circe-serenity ()
  (interactive)
  (setq circe-format-say circe-serenity-original-format-say
        circe-serenity-original-format-say nil
        circe-format-self-say circe-serenity-original-format-self-say
        circe-serenity-original-format-self-say nil
        circe-format-action circe-serenity-original-format-action
        circe-serenity-original-format-action nil
        circe-format-self-action circe-serenity-original-format-self-action
        circe-serenity-original-format-self-action nil
        circe-format-server-message circe-serenity-original-format-server-message
        circe-serenity-original-format-server-message nil
        circe-format-server-join-in-channel circe-serenity-original-format-server-join-in-channel
        circe-serenity-original-format-server-join-in-channel nil
        circe-format-server-join circe-serenity-original-format-server-join
        circe-serenity-original-format-server-join nil
        circe-format-server-quit circe-serenity-original-format-server-quit
        circe-serenity-original-format-server-quit nil
        circe-format-server-quit-channel circe-serenity-original-format-server-quit-channel
        circe-serenity-original-format-server-quit-channel nil
        circe-format-server-part circe-serenity-original-format-server-part
        circe-serenity-original-format-server-part nil
        circe-format-server-nick-change circe-serenity-original-format-server-nick-change
        circe-serenity-original-format-server-nick-change nil
        circe-format-server-topic circe-serenity-original-format-server-topic
        circe-serenity-original-format-topic nil))

(provide 'circe-serenity)
;;; circe-serenity.el ends here
