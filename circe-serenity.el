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

(defvar circe-serenity--formatters-alist nil
  "Alist of which formatter to use for which circe format.")

(defun circe-serenity--define-formatter (formatter format)
  "Define that FORMATTER should be used for FORMAT."
  (setf (alist-get format circe-serenity--formatters-alist) formatter))

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
(circe-serenity--define-formatter 'circe-serenity-say-formatter 'circe-format-say)
(circe-serenity--define-formatter 'circe-serenity-say-formatter 'circe-format-self-say)

(defun circe-serenity-action-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} {body}" circe-serenity-longest-nick)
    (plist-put keywords :intro "*"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-action-formatter 'circe-format-action)

(defun circe-serenity-server-message-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {body}" circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-message-formatter 'circe-format-server-message)

(defun circe-serenity-server-join-in-channel-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} joined {channel}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro ">>>"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-join-in-channel-formatter
                    'circe-format-server-join-in-channel)

(defun circe-serenity-server-join-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} logged on"
            circe-serenity-longest-nick)
    (plist-put keywords :intro ">>>"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-join-formatter 'circe-format-server-join)

(defun circe-serenity-server-quit-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} logged off"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "<<<"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-quit-formatter 'circe-format-server-quit)

(defun circe-serenity-server-quit-channel-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} left {channel}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "<<<"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-quit-channel-formatter
                    'circe-format-server-quit-channel)

(defun circe-serenity-server-part-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} parted from {channel}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-part-formatter 'circe-format-server-part)

(defun circe-serenity-server-nick-change-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {old-nick} is now know as {new-nick}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-nick-change-formatter
                    'circe-format-server-nick-change)

(defun circe-serenity-server-topic-formatter (&rest keywords)
  (propertize
   (lui-format
    (format "{intro:%ds}   {nick} changed topic to: {new-topic}"
            circe-serenity-longest-nick)
    (plist-put keywords :intro "***"))
   'wrap-prefix (circe-serenity--fill-string)))
(circe-serenity--define-formatter 'circe-serenity-server-topic-formatter 'circe-format-server-topic)

;;;###autoload
(defun enable-circe-serenity ()
  (interactive)
  (dolist (format-pair circe-serenity--formatters-alist)
    (cl-destructuring-bind (format . formatter) format-pair
      (if (null (get format 'circe-serenity-original))
          (put format 'circe-serenity-original (symbol-value format)))
      (set format formatter))))

(defun disable-circe-serenity ()
  (interactive)
  (dolist (format-pair circe-serenity--formatters-alist)
    (cl-destructuring-bind (format . _) format-pair
      (set format (get format 'circe-serenity-original))
      (put format 'circe-serenity-original nil))))

(provide 'circe-serenity)
;;; circe-serenity.el ends here
