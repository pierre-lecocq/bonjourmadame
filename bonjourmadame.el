;;; bonjourmadame.el --- Say "Hello ma'am!"

;; Time-stamp: <2015-05-25 15:07:58>
;; Copyright (C) 2015 Pierre Lecocq
;; Version: 0.1

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

;; Display or browse the image from bonjourmadame.fr
;; Updated every day at 10AM (on CEST timezone)

;;;; Changelog:

;; v0.1: first release

;;; Code:

(defvar bonjourmadame-cache-dir "~/.bonjourmadame"
  "The images cache directory.")

(defvar bonjourmadame-buffer-name "*Bonjour Madame*"
  "The buffer name used to display the image.")

(defvar bonjourmadame-url "http://bonjourmadame.fr"
  "The base URL of bonjourmadame.fr.")

(defvar bonjourmadame-refresh-hour 10
  "The hour bonjourmadame.fr is updated.")

(defvar bonjourmadame-regexp "<img\\(.\\)+src=\"\\(http://\\(.\\)+tumblr.com\\(.\\)+.png\\)+\"[^>]+>"
  "The regexp to match the wanted image URL.")

(defvar bonjourmadame--date ""
  "The date of the image.")

(defvar bonjourmadame--image-url ""
  "The internal image URL.")

(defun bonjourmadame--get-image-url ()
  "Get the image URL."
  (when (string= "" bonjourmadame--image-url)
    (with-current-buffer (url-retrieve-synchronously bonjourmadame-url)
      (goto-char (point-min))
      (re-search-forward bonjourmadame-regexp nil t)
      (setq bonjourmadame--image-url (match-string 2))
      (kill-buffer)))
  bonjourmadame--image-url)

(defun bonjourmadame--get-image-path ()
  "Get the local image path."
  (set-time-zone-rule "CEST")
  (setq reftime (float-time))
  (when (< (string-to-number (format-time-string "%H")) bonjourmadame-refresh-hour)
    (setq reftime (- reftime (* bonjourmadame-refresh-hour 60 60))))
  (setq bonjourmadame--date (format-time-string "%Y-%m-%d" reftime))
  (concat
   (file-name-as-directory bonjourmadame-cache-dir)
   (format "%s.png" bonjourmadame--date)))

(defun bonjourmadame--download-image ()
  "Download and store the image."
  (unless (file-accessible-directory-p bonjourmadame-cache-dir)
    (make-directory bonjourmadame-cache-dir t))
  (let ((path (bonjourmadame--get-image-path))
        (url (bonjourmadame--get-image-url)))
    (unless (file-exists-p path)
      (url-copy-file url path))))

(defun bonjourmadame--display-image ()
  "Display the image."
  (bonjourmadame--download-image)
  (let ((image (create-image (bonjourmadame--get-image-path))))
    (switch-to-buffer bonjourmadame-buffer-name)
    (erase-buffer)
    (insert-image image)
    (insert (format "\n\nDate: %s" bonjourmadame--date))))

;;;###autoload
(defun bonjourmadame-browse-to-image ()
  "Browse to the image."
  (interactive)
  (browse-url (bonjourmadame--get-image-url)))

;;;###autoload
(defun bonjourmadame-browse-to-site ()
  "Browse to the site."
  (interactive)
  (browse-url bonjourmadame-url))

;;;###autoload
(defun bonjourmadame ()
  "Say Hello ma'am!"
  (interactive)
  (bonjourmadame--display-image))

(provide 'bonjourmadame)

;;; bonjourmadame.el ends here
