;;; notes.el --- Easy access to a directory notes file  -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Paul W. Rankin

;; Author: Paul W. Rankin <hello@paulwrankin.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defgroup notes ()
  "Minor mode for displaying a notes file."
  :group 'convenience)

(defcustom notes-hook
  nil
  "Hook run after showing notes buffer."
  :type 'hook)

(defcustom notes-file
  "notes.txt"
  "Name of the notes file to find.

This is always relative to `default-directory'. The idea is that
your project has its own directory and notes file, but if you
would like to use a file-specific notes file, specify a string
with `add-file-local-variable'."
  :type 'string
  :group 'notes)
(make-variable-buffer-local 'notes-file)

(defcustom notes-select-window
  t
  "If non-nil, switch to notes window upon displaying it."
  :type 'boolean
  :group 'notes)

(defcustom notes-display-alist
  '((inhibit-same-window . t)
    (side . right)
    (window-width . 35)
    (slot . 1))
  "Alist used to display notes buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :group 'notes)

(defvar-local notes-buffer-identify
  nil
  "Buffer local variable to identify a notes buffer.")

(defun notes-show-or-hide (&optional arg)
  "Pop up a window containing notes of current directory.
If prefixed with ARG, create the `notes-file' if it does not exist."
  (interactive)
  (if notes-buffer-identify
      (bury-buffer)
    (let ((buf (find-file-noselect (expand-file-name notes-file default-directory))))
      (display-buffer-in-side-window buf notes-display-alist)
      (with-current-buffer buf
        (setq notes-buffer-identify t)
        (run-hooks 'notes-hook))
      (if notes-select-window
          (select-window (get-buffer-window buf (selected-frame))))
      (message "Showing `%s'; %s to hide" buf
               (key-description (where-is-internal this-command
                                                   overriding-local-map t))))))

(define-key global-map (kbd "M-s n") #'notes-show-or-hide)

(provide 'notes)
;;; notes.el ends here
