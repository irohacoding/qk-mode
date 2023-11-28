;;; qk-mode.el --- Let's take a break -*- lexical-binding: t; -*-

;; Copyright (C) 2023 IrohaCoding

;; Author: IrohaCoding <info@irohacoding.com>
;; Keywords: tools

;; Version: 0.2.0
;; Package-Requires: ((emacs "28.2"))
;; URL: https://github.com/irohacoding/qk-mode

;; This file is not part of GNU Emacs, but is distributed under
;; the same terms.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either verion 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; QK Mode is to take a break for hard working users.
;; Type M-x qk for start qk-mode. Type C-g to stop steam
;; and type C-g again to exit (kill *break* buffer).

;;; Code:

(defgroup qk-mode nil
  "Take a break for hard working users."
  :group 'applications
  :prefix "qk-mode-"
  :link '(url-link :tag "Repository" "https://github.com/irohacoding/qk-mode"))

(defvar qk-mode-buffer "*break*"
  "Buffer name for qk-mode.")

(defvar qk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'qk-mode-finish)
    map))

(defcustom qk-mode-max-steaming-count 8
  "Max steaming count."
  :type 'integer
  :group 'qk-mode)

(defcustom qk-mode-show-phrase t
  "Show favorite phrase after steaming."
  :type 'boolean
  :group 'qk-mode)

(defcustom qk-mode-favorite-phrase "less is more"
  "Customize favorite phrase."
  :type 'string
  :group 'qk-mode)

(define-derived-mode qk-mode text-mode "QK"
  "Major mode for taking a break."
  (setq cursor-type nil))

;;;###autoload
(defun qk ()
  "Take a break!"
  (interactive)
  (switch-to-buffer (get-buffer-create qk-mode-buffer))
  (qk-mode)
  (qk-mode--coffee-break))

(defun qk-mode--coffee-break ()
  "Display coffee cup and steam with animation."
  (let ((height (- (frame-height) 8))
        (cup-parts '(" ________\n"
                     "   |        |-_\n"
                     "    |        | ||\n"
                     "   |________|_-\n"
                     "\\______/")))
    (save-excursion
      (insert (make-string (/ height 2) ?\n))
      (dolist (cup cup-parts)
        (qk-mode--insert-center cup))
      (goto-line (- (line-number-at-pos) 5))
      (qk-mode--insert-steam))))

(defun qk-mode--insert-steam ()
  "Insert and repeat steam like animation and insert favorite
   phrase after steaming if `qk-mode-show-phrase' is t."
  (let ((count 0)
        (steam-parts '(" \\ | /"
                       " / / \\"
                       " ` \\"
                       " . /"
                       "   `,")))
    (while (< count qk-mode-max-steaming-count)
      (dolist (steam steam-parts)
        (sit-for 1)
        (qk-mode--insert-center steam)
        (goto-line (- (line-number-at-pos) 1)))
      (goto-line (+ (line-number-at-pos) 5))
      (if (and (eq (1+ count) qk-mode-max-steaming-count)
               qk-mode-show-phrase)
          (qk-mode--insert-favorite-phrase)
        (dotimes (i (length steam-parts))
          (sit-for 1)
          (delete-region (point) (line-end-position))
          (goto-line (- (line-number-at-pos) 1)))
        (goto-line (+ (line-number-at-pos) 5)))
      (setq count (1+ count)))))

(defun qk-mode--insert-favorite-phrase ()
  "Insert favorite phrase above coffee cup and steam."
  (let ((phrase qk-mode-favorite-phrase))
    (goto-line (- (line-number-at-pos) 6))
    (sit-for 1)
    (qk-mode--insert-center phrase)))

(defun qk-mode--insert-center (text)
  "Insert text centered in the current buffer."
  (let ((width (- (frame-width) 1)))
    (insert (make-string (/ (- width (length text)) 2) ?\s))
    (insert text)))

(defun qk-mode-finish ()
  "Kill *break* buffer for finish qk-mode."
  (interactive)
  (kill-buffer qk-mode-buffer))

(provide 'qk-mode)

;;; qk-mode.el ends here
