;;; qk-mode.el --- Let's take a break -*- lexical-binding: t; -*-

;; Copyright (C) 2023 IrohaCoding

;; Author: IrohaCoding <info@irohacoding.com>
;; Keywords: tools

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/irohacoding/qk-mode

;;; Commentary:

;; QK Mode is to take a break for hard working users.

;;; Code:

(defconst qk-mode-version "0.1.0"
  "QK Mode version.")

(defgroup qk-mode nil
  "Take a break for hard working users."
  :group 'tools
  :prefix "qk-"
  :link '(url-link :tag "Repository" "https://github.com/irohacoding/qk-mode"))

(defcustom qk-favorite-word "\"Time flies like an arrow; fruit flies like a banana.\" - Groucho Marx"
  "Display favorite word in *break* buffer.

Change your best favorite word."
  :type 'string
  :group 'qk-mode)

(defvar qk-mode-buffer "*break*"
  "Buffer name for qk-mode")

(define-derived-mode qk-mode text-mode "QK"
  "Major mode for taking a break.")

(defun qk ()
  "Take a break!"
  (interactive)
  (if (get-buffer qk-mode-buffer)
      (switch-to-buffer qk-mode-buffer)
    (switch-to-buffer (get-buffer-create qk-mode-buffer))
    (qk-mode)
    (insert qk-favorite-word)))

(provide 'qk-mode)

;;; qk-mode ends here.
