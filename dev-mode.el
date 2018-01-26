;;; dev-mode.el --- Options for window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup dev-mode nil "Default group for dev-mode." :group 'application)

;;; Buffers
(defcustom dev/edit "*scratch*"
  "Default buffer for editing."
  :type 'string
  :group 'dev-mode)

;;; Dirs
(defcustom dev/default-dir "~"
  "Default directory."
  :type 'directory
  :group 'dev-mode)

;;; Dired width
(defcustom dev/dired-width 40
  "Default dired width."
  :type 'integer
  :group 'dev-mode)

;;; Shell height
(defcustom dev/shell-height 60
  "Default shell height."
  :type 'integer
  :group 'dev-mode)

(eval-when-compile (require 'subr-x))

(defun shell-dir (dir)
  "DIR."
  (let ((default-directory dir)
  	(shell-wnd (selected-window))
  	(shell-buff nil))
    (setq shell-buf (shell))
    (if (not (eq (selected-window) shell-wnd))
  	(progn
  	  (previous-buffer)
  	  (other-window -1)
 	  (switch-to-buffer shell-buf)))))

(defun dev-mode()
  "DEV-MODE main code"
  (interactive)
    (let ((check-dir (read-directory-name "Directory to start: ")))
    (let ((start-dir (if (eql (length check-dir) 0) dev/default-dir check-dir) ))
      (message start-dir)
      ;;; Delete all windows
      (delete-other-windows)

      ;;; Dired in left side
      (split-window-horizontally dev/dired-width)
      (dired start-dir)

      ;;; Central window
      (other-window -1)
      (split-window-vertically)
      (switch-to-buffer dev/edit)

      ;;; Shell which opened in project dir
      (other-window 1)
      ;;(shell start-dir)
      ;;(switch-to-buffer "*shell*")
      (shell-dir start-dir)
      (switch-to-buffer "*shell*")

      ;;; Balance area
      (balance-windows-area) )))

(provide 'dev-mode)
