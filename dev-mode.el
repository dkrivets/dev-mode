;;; dev-mode.el --- Options for window -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Version: 0.01
;; Keywords: dev-mode, languages

;;; Commentary:

;; Simple mode to editing/programming something

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

(defvar dev-mode-map nil "Keymap for dev-mode.")

(progn
  ;; Initialize
  (setq dev-mode-map (make-sparse-keymap))

  ;; Re-start/Re-open new project
  (define-key dev-mode-map (kbd "C-c C-n") 'dev-mode))

(eval-when-compile (require 'subr-x))

(defun shell-dir (dir)
  "Run shell with specific DIR or change DIR if shell exists."
  (if (eql nil (get-buffer "*shell*"))
      (progn
	;; If not exist
	(let ((default-directory dir)
	      (shell-wnd (selected-window))
	      (shell-buff nil))
	  (setq shell-buf (shell))
	  (if (not (eq (selected-window) shell-wnd))
	      (progn
		(previous-buffer)
		(other-window -1)
		(switch-to-buffer shell-buf)))))
    ;; If exist shell
    (comint-send-string (get-buffer-process (get-buffer "*shell*"))
			(format "%s %s\n" "cd" dir))))

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

(use-local-map dev-mode-map)

(provide 'dev-mode)
