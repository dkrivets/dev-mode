;;; dev-mode.el --- Options for window -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Created: 17 Dec 2017
;; Version: 0.01
;; Keywords: dev-mode, languages, programming
;; Homepage: https://github.com/dkrivets/dev-mode
;; Package-Requires: (subr-x comint)

;;; Commentary:

;; Simple mode to editing/programming something
;; 
;; TODO: Add Flycheck errors list under "*shell*".
;; TODO: Realize changing shell type through customize group.

;;; Code:

(defgroup dev-mode nil "Dev-mode customization." :group 'applications)

;;; Buffers
(defcustom dev/edit "*scratch*"
  "Default buffer for editing is '*scratch*'."
  :type 'string
  :group 'dev-mode)

;;; Dirs
(defcustom dev/default-dir "~"
  "Default directory is '~'."
  :type 'directory
  :group 'dev-mode)

;;; Dired width
(defcustom dev/dired-width 40
  "Default dired width is 40."
  :type 'integer
  :group 'dev-mode)

;;; Shell height
(defcustom dev/shell-height 60
  "Default shell height is 60."
  :type 'integer
  :group 'dev-mode)

(defvar dev/shell-name "*shell*" "Shell name(by default is *shell*).")

(defvar dev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'dev-mode)
    map)
  "Keymap for dev-mode.")

(eval-when-compile (require 'subr-x))
(require 'comint)

(defun shell-dir (dir)
  "Run shell with specific DIR or change DIR if shell exists."
  (if (eql nil (get-buffer dev/shell-name ))
      (progn
	;; If not exist
	(let ((default-directory dir)
	      (shell-wnd (selected-window))
	      (shell-buf nil))
	  (setq shell-buf (shell))
	  (if (not (eq (selected-window) shell-wnd))
	      (progn
		(previous-buffer)
		(other-window -1)
		(switch-to-buffer shell-buf))
	    (message "other way"))))
    ;; If exist shell
    (comint-send-string (get-buffer-process (get-buffer dev/shell-name))
			(format "%s %s\n" "cd" dir))))


(define-minor-mode dev-mode
  "DEV-MODE main code. The kernel of mode."
  :group 'dev-mode
  :require 'dev-mode
  :keymap dev-mode-map
  :global t
    (let ((check-dir (read-directory-name "Directory: ")))
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

      (shell-dir start-dir)
      (switch-to-buffer dev/shell-name)
      (adjust-window-trailing-edge (selected-window) (- dev/shell-height (window-width)) t)
      
      ;;; Balance area
      (balance-windows-area) )))

(provide 'dev-mode)

;;; dev-mode.el ends here
