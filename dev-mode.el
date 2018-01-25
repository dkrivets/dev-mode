;;; dev-mode.el --- Options for window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup dev-mode nil "Default group for dev-mode." :group 'application)

;;; Buffers
(defcustom dev/edit "*scratch*"
  "Default buffer for editing."
  :type 'string
  :group 'dev-mode)

;;; Windows
(defcustom dev/wnd-left-width 40
  "Width of all left windows by default."
  :type 'integer
  :group 'dev-mode)

;;; Dirs
(defcustom dev/default-dir "~"
  "Default directory."
  :type 'string
  :group 'dev-mode)


;;;
(defvar left-wnd-list '() "List of left windows.")
(defvar center-wnd-list '() "List of windows in center.")
(defvar dev/wnd-default nil "Default window.")
(defvar dev/window-names (make-hash-table :test 'equal) "Hash of windows names.")


(eval-when-compile (require 'subr-x))

(defun set-window-name (name)
  "Set NAME to window."
  (interactive)
  ;;(setf (gethash name dev/window-names) (selected-window))
  (let ((wnd (selected-window)))
    (puthash name wnd dev/window-names)
  ))

(defun get-window-name (name)
  "Get window NAME."
  (interactive)
  (gethash name dev/window-names))

(defun get-names-list-by-window (name)
  "Get NAME by window."
  (interactive)
  (let ((list-keys (remove-duplicates  (hash-table-keys dev/window-names))))
    (mapcar
		(lambda (key)
		  (if (eql (gethash key dev/window-names) name)
		      key))
		list-keys)))

(defun get-name-by-window (wnd)
  "Get first name from result list founded by WND."
  (car (get-names-list-by-window wnd)))

(defun split-window-left (&optional size)
  "Split window left &optional SIZE.  Based on 'split-window-right'."
  ;; Default value of size
  (if (eql size nil)

      (setq size dev/wnd-left-width))
  ;; Split works
  (split-window-right size)
  (other-window 1))

(defun last-member-list (lst)
  "Get last list LST memeber."
  (car (reverse lst)))

(defun create-wnd (split fx &optional name)
  "SPLIT function to split window.  FX actions.  NAME bufefer name."
  ;; split window
  (eval split)
  (message (buffer-name))
  (other-window 1)
  (message (buffer-name))
  ;; Count windows and get new windows attrs
  (let ((count-wnd-before (length (window-list)))
	(current-wnd (selected-window))
	(dest-buff (window-buffer (selected-window))))
    ;; Complete actions
    (eval fx)
    ;; Check where is new actions. May be it in new window
    ;; Check window count if it changes do next
    (if (not (= (length (window-list)) count-wnd-before))
	(progn
	  (message "wnd don't eq")
	  ;; Find window creates by fx and get it buffer
	  (let ((unneeded-wnd (last-member-list (window-list)))
		(new-buff (window-buffer (last-member-list (window-list)))))
	    (message "zzz")
	    (message (buffer-name new-buff))
	    ;; Check windows cause fx may change focus and  it's true change focus
	    (if (not (eql current-wnd (selected-window)))
		(progn
		  (message "return back")
		  (other-window -1)))
	    ;; Check buffers and switch it
	    (if (not (string= (buffer-name dest-buff) (buffer-name new-buff)))
		(progn
		  (message "change buff")
		  (switch-to-buffer new-buff)))
	    (delete-window unneeded-wnd)
	    )))
    ;; Change buffer name
    (if (not (eql nil name))
	(rename-buffer name)))
  (message "aaa"))

(defun add-left (fx &optional name)
  "Add config data to lisr 'left-wnd-list'.  FX  NAME."
  (add-to-list 'left-wnd-list (list fx name)))

(defun add-center (fx &optional name)
  "Add config data to lisr 'center-wnd-list'.  FX  NAME."
  (add-to-list 'center-wnd-list (list fx name)))

(defun default-wnd (dir)
  "Default mode DIR."
  (add-left `(dired ,dir) (format "dired: %s" dir))
  (add-center '(switch-to-buffer (get-buffer-create dev/edit)) dev/edit)
  (add-center `(shell) (format "shell: %s" dir))
  (remove-duplicates left-wnd-list)
  (remove-duplicates center-wnd-list))


(defun set-default-wnd ()
  "."
  (set-window-name dev/edit)
  (setq dev/wnd-default (selected-window)))

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
  (interactive)
  (let ((check-dir (read-input "Directory to start: ")))
    (let ((start-dir (if (eql (length check-dir) 0) dev/default-dir check-dir) ))
      (message start-dir)
      ;;; Delete all windows
      (delete-other-windows)

      ;;; Dired in left side
      (split-window-horizontally)
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
      (balance-windows-area)
      )))

(provide 'dev-mode)
(dev-mode)
