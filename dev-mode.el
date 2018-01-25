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

(defun dev-mode (dir)
  "Mode for people likes IDE.  DIR start dir."
  (interactive "DDirectory:")
  (kill-all-local-variables)
  (delete-other-windows)
  (let ((last-buffer (window-buffer)))
    ;; check default
    (if (and (eq 0 (length left-wnd-list)) (eq 0 (length center-wnd-list)))
	(default-wnd dir)
      (message "nothing"))
    ;; build left
    (message "build left")
    (message "left-wnd-list: %s" left-wnd-list)
    (setq counter 0)
    (dolist (item left-wnd-list)
      (setq counter (1+ counter))
      (let ((fx (car item))
    	    (name (car (cdr item))))
	(message "left %s" name)
	(if (> counter 1)
	    (create-wnd '(split-window-left dev/wnd-left-width) fx name)
	  (create-wnd '(split-window dev/wnd-left-width) fx name))))
    (message "build center")
    (setq counter 0)
    (dolist (item center-wnd-list)
      (setq counter (1+ counter))
      (let ((fx (car item))
	    (name (car (cdr item))))
	(message "center %s" name)
	(create-wnd '(split-window) fx name)))
    ;; if no buffers
    (if (and (eq 1 (length (window-list))) (eq last-buffer (window-buffer)))
	(switch-to-buffer (get-buffer-create dev/edit)))
    (if (and (< 1 (length (window-list))) (eq last-buffer (window-buffer (other-window 1))))
	(delete-window))
    )
  )
(provide 'dev-mode)
;;; dev-mode.el Ends here
;(window-buffer)
;(buffer-name)
;(setq left-wnd-list '())
;(setq center-wnd-list '())

;;(defun base (&rest wnds)
;;  "WNDS."
;;  (let  ((counter 0))
;;    (dolist (item wnds)
;;      (setq counter (1+ counter))
;;      (if (> 1 counter)
;;	  (split-window-vertically))
;;      (message "item: %s"item)
;;      (eval (car item))
;;      (other-window 1))))

(defun base (wnds)
  "WNDS."
  (let  ((counter 0))
    (mapc
     (lambda (item)
       (setq counter (1+ counter))
       (if (> 1 counter)
	   ;;(split-window-vertically))
       	   (split-window-below))
       (message "item: %s" item)
       (eval item)
       (other-window 1))
     wnds)))


(defun to-default-wnd()
  (select-window dev/wnd-default))

(defun left (&rest wnds)
  "WNDS.NAME."
  (split-window-right 40);;(dev/wnd-left-width))
  (let ((left-wnd (selected-window))
	(dev/wnd-default (previous-window)))
    (base wnds)))

(defun center (&rest wnds)
  "WNDS."
  (to-default-wnd)
  (base wnds))

(defun set-default-buffer ()
  "."
  (switch-to-buffer (get-buffer-create dev/edit)))

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

(defun defOptions(dir)
  "DIR."
  ;;(switch-to-buffer (get-buffer-create "*scratch*"))
  ;;(set-default-buffer)
  
  ;;(split-window-horizontally 40)
  ;;(dired "~")
  ;;(other-window 1)
  (left `(dired "~"))


  ;;(split-window-vertically)
  ;;(other-window 1)
  ;;(let ((default-directory "~")
  ;;	(shell-wnd (selected-window))
  ;;	(shell-buff nil))
  ;;  (setq shell-buf (shell))
  ;;  (if (not (eq (selected-window) shell-wnd))
  ;;	(progn
  ;;	  (previous-buffer)
  ;;	  (other-window -1)
  ;;	  (switch-to-buffer shell-buf))))
  ;;(split-window-vertically)
  ;;(other-window)
  ;;(center `(set-default-buffer) `(shell-dir "~"))
  )

 (defun test()
  ;;(kill-all-local-variables)
  (delete-other-windows)
  ;(set-default-wnd)
  (defOptions "~/Documents")
  (to-default-wnd))

(test)

(defun name-window ()
  (interactive)
  (let ((name (read-input "Name: ")))
    (setf (gethash name window-names) (selected-window))))

(defun del-window ()
  (interactive)
  (let ((name (read-input "Name: ")))
    (delete-window (gethash name window-names))))

(name-window)
 (gethash "aaa" window-names)

(message "%s" window-names)

