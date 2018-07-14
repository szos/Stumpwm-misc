(in-package :stumpwm)

;; from the stumpwm wiki:
;; test keypress display message:
(defun display-key-press-hook (key key-seq cmd)
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :bottom-right))
      (message "Key Sequence: ~A" (print-key-seq (reverse key-seq))))
    (when (stringp cmd)
      (sleep 0.5))))

;; useful for recording videos where folks need to see key presses. 
(defcommand toggle-key-readout () ()
  (if (member 'display-key-press-hook *key-press-hook*)
      (remove-hook  *key-press-hook* 'display-key-press-hook)
      (add-hook *key-press-hook* 'display-key-press-hook)))


;;; some closures i wrote for dealing with various system settings. replace the shell commands to whatever you want.
;; Keyboard layout
;; layout tracking variable
(defun keyboard-layout-setter ()
  (let ((layout-list '("us" "no")))
    (lambda (&optional (layout "us" layout-supplied-p))
      (cond (layout-supplied-p
	     (run-shell-command (format nil "setxkbmap ~S" layout)))
	    (t
	     (setf layout-list 
		   (let ((a (pop layout-list)))
		     (append layout-list (list a))))
	     (run-shell-command (format nil "setxkbmap ~S" (car layout-list)))
	     (message "Current layout: ~S" (car layout-list)))))))

(defparameter *layout* (keyboard-layout-setter))
(defcommand cycle-layout () ()
  (funcall *layout*))
;; set up bightness control
;;set up a closure based implementation:

(defun brightness ()
  (run-shell-command "xbacklight -inc 100")
  (let ((level 100))
    (lambda (inter)
      (cond ((= inter 1) ;;increase brightness
	     (progn
	       (unless (>= level 100) 
		 (run-shell-command "xbacklight -inc 10")
		 (setf level (+ level 10)))
	       (message "Brightness: ~D%" level)
	       (sleep .2)))
	    ((= inter -1) ;;decrease brightness
	     (unless (<= level 0)
	       (run-shell-command "xbacklight -dec 10")
	       (setf level (- level 10)))
	     (message "Brightness: ~D%" level)
	     (sleep .2))
	    ((= inter 0) ;; go to 1% brightness
	     (setf level 1)
	     (run-shell-command "xbacklight -dec 100")
	     (sleep .2)
	     (run-shell-command "xbacklight -inc 1")
	       )
	    ((= inter 2) ;; reset to 10% brightness
	     (setf level 10)
	     (run-shell-command "xbacklight -dec 100")
	     (sleep .2)
	     (run-shell-command "xbacklight -inc 10")
	     (message "Brightness: ~D%" level))
	    (t
	     (message "Brightness: ~D%" level))))))

(defparameter *brightness* (brightness))

(defcommand brightness (inc) ((:number "1, 0, -1, or 2: "))
  (funcall *brightness* inc))

(defcommand brightness-reset () ()
  (funcall *brightness* 2))

;; volume tracking variable and initialization
(defun volume-setter ()
  (run-shell-command "pactl set-sink-volume 0 0%")
  (let ((max 100)
	(tracker 0))
    (lambda (change &optional (nmax 100 nmax-supplied-p))
      (when nmax-supplied-p
	(setf max nmax))
      (cond ((> (+ tracker change) max)
	     (message "Max Volume.  ~D%" tracker))
	    ((< (+ tracker change) 0)
	     (message "Min Volume. ~D%" tracker))
	    (t
	     (setf tracker (+ tracker change))
	     (setf (subseq *volume-readout* 1 (+ (/ tracker 10) 1)) "=============")
	     (setf (subseq *volume-readout* (+ (/ tracker 10) 1) 12) "*------------")
	     (message "Volume: ~D%" tracker)
	     (run-shell-command (format nil "pactl set-sink-volume 0 +~D%" change)))))))
(defparameter *volume-readout* "[*----------]") ; for the modeline

;; volume readout: 
;; [==========*]
;; [===*-------]
;; [=====*-----]

(defparameter *volume* (volume-setter))

(defcommand volume (inc) ((:number "enter the increment"))
  (funcall *volume* inc))

;; tracks the status of the mode line to ensure stumptray is safely enabled and disabled. 
;; mode line is off --- 0
;; mode line is on ---- 1
(defun mode-liner ()
  (let ((mode-line-tracker 0))
    (lambda ()
      (cond ((= mode-line-tracker 0)
	     (mode-line)
	     (stumptray:stumptray)
	     (setf mode-line-tracker 1))
	    ((= mode-line-tracker 1)
	     (stumptray:stumptray)
	     (mode-line)
	     (setf mode-line-tracker 0))
	    (t
	     (message "there is something wrong with the closure from the function mode-liner"))))))

(defparameter *mode-line* (mode-liner))
(when *initializing* (funcall *mode-line*))

(defcommand modeline-stumptray-toggle () ()
  (funcall *mode-line*))
