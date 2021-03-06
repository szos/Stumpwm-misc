;;; This file contains functionality for launching a shell command and doing something to it. 
;;; This was written to get around the pain of stump having no way to open a window and float it. 
;;; (well, there technically is functionality to do this, but i couldnt figure it out. pushing 
;;; functions to the main thread just wasnt really working, and I couldnt track down the author of 
;;; those specific commits). 
;;; So, Herin lies a macro called with-open-window, which takes a shell command, a function, and some
;;; arguments to pass to that function, alongside a restrict-class variable. If restrict-class is provided
;;; then the function isnt run unless the class of the window matches it. This is useful for applications that
;;; may take a while to start up, so that the function isnt run prematurely (the function controlling when 
;;; to run the provided function is hung on the *focus-window-hook*; if you switch windows before the window 
;;; opens, then the function will be run prematurely.

;;; A note on writing functions for this macro - the first argument MUST be reserved for the window.

;;; Here is some example usage:
;; (with-open-window "Xfce4-terminal -e alsamixer" nil #'reclassify-window "Alsamixer")
;; (defun reclassify-window (cwin new-class)
;;   (setf (window-class cwin) new-class))

;;; This will run alsamixer in a terminal, which would normally have the class "Xfce4-terminal",
;;; and reclassify it once it opens. 

;;(with-open-windows "cool-retro-term" nil #'float-window (current-group)
;;;This will float the window once its running. 

;;; Heres a practical example that will open emacs to an org buffer, reclassify it, and float it. 
;;; if this window already exists, raise it, unless its in a different group, in which case prompt 
;;; the user to choose to jump to it, or stay in the current group. 

(defcommand notes () ()
  (if-let ((win (fuzzy-finder '((:class "|FLOAT|Notes")) ;*window-format* t t
			      )))
    (if (eq (window-group win) (current-group))
	(raise win)
	(eval (second (select-from-menu (current-screen) `(("Jump to Notes" (raise ,win))
				    ("Stay Here" nil)))))
    (with-open-window "emacs ~/.stumpwm.d/notes.org" nil #'(lambda (cwin)
							     (float-in-tiles cwin "Notes")
							     ;;(toggle-always-on-top)
							     (meta (kbd "M->"))
							     ;;(window-send-string "C-x 1")
							     (meta (kbd "C-x"))
							     (meta (kbd "1"))))))

(defparameter *with-window*
;  "function, arguments, class restrictor."
  '(nil nil nil))

(defun with-open-window (cmd restrict-class function &rest args )
  (progn
     (setf (first *with-window*) function)
     (setf (second *with-window*) args)
     (setf (third *with-window*) restrict-class)
     (add-hook *focus-window-hook* 'with-window-hanger)
     (run-shell-command cmd)))

(defun with-window-hanger (cwin lwin)
  (declare (ignore lwin))
  ;; (when (atom (second *with-window*))
  ;;   (setf (second *with-window*) (list (second *with-window*))))
  (if (third *with-window*)
      (when (equal (window-class cwin)
		   (third *with-window*))
	(if (not (second *with-window*))
	    (funcall (first *with-window*) cwin)
	    (reduce (first *with-window*) (cons  cwin (second *with-window*))))
	(remove-hook *focus-window-hook* 'with-window-hanger))
      (progn
	(if (second *with-window*)
	    (reduce (first *with-window*) (cons cwin (second *with-window*)))
	    (funcall (first *with-window*) cwin))
	(remove-hook *focus-window-hook* 'with-window-hanger))))
