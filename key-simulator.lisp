;;; This file contains functionality for key simulation a la EXWM. 
;;; This works by defining bindings in *top-map*, and then unbinding them, 
;;; on a dynamic, window by window basis. 

(defmacro define-simulation-keymap (fn-name class kmap)
  `(defun ,(intern (format nil "~{~a~}" `(simulation-keys- ,fn-name))) (cur-window last-window)
     "add a function to simulate keys for windows of the class \,class "
     (let ((cwin-p (when (equalp (window-class cur-window) ,class)
		     t))
	   (lwin-p (when last-window (when (equalp (window-class last-window) ,class)
				       t))))
       (cond
	 ((and cwin-p lwin-p)
	  t)
	 ((and cwin-p (not lwin-p))
	  (sim-key-binder ,kmap))
	 ((and (not cwin-p) lwin-p)
	  (sim-key-unbinder ,kmap))
	 (t
	  nil)))))

(defun sim-key-binder (kmap)
  (define-key *top-map* (kbd (caar kmap)) (format nil "meta ~S" (cdar kmap)))
  (when (rest kmap) (sim-key-binder (rest kmap))))

(defun sim-key-unbinder (kmap)
  (undefine-key *top-map* (kbd (caar kmap)))
  (when (rest kmap) (sim-key-unbinder (rest kmap))))
    
;;;; example usage (all commands are run through meta, and must be valid keys):
(define-simulation-keymap firefox "Firefox" '(("C-v" . "SunPageDown") ("M-v" . "SunPageUp")))
(add-hook *focus-window-hook* 'simulation-keys-firefox)
