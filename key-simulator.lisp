(in-package :stumpwm)

;;; Example usage with firefox. Place the binding in the car and the command in the cdr of the alist.
;;; Dont bind M-x to anything, as its bound to a closure that shows a menu of all simulation keys, 
;;; in case a keybinding is forgotten. 
(define-simulation-keymap firefox "Firefox"
  (("C-g" . "meta ESC")
   ("C-v" . "meta SunPageDown")
   ("M-v" . "meta SunPageUp")
   ("C-y" . "meta C-v")
   ("M-w" . "meta C-c")
   ("C-w" . "meta C-x")
   ("C-s" . "meta C-f")
   ("C-r" . "meta C-S-g")
   ("C-n" . "meta Down")
   ("C-p" . "meta Up")
   ("C-f" . "meta Right")
   ("C-b" . "meta Left")
   ("C-B" . "meta C-[")
   ("C-F" . "meta C-]")
   ("M-f" . "meta C-t")
   ("M-b" . "meta C-S-t")
   ("M-<" . "meta Home")
   ("M->" . "meta End")
   ("M-s" . "meta C-l")
   ("s-f" . "meta '")))


(defun sim-key-binder (kmap)
  (define-key *top-map* (kbd (caar kmap)) (cdar kmap))
  (when (rest kmap) (sim-key-binder (rest kmap))))

(defun sim-key-unbinder (kmap)
  (undefine-key *top-map* (kbd (caar kmap)))
  (when (rest kmap) (sim-key-unbinder (rest kmap))))

;; this macro defines a function which adds/removes keybindings to *top-map* 
;; this is destructive. if you have a key defined in top map and also defined 
;; in simulation keys, it will be unbound after leaving the simulation keys 
;; window. in order to preserve *top-map* bindings, youll need to hang a 
;; function that binds the keys in *top-map* on the *focus-window-hook* 
;; and run it LAST when the hook is run. 
(defmacro define-simulation-keymap-hook-function (fn-name class kmap)
  `(defun ,(intern (format nil "~{~a~}" `(simulation-keys- ,fn-name))) (cur-window last-window)
     ,(format nil "function to simulate keys for windows of the class ~S" class)
     (let ((cwin-p (when (equalp (window-class cur-window) ,class)
		     t))
	   (lwin-p (when last-window (when (equalp (window-class last-window) ,class)
				       t))))
       (cond
	 ((and cwin-p lwin-p)
	  t)
	 ((and cwin-p (not lwin-p))
	  (sim-key-binder 
	   (cons
	    '("M-x" .
	      ,(format nil "~{~a~}" 
		       `("eval (funcall " stumpwm \: \: *simulation-keys-m-x- ,fn-name * ")")))
	    ',kmap)))
	 ((and (not cwin-p) lwin-p)
	  (sim-key-unbinder 
	   (cons
	    '("M-x" .
	      ,(format nil "~{~a~}" 
		       `("eval (funcall " stumpwm \: \: *simulation-keys-m-x- ,fn-name * ")")))
	    ',kmap)))
	 (t
	  nil)))))

(defmacro define-simulation-keymap (fn-name class kmap)
  "defines a parameter and a function for simulating keys. then it hangs the function on
the *focus-window-hook*. "
  `(progn
     (define-simulation-keymap-parameter ,fn-name ,kmap)
     (define-simulation-keymap-hook-function ,fn-name ,class ,kmap)
     (remove-hook *focus-window-hook* ',(intern (format nil "~{~a~}" `(simulation-keys- ,fn-name))))
     (add-hook *focus-window-hook* ',(intern (format nil "~{~a~}" `(simulation-keys- ,fn-name))))))

(defmacro define-simulation-keymap-parameter (fn-name kmap)
  "generate a dynamic variable (a closure), which generates a menu of all rebound keys."
  `(defparameter ,(intern (format nil "~{~a~}" `(*simulation-keys-M-x- ,fn-name *)))
     (let ((menu-list 
	    ',(mapcar #'(lambda (single-kmap)
			  (list (concatenate 'string (car single-kmap) " ==> "
					     (if (equalp (subseq (cdr single-kmap) 0 4) "meta")
						 (subseq (cdr single-kmap) 5)
						 (cdr single-kmap)))
				(cdr single-kmap)))
		      kmap)))
       (lambda ()
	 (run-commands (cadr (select-from-menu (current-screen) menu-list)))))))

