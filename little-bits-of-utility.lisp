(defmacro list-instance (props &optional (all-groups *run-or-raise-all-groups*)
				 (all-screens *run-or-raise-all-screens*)
				 (base 0))
  (when props
       `(let ((property-match (append (find-matching-windows ',(car props) ,all-groups ,all-screens)
				     (list-instance ,(rest props) ,all-groups ,all-screens 
						    ,(+ base 1)))))
	 (when (eq ,base 0) (windowlist "%n%s%c => %t" property-match))
	 property-match)))
   
;;; this is a macro for calling a windowlist of your specified window classes. eg if you run 
;;; multiple web browsers you might write:
;;; (list-instance ((:class "Firefox") (:class "Icecat") (:class "Opera") (:class "Chromium")))
;;; and get an interactive windowlist of all matching windows. 


(defcommand list-ttf-fonts () ()
  "list all fonts in xtf::*font-cache*"
  (let ((list-font-keys nil)
	(4-to nil)
	(formatter ""))
    (maphash #'(lambda (key value)
		 (setf list-font-keys (cons key list-font-keys))
		 ;;(format t "Font: ~S |||| ~S~%" key value)
		 )
	     xft::*font-cache*)
    (mapcar #'(lambda (value)
		(setf formatter 
		      (concatenate 'string formatter value ;'(#\Newline)
				   )))
	    list-font-keys)
    ;(setf formatter (mapcar (format t "Font: ~S~%")))
    (setf 4-to (4-to-a-row list-font-keys))
    ;; (setf 4-to (flatten 4-to))
    (message "~S" list-font-keys)))
    
(defun 4-to-a-row (list &optional (pre "Fonts: "))
  (unless (list) pre)
  (cond ((fourth list)
	 (let ((hold (concatenate 'string pre (first list) ", " (second list) ", "
				  (third list) ", " (fourth list) '(#\Newline))))
	   (pop list) (pop list) (pop list) (pop list)
	   ;; (concatenate 'string hold (4-to-a-row (list)))
	   hold))
	((third list)
	 (let ((hold (concatenate 'string pre (first list) ", " (second list) ", "
				  (third list) '(#\Newline))))
	   (pop list) (pop list) (pop list)
	   hold))
	((second list)
	 (let ((hold (concatenate 'string pre (first list) ", " (second list)
				  '(#\Newline))))
	   (pop list) (pop list)
	   hold))
	(t
	 (let ((hold (concatenate 'string pre (first list) ". ")))
	   (pop list)
	   hold))))
    
   ;;; a handy littl command for listing all the ttf fonts stumpwm can see with the font module, and formatting them to 
   ;;; four in a row. 

(defmacro conde (&body body)
  (if (not body)
      nil
      `(let ((return-list (cons  (if ,(car body)
				     ,(cadr body)) 
				 (conde ,@(rest (rest body))))))
	 return-list)))
;;; a macro that conds everything. think of it like a giant when statement. usage as follows:
;;; (conde 
;;;	(eq 11 11) (format t "hi")
;;;	(eq 54 43) (format t "bye"))

(defmacro conde-t (&body body)
  (when body
     (let ((reter (gensym)))
	`(let ((,reter (if ,(car body)
			   (cons ,(cadr body) (conde-t ,@(cddr body)))
			   (conde-t ,@(cddr body)))))
	    ,reter))))
;;; a macro just like conde but it only returns whatever evaluates as true. 
;;; for example usage:
;;; (conde-t 
;;;	(eq 4 4) (format t "hi~%")
;;;	(eq 4 5) (format t "Bye")
;;;	(eq 5 5) (+ 1 2 3)
;;;	t nil)
;;; would return 
;;; (nil 6 nil) 
;;; with the side effect of writing hi to the repl. 

;;; heres a little macro called if* which can act like if, or if with an implicit progn
(defmacro if* (test eval-t eval-f)
  `(if ,test
       ,(if (listp (car `,eval-t))
	    `(progn ,@eval-t)
	    `,eval-t)
       ,(if (listp (car `,eval-f))
	    `(progn ,@eval-f)
	    `,eval-f)))
;;; heres some example usage:
;;; (if* t
;;;      (format t "hi")
;;;      ((format t "nope")
;;;       (format t "bye")))
;;; would get expanded into 
;;; (if t
;;;     (format t "hi")
;;;     (progn (format t "nope") (format t "bye")))
