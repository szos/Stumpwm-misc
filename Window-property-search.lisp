;;; -*-lisp-*-

(in-package :stumpwm)

;;; this file contains a homebaked searching functionality for windows in stumpwm.
;;; stumpwm already lets you search for windows, but this will let you search for 
;;; all windows containing "some string" in the specified property (class, role, etc).
;;; this just takes changing a little code in window-match-properties-p from 
;;; window-placement.lisp. To preserve current functionality Ive tacked on the word fuzzy 
;;; to these functions, and threw them in a different file loaded by my init.lisp.

;;; Example Usage
(defcommand test-usage-pull/run-cmd (cmd pulley) ((:shell "exec: ")
						  (:rest "or pull: "))
  "This command takes a shell command and a substring to search for in the title 
of windows. If no windows match or the user quits at the window menu the shell 
command is run. Otherwise the selected window is pulled. Win is a window, you can 
do anything you want with it. raise it, pull it, delete it, etc."
  (if-let ((win (fuzzy-finder `((:title ,pulley)))))
    (pull-window win)
    (run-shell-command cmd)))

(defcommand find-then-do (str type) ((:string "Search term: ")
				     (:string "Property to search: "))
  "find a window, then choose to pull, raise or focus it. "
  (let ((win (fuzzy-finder `((,(if (char= (char type 0) #\:)
					(read-from-string type)
					(read-from-string (concatenate 'string ":" type)))
				    ,str)))))
    (eval (cadr (select-from-menu (current-screen)
				  `(("pull" (pull-window ,win))
				    ("raise" (raise-window ,win))
				    ("focus" (focus-all ,win))
				    ("delete" (delete-window ,win))))))))

(defcommand do-then-find (str type) ((:string "Search term: ")
				     (:string "Property to search: "))
  "choose what to do (raise or pull) then find the window."
  (let ((arg `((,(if (char= (char type 0) #\:) ;; check if the user provided a : or not. if not
		     (read-from-string type) ;; push one on to generate the correct symbols for the
		     (read-from-string (concatenate 'string ":" type))) ;; fuzzy finder
		 ,str))))
    (eval (cadr 
	   (select-from-menu 
	    (current-screen) 
	    `(("pull" (pull-window 
		       (fuzzy-finder ',arg)))
	      ("raise" (raise-window 
			(fuzzy-finder ',arg)))
	      ("focus" (focus-all
			(fuzzy-finder ',arg)))
	      ("delete" (delete-window 
			 (fuzzy-finder ',arg)))))))))

;;; Functions and definitions
(defun flatten-list (l)
  (if l
      (if (atom l)
	  (list l)
	  (mapcan #'custom-flatten l))))

(defun window-matches-properties-fuzzy (window &key class instance type role title)
  "Returns T if window matches any of the given properties"
  (and
   (if class (search class (window-class window)) t)
   (if instance (search instance (window-res window)) t)
   (if type (search type (window-type window)) t)
   (if role (search role (window-role window)) t)
   ;; (if role 
   ;;     (string-match (window-role window) role) t)
   (if title (search title (window-title window)) t)
   t))

(defun find-matching-windows-fuzzy (props all-groups all-screens)
  "Returns list of windows containing @var{props}. eg if its passed 'h' 
all windows containing h in the property are listed @var{all-groups} 
will find windows on all groups. Same for @{all-screens}. Result is sorted 
by group and window number, with group being more significant (think radix sort)."
  (let* ((screens (if all-screens
                      *screen-list*
                      (list (current-screen))))
         (winlist (if all-groups
                      (mapcan (lambda (s) (screen-windows s)) screens)
                      (group-windows (current-group))))
         (matches (remove-if-not (lambda (w)
                                   (apply 'window-matches-properties-fuzzy w props))
                                 winlist)))
    (stable-sort (sort matches #'< :key #'window-number)
                 #'< :key (lambda (w) (group-number (window-group w))))))


(defun fuzzy-finder (&optional 
		       (props '(:class "") props-supplied-p)
		       (fmt *window-format*)
		       (all-groups *run-or-raise-all-groups*)
		       (all-screens *run-or-raise-all-screens*))
  "returns a window chosen by the user after selection from a windowlist
derived from the properties sent in. if no properties are sent in it defaults
to collecting all windows"
  (if props-supplied-p
    (let ((matches (flatten-list
		    (loop for x in props
		       collect (find-matching-windows-fuzzy x all-groups all-screens)))))
      (select-window-from-menu matches fmt))
    (let ((matches (find-matching-windows-fuzzy props all-groups all-screens)))
      (select-window-from-menu matches fmt))))
