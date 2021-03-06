;;;; ratcontrol.lisp

(in-package #:ratcontrol)

;; "ratcontrol" goes here. Hacks and glory await!

;; set up this structure for the file: parameters, then functions, then commands, then keymaps.

(export '(ratcontrol-cut-down ratcontrol-cut-up ratcontrol-cut-left ratcontrol-cut-right
			      ratcontrol-initialize ratcontrol-help))

;; parameters for tracking the frame
(defvar *resolution* '(1920 1080))
(defparameter *x-min-max* '(0 1920))
(defparameter *y-min-max* '(0 1080))
;; (defparameter *iterative-jumps* '(10 70 200))

;; Functions
(defun multi-ratclick (int button)
  "clicks selected button multiple times. pass in number of additional clicks, eg passing 1 clicks twic"
  (ratclick button)
  (when (>= int 1) (multi-ratclick (- int 1) button)))

(defun binary-ratwarp-init ()
  "initializes x-min-max and y-min-max based on resolution"
  (setf (car *x-min-max*) 0)
  (setf (cadr *x-min-max*) (car *resolution*))
  (setf (cadr *y-min-max*) (cadr *resolution*))
  (setf (car *y-min-max*) 0)
  (ratwarp-center))

(defun ratwarp-center ()
  "warps to the center of the current selection."
  (let ((x (floor (+ (car *x-min-max*) (/ (- (cadr *x-min-max*) (car *x-min-max*)) 2))))
	(y (floor (+ (car *y-min-max*) (/ (- (cadr *y-min-max*) (car *y-min-max*)) 2)))))
    (ratwarp x y)))

(defun ratcon-cut-right ()
  (let ((xsize (- (cadr *x-min-max*) (car *x-min-max*))))
    (setf (car *x-min-max*) (+ (car *x-min-max*) (/ xsize 2)))))
(defun ratcon-cut-left ()
  (let ((xsize (- (cadr *x-min-max*) (car *x-min-max*))))
    (setf (cadr *x-min-max*) (- (cadr *x-min-max*) (/ xsize 2)))))
(defun ratcon-cut-up ()
  (let ((ysize (- (cadr *y-min-max*) (car *y-min-max*))))
    (setf (cadr *y-min-max*) (- (cadr *y-min-max*) (/ ysize 2)))))
(defun ratcon-cut-down ()
  (let ((ysize (- (cadr *y-min-max*) (car *y-min-max*))))
    (setf (car *y-min-max*) (+ (car *y-min-max*) (/ ysize 2)))))

;; Commands
(defcommand ratcontrol-cut-up () ()
  (ratcon-cut-up)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*)
	   (car *y-min-max*) (cadr *y-min-max*)))
(defcommand ratcontrol-cut-down () ()
  (ratcon-cut-down)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) 
	   (car *y-min-max*) (cadr *y-min-max*)))
(defcommand ratcontrol-cut-left () ()
  (ratcon-cut-left)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) 
	   (car *y-min-max*) (cadr *y-min-max*)))
(defcommand ratcontrol-cut-right () ()
  (ratcon-cut-right)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) 
	   (car *y-min-max*) (cadr *y-min-max*)))

(defcommand ratsnap (dir amnt) ((:direction "select a direction: ")
				(:number "Amount to snap by: "))
  "snaps move the pointer by amnt. the cuts arent influenced by snaps."
  (case dir
    (:right
     (ratrelwarp amnt 0))
    (:left
     (ratrelwarp (- 0 amnt) 0))
    (:up
     (ratrelwarp 0 (- 0 amnt)))
    (:down
     (ratrelwarp 0 amnt))
    (t
     (message "Invalid Direction"))))

(defcommand ratcontrol-initialize (resolution-x resolution-y)
    ((:number "Resolution x size: ")
     (:number "Resolution Y size: "))
  "use to set/calibrate the resolution. "
  (setf *resolution* (list resolution-x resolution-y)))

(defcommand ratcontrol-click (button) ((:number "enter button: "))
  "clicks and reinitializes. send in zero to just reinitialize."
  (unless (eq button 0) (ratclick button))
  (binary-ratwarp-init))

;; Keymap
(define-interactive-keymap ratcontrol (:on-enter #'binary-ratwarp-init
					      :on-exit #'banish)
  "a keymap for ratcontrol. designed to stay out of the way of
typing and allow one work with text editors that arent keyboard driven"
  ((kbd "C-p") "ratcontrol-cut-up")
  ((kbd "C-n") "ratcontrol-cut-down")
  ((kbd "C-f") "ratcontrol-cut-right")
  ((kbd "C-b") "ratcontrol-cut-left")
  ((kbd "C-c") "ratcontrol-click 1")
  ((kbd "M-c") "ratcontrol-click 2")
  ((kbd "M-p") "ratsnap up 50")
  ((kbd "M-n") "ratsnap down 50")
  ((kbd "M-f") "ratsnap right 50")
  ((kbd "M-b") "ratsnap left 50")
  ((kbd "C-C") "ratclick 1")
  ((kbd "M-C") "ratclick 2")
  ((kbd "C-M-c") "run-shell-command xte \"mouseclick 5\"")
  ((kbd "C-M-C") "run-shell-command xte \"mouseclick 4\"") ;; use 6 and 7 for left and right scrolling.
  ((kbd "C-q") "ratcontrol-click 0")
  ((kbd "C-r") "meta RET")
  ((kbd "C-h") "ratcontrol-help")
  ((kbd "M-h") "ratcontrol-help"))

(defcommand ratcontrol-help () ()
  (message "::::::::Ratcontrol Help::::::::
::::::::::Navigation:::::::::::
C-p                      Cut Up
C-n                    Cut Down
C-f                   Cut Right
C-b                    Cut Down
:::::::::::Clicking::::::::::::
C-c        Left Click and Reset
M-c       Right Click and Reset
C-C                  Left Click
M-C                 Right Click
:::::::::::::Misc::::::::::::::
C-q               Reset Pointer
C-r     Send RET Char to Window
C-h/M-h         Ratcontrol Help
:::::::::::Scrolling:::::::::::
C-M-c                 Scroll up
C-M-C               Scroll Down"))
