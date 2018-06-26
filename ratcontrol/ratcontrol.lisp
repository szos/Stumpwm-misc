;;;; ratcontrol.lisp

(in-package #:ratcontrol)

;;; "ratcontrol" goes here. Hacks and glory await!

;;; set up this structure for the file: parameters, then functions, then commands, then keymaps.

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
  (setf (car *x-min-max*) 0)
  (setf (cadr *x-min-max*) (car *resolution*))
  (setf (cadr *y-min-max*) (cadr *resolution*))
  (setf (car *y-min-max*) 0)
  (ratwarp-center))

(defun ratwarp-center ()
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
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) (car *y-min-max*) (cadr *y-min-max*)))
(defcommand ratcontrol-cut-down () ()
  (ratcon-cut-down)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) (car *y-min-max*) (cadr *y-min-max*)))
(defcommand ratcontrol-cut-left () ()
  (ratcon-cut-left)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) (car *y-min-max*) (cadr *y-min-max*)))
(defcommand ratcontrol-cut-right () ()
  (ratcon-cut-right)
  (ratwarp-center)
  (message "Resolution X: ~D ~D~%Resolution Y: ~D ~D" (car *x-min-max*) (cadr *x-min-max*) (car *y-min-max*) (cadr *y-min-max*)))


(defcommand ratcontrol-initialize (resolution-x resolution-y ;; jump-s jump-m jump-l
						)
    ((:number "Resolution x size: ")
     (:number "Resolution Y size: ")
     ;; (:number "Jump size small: ")
     ;; (:number "Jump size medium: ")
     ;; (:number "Jump size large: ")
     )
  (setf *resolution* (list resolution-x resolution-y))
  ;; (setf *iterative-jumps* (list jump-s jump-m jump))
  )

(defcommand ratcontrol-click (button) ((:number "enter button: "))
  (unless (eq button 0) (ratclick button))
  (binary-ratwarp-init))


;; Keymaps
(define-interactive-keymap ratcontrol-binary (:on-enter #'binary-ratwarp-init
					      :on-exit #'banish)
  ((kbd "C-p") "ratcontrol-cut-up")
  ((kbd "C-n") "ratcontrol-cut-down")
  ((kbd "C-f") "ratcontrol-cut-right")
  ((kbd "C-b") "ratcontrol-cut-left")
  ((kbd "C-c") "ratcontrol-click 1")
  ((kbd "M-c") "ratcontrol-click 2")
  ((kbd "C-C") "ratclick 1")
  ((kbd "M-C") "ratclick 2")
  ((kbd "C-M-c") "ratclick 4")
  ((kbd "C-M-C") "ratclick 5")
  ((kbd "C-q") "ratcontrol-click 0")
  ((kbd "C-r") "meta RET"))

(defcommand ratcontrol-help () ()
  (message "
::::::::Ratcontrol Help::::::::
::::::::::Navigation:::::::::::
C-p                      Cut Up
C-n                    Cut Down
C-f                   Cut Right
C-b                    Cut Down
:::::::::::::Misc::::::::::::::
C-q               Reset Pointer
C-r     Send RET Char to Window
:::::::::::Clicking::::::::::::
C-c        Left Click and Reset
M-c       Right Click and Reset
C-C                  Left Click
M-C                 Right Click
::::::::::Not Working::::::::::
C-M-c                Ratclick 4
C-M-C                Ratclick 5"))

