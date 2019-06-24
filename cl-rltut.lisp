;;;; cl-rltut.lisp

(in-package #:cl-rltut)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defparameter *player-x* (round (/ *screen-width* 2)))
(defparameter *player-y* (round (/ *screen-height* 2)))

(defun draw ()
  (blt:clear)
  (setf (blt:color) (blt:rgba 1.0 1.0 1.0)
        (blt:cell-char *player-x* *player-y*) #\@)
  (blt:refresh))

(defun handle-keys ()
  (let ((action (make-hash-table)))
    (blt:key-case (blt:read)
                  (:up (setf (gethash :move action) (cons 0 -1)))
                  (:down (setf (gethash :move action) (cons 0 1)))
                  (:left (setf (gethash :move action) (cons -1 0)))
                  (:right (setf (gethash :move action) (cons 1 0)))
                  (:escape (setf (gethash :exit action) t))
                  (:close (setf (gethash :exit action) t)))
    action))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Roguelike"))

(defun main ()
  (blt:with-terminal
    (config)
    (loop :do
      (draw)
      (let* ((action (handle-keys))
             (move (gethash :move action))
             (exit (gethash :exit action)))
        (if exit
            (return))
        (when move
            (incf *player-x* (car move))
            (incf *player-y* (cdr move)))))))
