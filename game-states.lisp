(in-package #:cl-rltut)

(defclass game-state ()
  ((state :initarg :state :accessor game-state/state)
   (previous-state :initarg :previous-state :accessor game-state/previous-state :initform nil)
   (entities :initarg :entities :accessor game-state/entities)
   (running :initarg :running :accessor game-state/running)))

(defmethod initialize-instance :after ((game-state game-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (state previous-state) game-state
    (unless previous-state
      (setf previous-state state))))
