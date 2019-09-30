(in-package #:cl-rltut)

(defclass game-state ()
  ((state :initarg :state :accessor game-state/state)
   (running :initarg :running :accessor game-state/running)))
