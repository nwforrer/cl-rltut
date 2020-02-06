(in-package #:cl-rltut)

(defclass game-state ()
  ((state :initarg :state :accessor game-state/state)
   (entities :initarg :entities :accessor game-state/entities)
   (running :initarg :running :accessor game-state/running)))
