(in-package :cl-rltut)

(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks)
   (fighter :initarg :fighter :accessor entity/fighter :initform nil)
   (ai :initarg :ai :accessor entity/ai :initform nil)))

(defmethod initialize-instance :after ((entity entity) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fighter ai) entity
    (when fighter
      (setf (component/owner fighter) entity))
    (when ai
      (setf (component/owner ai) entity))))

(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))

(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let* ((dx (- target-x x))
           (dy (- target-y y))
           (distance (sqrt (+ (expt dx 2) (expt dy 2)))))
      (setf dx (round (/ dx distance))
            dy (round (/ dy distance)))
      (unless (or (blocked-p map (+ x dx) (+ y dy))
                  (blocking-entity-at entities (+ x dx) (+ y dy)))
        (move e dx dy)))))

(defmethod distance-to ((e entity) (other entity))
  (let ((dx (- (entity/x other) (entity/x e)))
        (dy (- (entity/y other) (entity/y e))))
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defmethod draw ((e entity) tiles)
  (with-slots (x y char color) e
    (if (tile/visible (aref tiles x y))
        (setf
         (blt:background-color) (blt:cell-background-color x y)
         (blt:color) color
         (blt:cell-char x y) char))))
