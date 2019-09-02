(in-package :cl-rltut)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))

(defmethod initialize-instance :after ((fighter fighter) &rest initargs)
  (declare (ignore initargs))
  (with-slots (hp max-hp) fighter
    (unless max-hp
      (setf max-hp hp))))

(defclass basic-monster (component) ())

(defgeneric take-turn (component target map astar-map entities))

(defmethod take-turn ((component basic-monster) target map astar-map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map astar-map entities))
            ((> (fighter/hp (entity/fighter monster)) 0)
             (format t "The ~A insults you! Your ego is damaged!~%" (entity/name monster)))))))
