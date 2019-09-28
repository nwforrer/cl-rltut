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

(defgeneric take-damage (component amount))

(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount))

(defgeneric attack (component target))

(defmethod attack ((component fighter) (target entity))
  (let ((damage (- (fighter/power component) (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (take-damage (entity/fighter target) damage)
       (format t "~A attacks ~A for ~A hit points.~%"
               (entity/name (component/owner component))
               (entity/name target)
               damage))
      (t
       (format t "~A attacks ~A but does no damage.~%"
               (entity/name (component/owner component))
               (entity/name target))))))

(defclass basic-monster (component) ())

(defgeneric take-turn (component target map entities))

(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter monster)) 0)
             (attack (entity/fighter monster) target))))))
