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
  (decf (fighter/hp component) amount)
  (let ((results nil))
    (when (<= (fighter/hp component) 0)
      (setf results (list :dead (component/owner component))))
    results))

(defgeneric attack (component target))

(defmethod attack ((component fighter) (target entity))
  (let ((results nil)
        (damage (- (fighter/power component) (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (setf results (append (list :message
                                   (format nil "~A attacks ~A for ~A hit points.~%"
                                           (entity/name (component/owner component))
                                           (entity/name target)
                                           damage))
                             (take-damage (entity/fighter target) damage))))
      (t
       (setf results (list :message (format nil "~A attacks ~A but does no damage.~%"
                                            (entity/name (component/owner component))
                                            (entity/name target))))))))

(defclass basic-monster (component) ())

(defgeneric take-turn (component target map entities))

(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter monster)) 0)
             (setf results (attack (entity/fighter monster) target)))))
    results))

(defclass item (component)
  ((use-function :initarg :use-function :accessor item/use-function :initform nil)
   (use-args :initarg :use-args :accessor item/use-args :initform nil)
   (targeting :initarg :targeting :accessor item/targeting :initform nil)
   (targeting-message :initarg :targeting-message :accessor item/targeting-message :initform nil)))

(defclass inventory (component)
  ((capacity :initarg :capacity :accessor inventory/capacity)
   (items :initarg :items :accessor inventory/items :initform nil)))

(defgeneric add-item (inventory item))
(defmethod add-item ((inventory inventory) (item entity))
  (let ((results nil))
    (with-slots (items capacity) inventory
      (cond
        ((>= (length items) capacity)
         (setf results (list :item-added nil
                             :message "You cannot carry any more, your inventory is full")))
        (t
         (setf results (list :item-added item
                             :message (format nil "You pick up the ~A" (entity/name item))
                             :message-color (blt:yellow)))
         (setf items (append items (list item))))))
    results))

(defgeneric drop-item (inventory item))
(defmethod drop-item ((inventory inventory) (item entity))
  (let ((results nil))
    (with-slots (items) inventory
      (with-slots (x y) (component/owner inventory)
        (setf (entity/x item) x
              (entity/y item) y
              items (remove-if #'(lambda (i)
                                   (eql i item))
                               items)
              results (list :item-dropped item
                            :message (format nil "You dropped the ~A" (entity/name item))
                            :message-color (blt:yellow)))))))
