(in-package :cl-rltut)

(defun heal (item target &key args)
  (declare (ignore item))
  (let ((amount (getf args :heal-amount)))
    (with-slots (hp max-hp) (entity/fighter target)
      (cond ((= hp max-hp)
             (list :consumed nil :message "You are already at full health." :message-color (blt:yellow)))
            (t
             (incf hp amount)
             (when (> hp max-hp)
               (setf hp max-hp))
             (list :consumed t :message "Your wounds start to feel better!" :message-color (blt:green)))))))

(defun cast-lightning (item target &key args)
  (declare (ignore item))
  (let* ((caster target)
         (entities (getf args :entities))
         (map (getf args :map))
         (damage (getf args :damage))
         (max-range (getf args :max-range))
         (results nil)
         (target nil)
         (closest-distance (1+ max-range)))
    (dolist (entity entities)
      (let ((in-sight (tile/visible (aref (game-map/tiles map) (entity/x entity) (entity/y entity))))
            (distance (distance-to caster entity)))
        (when (and in-sight
                   (not (eql entity caster))
                   (entity/fighter entity)
                   (< distance closest-distance))
          (setf target entity
                closest-distance distance))))

    (if target
        (setf results (append (list :consumed t :target target :message (format nil "A lightning bolt strikes the ~A with a load thunder! The damage is ~A" (entity/name target) damage))
                              (take-damage (entity/fighter target) damage)))
        (setf results (list :consumed nil :target nil :message (format nil "No enemy is close enough to strike.") :message-color (blt:red))))
    results))
