(in-package :cl-rltut)

(defun use-item (item game-state log player &key args)
  (let ((item-comp (entity/item item))
        (results nil))
    (cond ((and (item/targeting item-comp)
                (not (getf args :target-x))
                (not (getf args :target-y)))
                (setf (game-state/targeting-item game-state) item)
                (setf (game-state/state game-state) :targeting)
                (setf (game-state/previous-state game-state) :player-turn)
           (add-message log (item/targeting-message item-comp))
           )
            (t
            (let ((use-result (funcall (item/use-function item-comp) item-comp player :args args)))
                (setf results use-result)
                (when (getf use-result :consumed)
                    (setf (game-state/state game-state) :enemy-turn)
                    (setf (inventory/items (entity/inventory player))
                            (remove-if #'(lambda (i)
                                            (eql i item))
                                       (inventory/items (entity/inventory player))))))))
    results))

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

(defun cast-fireball (item caster &key args)
  (declare (ignore item caster))
  (let* ((entities (getf args :entities))
         (target-x (getf args :target-x))
         (target-y (getf args :target-y))
         (map (getf args :map))
         (damage (getf args :damage))
         (radius (getf args :radius))
         (in-sight (tile/visible (aref (game-map/tiles map) target-x target-y)))
         (results nil))
    (cond (in-sight
            (dolist (entity entities)
                (let ((distance (distance-to-location entity target-x target-y)))
                (when (and (<= distance radius)
                            (entity/fighter entity))
                  (let ((damage-results (take-damage (entity/fighter entity) damage)))
                    (setf results (append
                                 results
                                 (list :consumed t :target entity :message (format nil "The fireball explodes, burning everything within ~A tiles" radius) :message-color (blt:orange))
                                 damage-results)))))))
          (t
           (setf results (list :consumed nil :target nil :message (format nil "You cannot target a tile outside your field of view") :message-color (blt:yellow)))))
    results))

(defun cast-confuse (item caster &key args)
  (declare (ignore item caster))
  (let* ((entities (getf args :entities))
         (target-x (getf args :target-x))
         (target-y (getf args :target-y))
         (map (getf args :map))
         (in-sight (tile/visible (aref (game-map/tiles map) target-x target-y)))
         (results nil))
    (cond (in-sight
           (dolist (entity entities)
             (when (and (= target-x (entity/x entity))
                        (= target-y (entity/y entity)))
               (let ((confused-ai (make-instance 'confused-monster
                                                 :previous-ai (entity/ai entity)
                                                 :num-turns 10
                                                 :owner entity)))
                 (setf (entity/ai entity) confused-ai)
                 (setf results (list :consumed t
                                     :target entity
                                     :message (format nil "The eyes of the ~A look vacant, as he starts to stumble around!" (entity/name entity))
                                     :message-color (blt:green)))))))
          (t
           (setf results (list :consumed nil
                               :target nil
                               :message (format nil "You cannot target a tile outside your field of view.")
                               :message-color (blt:yellow)))))
    results))
