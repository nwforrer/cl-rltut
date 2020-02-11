(in-package :cl-rltut)

(defun heal (item target)
  (let ((amount (getf (item/use-args item) :heal-amount)))
    (with-slots (hp max-hp) (entity/fighter target)
      (cond ((= hp max-hp)
             (list :consumed nil :message "You are already at full health." :message-color (blt:yellow)))
            (t
             (incf hp amount)
             (when (> hp max-hp)
               (setf hp max-hp))
             (list :consumed t :message "Your wounds start to feel better!" :message-color (blt:green)))))))
