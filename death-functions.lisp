(in-package #:cl-rltut)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))

  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (with-slots (char color blocks ai name render-order) monster
    (setf char #\%
          color (blt:red)
          blocks nil
          ai nil
          name (format nil "remains of ~A" name)
          render-order :corpse)
    (format nil "~A is dead!" name)))
