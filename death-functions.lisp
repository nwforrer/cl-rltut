(in-package #:cl-rltut)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:rgba 114 47 55))

  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (let ((death-message (format nil "~A is dead!" (entity/name monster))))
    (with-slots (char color blocks ai name) monster
      (setf char #\%
            color (blt:rgba 114 47 55)
            blocks nil
            ai nil
            name (format nil "remains of ~A" name)))
    death-message))
