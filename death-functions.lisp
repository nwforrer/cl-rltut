(in-package #:cl-rltut)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))

  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (with-slots (char color blocks ai name render-order fighter) monster
    (let ((message (format nil "~A is dead!~%" name)))
      (setf char #\%
            color (blt:red)
            blocks nil
            ai nil
            fighter nil
            name (format nil "remains of ~A" name)
            render-order :corpse)
      message)))
