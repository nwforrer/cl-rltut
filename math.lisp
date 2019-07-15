(in-package :cl-rltut)

(defun degree-to-radian (degree)
  (* degree (/ pi 180)))

(defun diagonal-distance (x0 y0 x1 y1)
  (let ((dx (- x0 x1))
        (dy (- y1 y0)))
    (max (abs dx) (abs dy))))

(defun lerp (start end time)
  (+ start (* time (- end start))))
