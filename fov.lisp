(in-package :cl-rltut)

(defparameter *fov-distance* 5)

(defun reset-visibility (map)
  (map-tiles-loop (map tile)
    (setf (tile/visible tile) nil)))

(defun fov (map x y)
  (reset-visibility map)

  ;; loop aver 360 degrees
  (dotimes (degree 360)
    (let* ((rad (degree-to-radian degree))
           (nx (round (+ (* (cos rad) *fov-distance*) x)))
           (ny (round (+ (* (sin rad) *fov-distance*) y)))
           (d (diagonal-distance x y nx ny)))
      (dotimes (tile d)
        (let ((tx (round (lerp x nx (/ tile d))))
              (ty (round (lerp y ny (/ tile d)))))
          (if (or (< tx 0) (> tx (game-map/w map)))
              (return))
          (if (or (< ty 0) (> ty (game-map/h map)))
              (return))

          ;; if tile is a wall, mark as seen and stop the line early
          (when (tile/block-sight (aref (game-map/tiles map) tx ty))
            (setf (tile/visible (aref (game-map/tiles map) tx ty)) t
                  (tile/explored (aref (game-map/tiles map) tx ty)) t)
            (return))

          (setf (tile/visible (aref (game-map/tiles map) tx ty)) t
                (tile/explored (aref (game-map/tiles map) tx ty)) t))))))
