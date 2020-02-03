(in-package #:cl-rltut)

(defparameter *render-order*
  '(:corpse 1
    :item 2
    :actor 3))

(defun render-order-compare (entity-1 entity-2)
  (< (getf *render-order* (entity/render-order entity-1))
     (getf *render-order* (entity/render-order entity-2))))

(defun render-all (entities player map stats-panel screen-width screen-height)
  (declare (ignore screen-width screen-height player))
  (blt:clear)
  (dotimes (y *map-height*)
    (dotimes (x *map-width*)
      (let* ((tile (aref (game-map/tiles map) x y))
             (wall (tile/block-sight tile))
             (visible (tile/visible tile))
             (explored (tile/explored tile)))
        (cond (visible
               (if wall
                   (setf (blt:background-color) (getf *color-map* :light-wall))
                   (setf (blt:background-color) (getf *color-map* :light-ground)))
               (setf (blt:cell-char x y) #\Space))
              (explored
               (if wall
                   (setf (blt:background-color) (getf *color-map* :dark-wall))
                   (setf (blt:background-color) (getf *color-map* :dark-ground)))
               (setf (blt:cell-char x y) #\Space))))))
  (mapc #'(lambda (entity) (draw entity (game-map/tiles map)))
        (sort entities #'render-order-compare))
  (setf (blt:background-color) (blt:black)
        (blt:color) (blt:white))
  (render-panel stats-panel)

  (blt:refresh))
