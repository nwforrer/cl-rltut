(in-package #:cl-rltut)

(defparameter *render-order*
  '(:corpse 1
    :item 2
    :actor 3))

(defun render-order-compare (entity-1 entity-2)
  (< (getf *render-order* (entity/render-order entity-1))
     (getf *render-order* (entity/render-order entity-2))))

(defun get-names-under-mouse (x y entities map)
  (when (and (< y (game-map/h map))
             (< x (game-map/w map)))
    (let ((names nil)
          (in-fov (tile/visible (aref (game-map/tiles map) x y))))
      (when in-fov
        (dolist (entity entities)
          (when (and (= (entity/x entity) x)
                     (= (entity/y entity) y))
            (setf names (append names (list (entity/name entity)))))))
      (format nil "~{~A~^, ~}" names))))

(defun render-all (game-state player map stats-panel screen-width screen-height)
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
  (let ((entities (game-state/entities game-state)))
    (mapc #'(lambda (entity) (draw entity (game-map/tiles map)))
          (sort (copy-seq entities) #'render-order-compare))
    (setf (blt:background-color) (blt:black)
          (blt:color) (blt:white))
    (render-panel stats-panel)

    (let ((entity-names (get-names-under-mouse (blt:mouse-x) (blt:mouse-y) entities map)))
      (when entity-names
        (setf (blt:color) (blt:yellow))
        (blt:print (1+ (panel/x stats-panel)) (1+ (panel/y stats-panel)) entity-names))))

  (when (or (eql (game-state/state game-state) :show-inventory)
            (eql (game-state/state game-state) :drop-inventory))
    (let ((inventory-title (if (eql (game-state/state game-state) :show-inventory)
                               "Press key next to item to use it, or Esc to cancel."
                               "Press key next to item to drop it, or Esc to cancel.")))
      (inventory-menu inventory-title (entity/inventory player) 50 screen-width screen-height)))

  (blt:refresh))
