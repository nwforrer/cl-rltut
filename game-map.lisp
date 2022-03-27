(in-package #:cl-rltut)

(defclass tile ()
  ((blocked :initarg :blocked
            :accessor tile/blocked
            :initform nil)
   (block-sight :initarg :block-sight
                :accessor tile/block-sight
                :initform nil)
   (visible :initarg :visible
            :accessor tile/visible
            :initform nil)
   (explored :initarg :explored
             :accessor tile/explored
             :initform nil)))

(defmethod initialize-instance :after ((tile tile) &rest initargs)
  (declare (ignore initargs))
  (with-slots (blocked block-sight) tile
    (if (null block-sight)
        (setf block-sight blocked))))

(defmethod set-tile-slots ((tile tile) &key (blocked nil blocked-supplied-p) (block-sight nil block-sight-supplied-p))
  (if blocked-supplied-p
      (setf (slot-value tile 'blocked) blocked))
  (if block-sight-supplied-p
      (setf (slot-value tile 'block-sight) block-sight)))

(defclass game-map ()
  ((width :initarg :w :accessor game-map/w)
   (height :initarg :h :accessor game-map/h)
   (tiles :accessor game-map/tiles)))

(defmacro map-tiles-loop ((map tile-val &key (row-val (gensym)) (col-val (gensym)) (x-start 0) (y-start 0) (x-end nil) (y-end nil)) &body body)
  `(dorange (,col-val ,x-start (if (null ,x-end) (game-map/w ,map) ,x-end))
     (dorange (,row-val ,y-start (if (null ,y-end) (game-map/h ,map) ,y-end))
       (let ((,tile-val (aref (game-map/tiles ,map) ,col-val ,row-val)))
                       (declare (ignorable ,tile-val))
         ,@body))))

(defmethod initialize-instance :after ((map game-map) &key (initial-blocked-value t))
  "Initializes the TILES slot-value to a new array with dimensions WxH.
Initializes each tile in the TILES array with BLOCKED set to INITIAL-BLOCKED-VALUE."
  (setf (game-map/tiles map) (make-array (list (game-map/w map) (game-map/h map))))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked initial-blocked-value))))

(defclass rect ()
  ((x1 :initarg :x1 :accessor rect/x1)
   (x2 :initarg :x2 :accessor rect/x2)
   (y1 :initarg :y1 :accessor rect/y1)
   (y2 :initarg :y2 :accessor rect/y2)))

(defmethod initialize-instance :after ((rect rect) &key x y w h)
  (with-slots (x1 x2 y1 y2) rect
    (setf x1 x
          y1 y
          x2 (+ x w)
          y2 (+ y h))))

(defmethod center ((rect rect))
  (with-slots (x1 x2 y1 y2) rect
    (let ((center-x (round (/ (+ x1 x2) 2)))
          (center-y (round (/ (+ y1 y2) 2))))
      (values center-x center-y))))

(defmethod intersect ((rect rect) (other rect))
  "Returns T if this RECT intersects with OTHER"
  (and (<= (rect/x1 rect) (rect/x2 other))
       (>= (rect/x2 rect) (rect/x1 other))
       (<= (rect/y1 rect) (rect/y2 other))
       (>= (rect/y2 rect) (rect/y1 other))))

(defgeneric make-map (map max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room max-items-per-room))
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room max-items-per-room)
  (do* ((rooms nil)
        (num-rooms 0)
        (room-index 0 (1+ room-index))
        (w (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (h (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (x (random (- map-width w))
           (random (- map-width w)))
        (y (random (- map-height h))
           (random (- map-height h)))
        (new-room (make-instance 'rect :x x :y y :w w :h h)
                  (make-instance 'rect :x x :y y :w w :h h))
        (can-place-p t t))
       ((>= room-index max-rooms))
    (dolist (other-room rooms)
      (if (intersect new-room other-room)
          (setf can-place-p nil)))
    (when can-place-p
      (create-room map new-room)
      (multiple-value-bind (new-x new-y) (center new-room)
        (if (zerop num-rooms)
            (setf (entity/x player) new-x
                  (entity/y player) new-y)
            (multiple-value-bind (prev-x prev-y) (center (car (last rooms)))
              (cond ((= (random 2) 1)
                     (create-h-tunnel map prev-x new-x prev-y)
                     (create-v-tunnel map prev-y new-y new-x))
                    (t
                     (create-v-tunnel map prev-y new-y prev-x)
                     (create-h-tunnel map prev-x new-x new-y)))))
        (place-entities map new-room entities max-enemies-per-room max-items-per-room)
        (if (null rooms)
            (setf rooms (list new-room))
            (push new-room (cdr (last rooms))))
        (incf num-rooms)))))

(defun entity-at (entities x y)
  (find-if #'(lambda (e)
               (and (= (entity/x e) x
                       (entity/y e) y)))
           entities))

(defun blocking-entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y)
             (entity/blocks entity))
        (return entity))))

(defun place-monsters (room entities num-monsters)
  (dotimes (monster-index num-monsters)
    (let ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
          (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room)))))
      (unless (entity-at entities x y)
        (cond ((< (random 100) 80)
               (let* ((fighter-component (make-instance 'fighter :hp 10 :defense 0 :power 3))
                      (ai-component (make-instance 'basic-monster))
                      (orc (make-instance 'entity :name "Orc" :x x :y y :color (blt:green) :char #\o :blocks t
                                                  :render-order :actor
                                                  :fighter fighter-component :ai ai-component)))
                 (nconc entities (list orc))))
              (t
               (let* ((fighter-component (make-instance 'fighter :hp 16 :defense 1 :power 4))
                      (ai-component (make-instance 'basic-monster))
                      (troll (make-instance 'entity :name "Troll" :x x :y y :color (blt:yellow) :char #\T :blocks t
                                                    :render-order :actor
                                                    :fighter fighter-component :ai ai-component)))
                 (nconc entities (list troll)))))))))

(defun place-items (room entities num-items)
  (dotimes (item-index num-items)
    (let* ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
           (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room))))
           (existing-entity (entity-at entities x y))
           (item-chance (random 85)))
      (unless existing-entity
        (cond ((< item-chance 70)

               (let* ((item-component (make-instance 'item :use-function #'heal :use-args '(:heal-amount 4)))
                      (potion (make-instance 'entity :name "Healing Potion" :x x :y y :color (blt:purple)
                                                     :item item-component
                                                     :char #\! :blocks nil :render-order :item)))
                 (nconc entities (list potion))))
              ((< item-chance 85)
               (let* ((item-component (make-instance 'item
                                                     :use-function #'cast-fireball
                                                     :use-args '(:damage 12 :radius 3)
                                                     :targeting t
                                                     :targeting-message "Left-click a target tile for the fireball, or right-click to cancel"))
                      (fireball (make-instance 'entity :name "Fireball Scroll" :x x :y y :color (blt:red)
                                                     :item item-component
                                                     :char #\# :blocks nil :render-order :item)))
                 (nconc entities (list fireball))))
              (t
               (let* ((item-component (make-instance 'item :use-function #'cast-lightning :use-args '(:damage 20 :max-range 5)))
                      (potion (make-instance 'entity :name "Lightning Scroll" :x x :y y :color (blt:yellow)
                                                     :item item-component
                                                     :char #\# :blocks nil :render-order :item)))
                 (nconc entities (list potion)))))))))

(defgeneric place-entities (map room entities max-enemies-per-room max-items-per-room))

(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room max-items-per-room)
  (let ((num-monsters (random max-enemies-per-room))
        (num-items (random (1+ max-items-per-room))))
    (place-monsters room entities num-monsters)
    (place-items room entities num-items)))

(defmethod create-room ((map game-map) (room rect))
  (map-tiles-loop (map tile
                   :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                   :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil)))

(defmethod create-h-tunnel ((map game-map) x1 x2 y)
  (let ((start-x (min x1 x2))
        (end-x (max x1 x2)))
    (map-tiles-loop (map tile
                     :x-start start-x :x-end (1+ end-x)
                     :y-start y :y-end (1+ y))
      (set-tile-slots tile :blocked nil :block-sight nil))))

(defmethod create-v-tunnel ((map game-map) y1 y2 x)
  (let ((start-y (min y1 y2))
        (end-y (max y1 y2)))
    (map-tiles-loop (map tile
                     :x-start x :x-end (1+ x)
                     :y-start start-y :y-end (1+ end-y))
      (set-tile-slots tile :blocked nil :block-sight nil))))

(defmethod blocked-p ((map game-map) x y)
  (tile/blocked (aref (game-map/tiles map) x y)))
