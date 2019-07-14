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

(defmethod in-bounds-p ((map game-map) (pos vec))
  (with-slots (width height) map
    (not (or (minusp (vec-x pos))
             (minusp (vec-y pos))
             (>= (vec-x pos) width)
             (>= (vec-y pos) height)))))

(defmacro map-tiles-loop ((map tile-val &key (row-val (gensym)) (col-val (gensym)) (x-start 0) (y-start 0) (x-end nil) (y-end nil)) &body body)
  `(loop :for ,col-val :from ,x-start :below (if (null ,x-end) (game-map/w ,map) ,x-end)
         :do
            (loop :for ,row-val :from ,y-start :below (if (null ,y-end) (game-map/h ,map) ,y-end)
                  :do
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

(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player)
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
        (if (null rooms)
            (setf rooms (list new-room))
            (push new-room (cdr (last rooms))))
        (incf num-rooms)))))

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
