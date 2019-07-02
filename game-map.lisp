(in-package #:cl-rltut)

(defclass tile ()
  ((blocked :initarg :blocked
            :accessor tile/blocked
            :initform nil)
   (block-sight :initarg :block-sight
                :accessor tile/block-sight
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

(defmethod make-map ((map game-map))
  (let ((room-1 (make-instance 'rect :x 20 :y 15 :w 10 :h 15))
        (room-2 (make-instance 'rect :x 35 :y 15 :w 10 :h 15)))
    (create-room map room-1)
    (create-room map room-2)
    (create-h-tunnel map 25 40 23)))

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
