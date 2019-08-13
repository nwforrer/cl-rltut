;;;; cl-rltut.lisp

(in-package #:cl-rltut)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defparameter *map-width* 80)
(defparameter *map-height* 45)

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)

(defparameter *max-enemies-per-room* 5)

(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
                                :light-wall (blt:rgba 130 110 50)
                                :light-ground (blt:rgba 200 180 50)))

(deftype game-states () '(member :player-turn :enemy-turn :exit))

(defun render-all (entities map)
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
  (mapc #'(lambda (entity) (draw entity (game-map/tiles map))) entities)
  (setf (blt:background-color) (blt:black))
  (blt:refresh))

(defun handle-keys ()
  (blt:key-case (blt:read)
                ((or :up :k) (list :move (cons 0 -1)))
                ((or :down :j) (list :move (cons 0 1)))
                ((or :left :h) (list :move (cons -1 0)))
                ((or :right :l) (list :move (cons 1 0)))
                (:y (list :move (cons -1 -1)))
                (:u (list :move (cons 1 -1)))
                (:b (list :move (cons -1 1)))
                (:n (list :move (cons 1 1)))
                (:escape (list :quit t))
                (:close (list :quit t))))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Roguelike"))

(defun game-tick (player entities map game-state)
  (declare (type game-states game-state))
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))
    (when (and move (eql game-state :player-turn))
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at entities destination-x destination-y)))
            (cond (target
                   (format t "You kick the ~A.~%" (entity/name target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
          (setf game-state :enemy-turn))))
    (when exit
      (setf game-state :exit)))

  (when (eql game-state :enemy-turn)
    (dolist (entity entities)
      (if (entity/ai entity)
          (take-turn (entity/ai entity) player map entities)))
    (setf game-state :player-turn))

  game-state)

(defun main ()
  (blt:with-terminal
    (config)
    (let* ((fighter-component (make-instance 'fighter
                                             :hp 30
                                             :defense 2
                                             :power 5))
           (player (make-instance 'entity
                                  :name "Player"
                                  :x (/ *screen-width* 2)
                                  :y (/ *screen-height* 2)
                                  :char #\@
                                  :color (blt:white)
                                  :blocks t
                                  :fighter fighter-component))
           (entities (list player))
           (map (make-instance 'game-map :w *map-width* :h *map-height*)))
      (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
      (fov map (entity/x player) (entity/y player))

      (do ((game-state :player-turn (game-tick player entities map game-state)))
          ((eql game-state :exit))))))
