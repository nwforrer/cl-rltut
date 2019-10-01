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

(defvar *state* nil)

(defun render-all (entities player map screen-width screen-height)
  (declare (ignore screen-width))
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
  (setf (blt:background-color) (blt:black)
        (blt:color) (blt:white))
  (blt:print 1 (1- screen-height) (format nil "HP: ~2d/~2d"
                                          (fighter/hp (entity/fighter player))
                                          (fighter/max-hp (entity/fighter player))))

  (blt:refresh))

(defun handle-keys ()
  (when (blt:has-input-p)
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
                  (:close (list :quit t)))))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "output.vsync = true")
  (blt:set "window.title = Roguelike"))

(defun game-tick (player entities map game-state)
  (declare (type game-state game-state))
  (livesupport:update-repl-link)
  (livesupport:continuable
    (render-all entities player map *screen-width* *screen-height*)
    (let* ((player-turn-results nil)
           (action (handle-keys))
           (move (getf action :move))
           (exit (getf action :quit)))
      (when (and move (eql (game-state/state game-state) :player-turn))
        (let ((destination-x (+ (entity/x player) (car move)))
              (destination-y (+ (entity/y player) (cdr move))))
          (unless (blocked-p map destination-x destination-y)
            (let ((target (blocking-entity-at entities destination-x destination-y)))
              (cond (target
                     (setf player-turn-results (attack (entity/fighter player) target)))
                    (t
                     (move player (car move) (cdr move))
                     (fov map (entity/x player) (entity/y player)))))
            (setf (game-state/state game-state) :enemy-turn))))
      (when exit
        (setf (game-state/running game-state) nil))

      (let ((message (getf player-turn-results :message))
            (dead-entity (getf player-turn-results :dead)))
        (when message
          (format t message))
        (when dead-entity
          (cond ((equal dead-entity player)
                 (setf (values message (game-state/state game-state))
                       (kill-player dead-entity)))
                (t
                 (setf message (kill-monster dead-entity))))
          (format t message)))

      (when (eql (game-state/state game-state) :enemy-turn)
        (dolist (entity entities)
          (when (entity/ai entity)
            (let* ((enemy-turn-results (take-turn (entity/ai entity) player map entities))
                   (message (getf enemy-turn-results :message))
                   (dead-entity (getf enemy-turn-results :dead)))
              (when message
                (format t message))
              (when dead-entity
                (cond ((equal dead-entity player)
                       (setf (values message (game-state/state game-state))
                             (kill-player dead-entity)))
                      (t
                       (setf message (kill-monster dead-entity))))
                (format t message)

                (when (eql (game-state/state game-state) :player-dead)
                  (return-from game-tick game-state))))))
        (setf (game-state/state game-state) :player-turn))))

  game-state)

(defun stop ()
  (setf (game-state/running *state*) nil))

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

      (do ((*state* (make-instance 'game-state :running t :state :player-turn) (game-tick player entities map *state*)))
          ((null (game-state/running *state*)))))))
