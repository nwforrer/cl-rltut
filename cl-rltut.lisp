;;;; cl-rltut.lisp

(in-package #:cl-rltut)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defparameter *map-width* 80)
(defparameter *map-height* 43)

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)

(defparameter *max-enemies-per-room* 5)
(defparameter *max-items-per-room* 2)

(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
                                :light-wall (blt:rgba 130 110 50)
                                :light-ground (blt:rgba 200 180 50)))

(defvar *state* nil)

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
  (blt:set "input.filter = keyboard, mouse")
  (blt:set "window.title = Roguelike"))

(defun game-tick (player entities map game-state stats-panel log)
  (declare (type game-state game-state))
  (declare (type message-log log))
  (render-all entities player map stats-panel *screen-width* *screen-height*)
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
        (add-message log message))
      (when dead-entity
        (cond ((equal dead-entity player)
               (setf (values message (game-state/state game-state))
                     (kill-player dead-entity)))
              (t
               (setf message (kill-monster dead-entity))))
        (add-message log message :color (blt:orange))))

    (when (eql (game-state/state game-state) :enemy-turn)
      (dolist (entity (remove-if-not #'entity/ai entities))
        (let* ((enemy-turn-results (take-turn (entity/ai entity) player map entities))
               (message (getf enemy-turn-results :message))
               (dead-entity (getf enemy-turn-results :dead)))
          (when message
            (add-message log message))
          (when dead-entity
            (cond ((equal dead-entity player)
                   (setf (values message (game-state/state game-state))
                         (kill-player dead-entity)))
                  (t
                   (setf message (kill-monster dead-entity))))
            (add-message log message :color (blt:red))

            (when (eql (game-state/state game-state) :player-dead)
              (return-from game-tick game-state)))))
      (setf (game-state/state game-state) :player-turn)))

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
                                  :render-order :actor
                                  :fighter fighter-component))
           (entities (list player))
           (map (make-instance 'game-map :w *map-width* :h *map-height*))
           (stats-panel (make-panel 0 *map-height* *screen-width* (- *screen-height* *map-height*)))
           (message-log (make-message-log stats-panel 20 2 (- *screen-width* 20) (- *screen-height* *map-height* 1))))
      (make-bar "HP" stats-panel 1 2 15
                (fighter/hp fighter-component)
                (blt:rgba 0 128 0) (blt:rgba 100 100 100)
                :value-bind #'(lambda () (fighter/hp fighter-component))
                :max-bind #'(lambda () (fighter/max-hp fighter-component)))
      (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room* *max-items-per-room*)
      (fov map (entity/x player) (entity/y player))

      (add-message message-log "Welcome to the dungeon!")

      (do ((*state* (make-instance 'game-state :running t :state :player-turn) (game-tick player entities map *state* stats-panel message-log)))
          ((null (game-state/running *state*)))))))
