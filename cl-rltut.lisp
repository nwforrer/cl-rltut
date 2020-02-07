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

(defun handle-keys (game-state)
  (cond ((eql (game-state/state game-state) :player-turn)
         (handle-player-turn-keys))
        ((eql (game-state/state game-state) :player-dead)
         (handle-player-dead-keys))
        ((eql (game-state/state game-state) :show-inventory)
         (handle-inventory-keys))))

(defun handle-player-turn-keys ()
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
                  (:g (list :pickup t))
                  (:i (list :show-inventory t))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))

(defun handle-player-dead-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  (:i (list :show-inventory t))
                  (:escape (list :quit t)))))

(defun handle-inventory-keys ()
  (when (blt:has-input-p)
    (let ((key (blt:read))
          (char-key (blt:character-input)))
      (when char-key
        (let ((index (- (char-code char-key) (char-code #\a))))
          (when (>= index 0)
            (return-from handle-inventory-keys (list :inventory-index index)))))
      (blt:key-case key
                    (:escape (list :quit t))))))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard, mouse")
  (blt:set "window.title = Roguelike"))

(defun player-turn (game-state map player action)
  (let ((player-turn-results nil)
        (move (getf action :move))
        (pickup (getf action :pickup))
        (show-inventory (getf action :show-inventory)))
    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at (game-state/entities game-state) destination-x destination-y)))
            (cond (target
                   (setf player-turn-results (attack (entity/fighter player) target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
          (setf (game-state/state game-state) :enemy-turn))))

    (when pickup
      (dolist (entity (game-state/entities game-state))
        (when (and (entity/item entity)
                   (= (entity/x entity) (entity/x player))
                   (= (entity/y entity) (entity/y player)))
          (setf player-turn-results (add-item (entity/inventory player) entity)))))

    (when show-inventory
      (with-slots (previous-state state) game-state
        (setf previous-state state
              state :show-inventory)))

    (values player-turn-results game-state)))

(defun handle-player-results (game-state player player-turn-results log)
  (let ((message (getf player-turn-results :message))
        (dead-entity (getf player-turn-results :dead))
        (item-added (getf player-turn-results :item-added)))
    (when message
      (add-message log message))
    (when dead-entity
      (cond ((equal dead-entity player)
             (setf (values message (game-state/state game-state))
                   (kill-player dead-entity)))
            (t
             (setf message (kill-monster dead-entity))))
      (add-message log message :color (blt:orange)))
    (when item-added
      (setf (game-state/entities game-state) (remove-if
                                              #'(lambda (entity)
                                                  (and (eql (entity/name entity)
                                                            (entity/name item-added))
                                                       (= (entity/x entity) (entity/x item-added))
                                                       (= (entity/y entity) (entity/y item-added))))
                                              (game-state/entities game-state))
            (game-state/state game-state) :enemy-turn)))
  game-state)

(defun enemy-turn (game-state player map log)
  (dolist (entity (remove-if-not #'entity/ai (game-state/entities game-state)))
    (let* ((enemy-turn-results (take-turn (entity/ai entity) player map (game-state/entities game-state)))
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
        (add-message log message :color (blt:red)))))
  game-state)

(defun game-tick (player map game-state stats-panel log)
  (declare (type game-state game-state))
  (declare (type message-log log))
  (render-all game-state player map stats-panel *screen-width* *screen-height*)
  (let* ((player-turn-results nil)
         (action (handle-keys game-state))
         (inventory-index (getf action :inventory-index))
         (exit (getf action :quit)))

    (when (eql (game-state/state game-state) :player-turn)
      (setf (values player-turn-results game-state) (player-turn game-state map player action)))

    (when (and inventory-index
               (not (eql (game-state/previous-state game-state) :player-dead))
               (< inventory-index (length (inventory/items (entity/inventory player)))))
      (let ((item (nth inventory-index (inventory/items (entity/inventory player)))))
        (format t "item: ~A~%" item)))

    (when exit
      (if (eql (game-state/state game-state) :show-inventory)
          (setf (game-state/state game-state) (game-state/previous-state game-state))
          (setf (game-state/running game-state) nil)))

    (setf game-state (handle-player-results game-state player player-turn-results log))

    (when (eql (game-state/state game-state) :enemy-turn)
      (setf game-state (enemy-turn game-state player map log))
      (when (eql (game-state/state game-state) :player-dead)
          (return-from game-tick game-state))
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
           (inventory-component (make-instance 'inventory
                                               :capacity 26))
           (player (make-instance 'entity
                                  :name "Player"
                                  :x (/ *screen-width* 2)
                                  :y (/ *screen-height* 2)
                                  :char #\@
                                  :color (blt:white)
                                  :blocks t
                                  :render-order :actor
                                  :fighter fighter-component
                                  :inventory inventory-component))
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

      (do ((*state* (make-instance 'game-state :running t :state :player-turn :entities entities) (game-tick player map *state* stats-panel message-log)))
          ((null (game-state/running *state*)))))))
