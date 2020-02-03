;;;; cl-rltut.asd

(asdf:defsystem #:cl-rltut
  :description "Describe cl-rltut here"
  :author "Nick Forrer"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt #:queues.priority-queue)
  :components ((:file "package")
               (:file "util")
               (:file "math")
               (:file "game-map")
               (:file "ui")
               (:file "rendering")
               (:file "pathfinding")
               (:file "entity")
               (:file "components")
               (:file "fov")
               (:file "game-states")
               (:file "death-functions")
               (:file "cl-rltut")))
