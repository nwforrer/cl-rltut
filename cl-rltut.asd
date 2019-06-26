;;;; cl-rltut.asd

(asdf:defsystem #:cl-rltut
  :description "Describe cl-rltut here"
  :author "Nick Forrer"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "cl-rltut")
               (:file "game-map")))
