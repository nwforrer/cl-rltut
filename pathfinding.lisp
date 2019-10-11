(in-package #:cl-rltut)

(defparameter *all-directions*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)
        (cons -1 -1)
        (cons -1 1)
        (cons 1 -1)
        (cons 1 1)))

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
   (distance-from-parent :initarg :distance-from-parent :accessor node/distance-from-parent)
   (parent :initarg :parent :initform nil :accessor node/parent)
   (position :initarg :position :initform nil :accessor node/position)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (position parent) obj
      (format stream "~A, parent ~A" position parent))))

(defun node-equal (n1 n2)
  (equal (node/position n1) (node/position n2)))

(defun node-compare (n1 n2)
  (< (node/f n1) (node/f n2)))

(defun find-in-queue (queue n)
  (let ((node nil))
    (queues:map-queue #'(lambda (item)
                          (when (node-equal n item)
                            (setf node item)))
                      queue)
    node))

(defun create-path (current-node)
  "Given a node, return a list of all parent nodes leading to it."
  (do ((path nil)
       (current current-node (node/parent current)))
      ((null current) (reverse path))
    (setf path (append path (list (node/position current))))))

(defun make-node (parent-node node-position direction-from-parent)
  (let ((distance 10))
    (if (and (not (zerop (car direction-from-parent)))
             (not (zerop (cdr direction-from-parent))))
        (setf distance 14))
    (make-instance 'node :parent parent-node :position node-position :distance-from-parent distance)))

(defun generate-node-cost (child current-node end-node)
  (with-slots (g h f position distance-from-parent) child
    (setf g (+ distance-from-parent (node/g current-node))
          h (+ (expt (- (car position) (car (node/position end-node))) 2)
               (expt (- (cdr position) (cdr (node/position end-node))) 2))
          f (+ g h))))

(defun update-open-queue (open-list child-node)
  (let ((existing-child (find-in-queue open-list child-node)))
    (cond ((and existing-child (< (node/g child-node) (node/g existing-child)))
           (queues:queue-change open-list
                                (queues:queue-find open-list existing-child)
                                child-node))
          (t
           (queues:qpush open-list child-node)))))

(defun generate-node-children (current-node map open-list closed-list end-node)
  (dolist (new-position *all-directions*)
    (let ((node-position (cons (+ (car (node/position current-node))
                                  (car new-position))
                               (+ (cdr (node/position current-node))
                                  (cdr new-position)))))
      (unless (or (> (car node-position) (1- (game-map/w map)))
                  (< (car node-position) 0)
                  (> (cdr node-position) (1- (game-map/h map)))
                  (< (cdr node-position) 0))
        (unless (tile/blocked (aref (game-map/tiles map) (car node-position) (cdr node-position)))
          (let ((child (make-node current-node node-position new-position)))
            ;; child is on the closed list
            (unless (find child closed-list :test 'node-equal)
              (generate-node-cost child current-node end-node)
              (update-open-queue open-list child))))))))

(defun astar (map start end)
  "Returns a list of cons cells as a path from the given start to the given end in the given map."
  (let ((start-node (make-instance 'node :position start))
        (end-node (make-instance 'node :position end))
        (open-list (queues:make-queue :priority-queue :compare #'node-compare))
        (closed-list nil))
    (queues:qpush open-list start-node)
    (do ((current-node (queues:qpop open-list) (queues:qpop open-list)))
        ((null current-node))
      (setf closed-list (append closed-list (list current-node)))

      ;; found the goal
      (when (node-equal current-node end-node)
        (return-from astar (create-path current-node)))

      (generate-node-children current-node map open-list closed-list end-node))))


;; (defparameter *maze* (list (list 0 0 0 0 1 0 0 0 0 0)
;;                            (list 0 0 0 0 1 0 0 0 0 0)
;;                            (list 0 0 0 0 0 0 0 0 0 0)
;;                            (list 0 0 0 0 1 0 0 0 0 0)))

;; (defparameter *maze* (list (list 0 0 0)
;;                            (list 0 1 0)
;;                            (list 0 0 0)
;;                            ))
