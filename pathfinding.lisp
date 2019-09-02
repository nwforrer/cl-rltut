(in-package #:cl-rltut)

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
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

(defun make-astar-map (map)
  (declare (type game-map map))
  (let ((astar-map nil))
    (dorange (y 0 (game-map/h map))
      (let ((row nil))
        (dorange (x 0 (game-map/w map))
          (let* ((tile-val (aref (game-map/tiles map) x y))
                 (blocked-val 0))
            (when (tile/blocked tile-val)
              (setf blocked-val 1))
            (setf row (append row (list blocked-val)))))
        (setf astar-map (append astar-map (list row)))))
    astar-map))

(defun astar (maze start end)
  "Returns a list of cons cells as a path from the given start to the given end in the given maze."
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
        (return-from astar
          (do ((path nil)
               (current current-node (node/parent current)))
              ((null current) (reverse path))
            (setf path (append path (list (node/position current)))))))

      ;; generate children
      (let ((children nil))
        (dolist (new-position (list (cons 0 -1)
                                    (cons 0 1)
                                    (cons -1 0)
                                    (cons 1 0)
                                    (cons -1 -1)
                                    (cons -1 1)
                                    (cons 1 -1)
                                    (cons 1 1)))
          (let ((node-position (cons (+ (car (node/position current-node))
                                        (car new-position))
                                     (+ (cdr (node/position current-node))
                                        (cdr new-position)))))
            (unless (or (> (car node-position) (1- (length (nth (1- (length maze)) maze))))
                        (< (car node-position) 0)
                        (> (cdr node-position) (1- (length maze)))
                        (< (cdr node-position) 0))
              (when (zerop (nth (car node-position) (nth (cdr node-position) maze)))
                (let ((new-child (make-instance 'node :parent current-node :position node-position))
                      (distance 10))
                  (if (and (not (zerop (car new-position)))
                           (not (zerop (cdr new-position))))
                      (setf distance 14))
                  (setf (node/g new-child) distance)
                  (setf children (append children (list new-child))))))))

        ;; loop through children
        (dolist (child children)
          ;; child is on the closed list
          (unless (find child closed-list :test 'node-equal)
            (with-slots (g h f position) child
              (setf g (+ g (node/g current-node))
                    h (+ (expt (- (car position) (car (node/position end-node))) 2)
                         (expt (- (cdr position) (cdr (node/position end-node))) 2))
                    f (+ g h)))
            (let ((existing-child (find-in-queue open-list child)))
              (cond ((and existing-child (< (node/g child) (node/g existing-child)))
                     (queues:queue-change open-list
                                          (queues:queue-find open-list existing-child)
                                          child))
                    (t
                     (queues:qpush open-list child))))))))))


;; (defparameter *maze* (list (list 0 0 0 0 1 0 0 0 0 0)
;;                            (list 0 0 0 0 1 0 0 0 0 0)
;;                            (list 0 0 0 0 0 0 0 0 0 0)
;;                            (list 0 0 0 0 1 0 0 0 0 0)))

;; (defparameter *maze* (list (list 0 0 0)
;;                            (list 0 1 0)
;;                            (list 0 0 0)
;;                            ))
