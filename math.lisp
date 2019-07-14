(in-package :cl-rltut)

(defstruct vec x y)

(defgeneric add (val1 val2))

(defmethod add ((val1 vec) (val2 vec))
  (make-vec :x (+ (vec-x val1) (vec-x val2))
            :y (+ (vec-y val1) (vec-y val2))))

(defmethod minus ((val1 vec) (val2 vec))
  (make-vec :x (- (vec-x val1) (vec-x val2))
            :y (- (vec-y val1) (vec-y val2))))

(defmethod multiply ((vec vec) (n number))
  (make-vec :x (* (vec-x vec) n)
            :y (* (vec-y vec) n)))

(defmethod magnitude ((vec vec))
  (sqrt (+ (expt (vec-x vec) 2)
           (expt (vec-y vec) 2))))

(defun degree-to-radian (degree)
  (* degree (/ pi 180)))

(defun diagonal-distance (x0 y0 x1 y1)
  (let ((dx (- x0 x1))
        (dy (- y1 y0)))
    (max (abs dx) (abs dy))))

(defun lerp (start end time)
  (+ start (* time (- end start))))
