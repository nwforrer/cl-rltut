(in-package :cl-rltut.ui)

(defparameter *bar-width* 20)
(defparameter *panel-height* 7)

(defclass panel ()
  ((x :initarg :x :accessor panel/x)
   (y :initarg :y :accessor panel/y)
   (width :initarg :width :accessor panel/width)
   (height :initarg :height :accessor panel/height)
   (components :initarg :components :accessor panel/components :initform nil)))

(defmethod print-object ((object panel) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y width height) object
      (format stream "(~A,~A) ~Ax~A" x y width height))))

(defun make-panel (x y width height)
  (make-instance 'panel
                 :x x :y y :width width :height height))

(defclass bar ()
  ((name :initarg :name :accessor bar/name)
   (panel :initarg :panel :accessor bar/panel)
   (x :initarg :x :accessor bar/x)
   (y :initarg :y :accessor bar/y)
   (total-width :initarg :total-width :accessor bar/total-width)
   (value :initarg :value :accessor bar/value)
   (value-bind :initarg :value-bind)
   (maximum :initarg :maximum :accessor bar/maximum)
   (max-bind :initarg :max-bind)
   (color :initarg :color :accessor bar/color)
   (bg-color :initarg :bg-color :accessor bar/bg-color)))

(defmethod print-object ((object bar) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name total-width value maximum) object
      (format stream "~A ~Aw ~A/~A" name total-width value maximum))))

(defun make-bar (name panel x y total-width value color bg-color &key (value-bind nil) (max-bind nil))
  (let ((bar (make-instance 'bar :name name :panel panel
                                 :x x :y y :total-width total-width
                                 :value value :maximum value
                                 :value-bind value-bind :max-bind max-bind
                                 :color color :bg-color bg-color)))
    (setf (panel/components panel) (append (panel/components panel) (list bar)))))

(defmethod render-panel ((panel panel))
  (with-slots (x y width height components) panel
      (blt:draw-box x y width height)
      (dolist (component components)
        (render component))))

(defgeneric render (component))
(defmethod render ((bar bar))
  (with-slots (name panel x y total-width value value-bind maximum max-bind color bg-color) bar
    (when value-bind
      (setf value (funcall value-bind)))
    (when max-bind
      (setf maximum (funcall max-bind)))
    (let ((x-pos (+ (panel/x panel) x))
          (y-pos (+ (panel/y panel) y))
          (fill-width (round (* (/ value maximum) total-width)))
          (content (format nil "~A: ~A/~A" name value maximum)))
      (blt:draw-box x-pos y-pos total-width 1 :background-color bg-color :border nil)
      (unless (zerop value)
        (setf fill-width (max 1 fill-width))
        (blt:draw-box x-pos y-pos fill-width 1 :background-color color :border nil))
      (setf (blt:color) (blt:rgba 255 255 255))
      (blt:draw-box x-pos (1- y-pos) total-width 2 :background-color nil :border nil
                                                   :contents content))))
