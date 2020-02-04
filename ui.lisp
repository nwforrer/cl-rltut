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

(defclass panel-component ()
  ((panel :initarg :panel :accessor panel-component/panel)
   (x :initarg :x :accessor panel-component/x)
   (y :initarg :y :accessor panel-component/y)))

(defclass bar (panel-component)
  ((name :initarg :name :accessor bar/name)
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
    (setf (panel/components panel) (append (panel/components panel) (list bar)))
    bar))

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

(defclass message-log (panel-component)
  ((messages :initarg :messages :accessor message-log/messages :initform nil)
   (width :initarg :width :accessor message-log/width)
   (height :initarg :height :accessor message-log/height)))

(defclass message ()
  ((text :initarg :text :accessor message/text)
   (color :initarg :color :accessor message/color)))

(defun make-message-log (panel x y width height)
  (let ((log (make-instance 'message-log :panel panel :x x :y y :width width :height height)))
    (setf (panel/components panel) (append (panel/components panel) (list log)))
    log))

(defun word-wrap (full-line width)
  (do ((lines nil)
       (line full-line))
      ((zerop (length line)) lines)
    (cond ((< (length line) width)
           (setf lines (append lines (list line))
                 line nil))
           (t
            (setf lines (append lines (list (subseq line 0 width)))
                  line (subseq line width))))))

(defgeneric add-message (log message &key color))
(defmethod add-message ((log message-log) message &key (color (blt:rgba 255 255 255)))
  (with-slots (messages width height) log
    (let ((wrapped-text (word-wrap message width)))
      (dolist (text wrapped-text)
        (setf messages (append messages (list (make-instance 'message :text text :color color))))
        (when (>= (length messages) (1- height))
          (setf messages (rest messages)))))))

(defmethod render ((log message-log))
  (let ((x (+ (panel-component/x log) (panel/x (panel-component/panel log))))
        (y (+ (panel-component/y log) (panel/y (panel-component/panel log)))))
    (dolist (message (message-log/messages log))
      (setf (blt:color) (message/color message))
      (blt:print x y (message/text message))
      (incf y))))
