(in-package :cl-rltut)

(defmacro dorange ((val start end &key (step 1)) &body body)
  `(do ((,val ,start (+ ,val ,step)))
       ((>= ,val ,end))
     ,@body))
