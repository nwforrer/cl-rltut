(in-package :cl-rltut)

(defun menu (header options width screen-width screen-height)
  (assert (< (length options) 26))

  (let* ((header-height 3)
         (height (+ (length options) header-height 1))
         (box-x (round (- (/ screen-width 2) (/ width 2))))
         (box-y (round (- (/ screen-height 2) (/ height 2)))))

    (blt:draw-box box-x box-y width height :contents header)

    (let ((y (+ header-height box-y))
          (letter-index (char-code #\a)))
      (dolist (option-text options)
        (let* ((text (concatenate 'string "(" (string (code-char letter-index)) ") " option-text)))
          (blt:print (1+ box-x) y text)
          (incf y)
          (incf letter-index))))))

(defun inventory-menu (header inventory inventory-width screen-width screen-height)
  (let ((options (if (zerop (length (inventory/items inventory)))
                     '("Inventory is empty.")
                     (mapcar #'(lambda (i)
                                 (entity/name i))
                             (inventory/items inventory)))))
    (menu header options inventory-width screen-width screen-height)))
