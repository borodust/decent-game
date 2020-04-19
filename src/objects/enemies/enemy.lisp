(cl:in-package :decent-game)


(defclass enemy () ())


(defun make-enemies (&key world enemy-type-list)
  (let ((enemies nil))
    (dolist (enemy-type enemy-type-list enemies)
      (push (make-instance enemy-type :world world) enemies))))


(defun render-enemies (enemy-list)
  (dolist (enemy enemy-list)
    (render enemy)))


(defun make-enemy (type world &key position)
  (make-instance type :world world :position position))
