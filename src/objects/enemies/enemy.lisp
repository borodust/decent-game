(cl:in-package :decent-game)


(defclass enemy (stats)
  ((body :initform nil)))


(defun make-enemies (&key world enemy-type-list)
  (let ((enemies nil))
    (dolist (enemy-type enemy-type-list enemies)
      (push (make-instance enemy-type :world world) enemies))))


(defun render-enemies (enemy-list)
  (dolist (enemy enemy-list)
    (render enemy)))
