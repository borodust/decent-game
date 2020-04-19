(cl:in-package :decent-game)


(defclass boss (enemy)
  ()
  (:default-initargs
   :hp-max 10
   :direction -1)
  (:documentation "The final boss."))

(defmethod make-fighter-body ((this boss) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                10
                                :owner this
                                :mass 2)))
    (setf (body-position body) (or position (gk:vec2 200 30)))
    body))

(defun make-boss ()
  (make-instance 'boss))
