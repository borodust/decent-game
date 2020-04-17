(cl:in-package :decent-game)

(defclass fighter (stats)
  ((states :initform (list) :accessor states)
   (direction :initarg :direction
              :accessor direction
              :initform (error "The fighter needs a direction he's facing.")
              :documentation "-1 for left, 0 for both, +1 for right")
   (body :initform nil))
  (:documentation "Something that can fight,has a body and stats."))


(defmethod damage-for (amount (this fighter))
  "Damages the `hp' of `this' for `amount'. `This' can't fall below 0 hp."
  (with-slots (hp) this
    (setf hp (a:clamp amount hp amount))))

(defmethod heal-for (amount (this fighter))
  "Heals the `hp' of `this' for `amount'. `This' can't heal beyond `hp-max'."
  (with-slots (hp hp-max) this
    (incf hp (a:clamp amount (- hp-max hp) amount))))
