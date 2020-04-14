(cl:in-package :decent-game)

(defclass stats ()
  ((hp :accessor hp)
   (hp-max :initarg :hp-max
           :accessor hp-max)
   (strength :initarg :strength
             :accessor strength))
  (:default-initargs
   :hp-max 10
   :strength 1)
  (:documentation "This class holds all of the characters stats."))

(defmethod initialize-instance :after ((this stats) &key)
  (setf (hp this) (hp-max this)))

(defmethod damage-for (amount (this stats))
  "Damages the `hp' of `this' for `amount'. `This' can't fall below 0 hp."
  (with-slots (hp) this
    (setf hp (a:clamp amount hp amount))))

(defmethod heal-for (amount (this stats))
  "Heals the `hp' of `this' for `amount'. `This' heal beyond `hp-max'."
  (with-slots (hp hp-max) this
    (incf hp (a:clamp amount (- hp-max hp) amount))))
