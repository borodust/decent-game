(cl:in-package :decent-game)

(defclass stats ()
  ((hp :accessor hp)
   (hp-max :initarg :hp-max
           :accessor hp-max)
   (strength :initarg :strength
             :accessor strength)
   (attack-speed :initarg :attack-speed
                 :accessor attack-speed)
   (movement-speed :initarg :movement-speed
                   :accessor movement-speed
                   :documentation "Movement speed in pixels per second."))
  (:default-initargs
   :hp-max 10
   :strength 1
   :attack-speed 24
   :movement-speed 50)
  (:documentation "This class holds all of the characters stats."))

(defmethod initialize-instance :after ((this stats) &key)
  (setf (hp this) (hp-max this)))
