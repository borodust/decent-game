(cl:in-package :decent-game)

(defclass movable ()
  ((movement-speed :initarg :movement-speed
                   :accessor movement-speed
                   :documentation "movement speed in pixels per second."))
  (:default-initargs
   :movement-speed 100)
  (:documentation "Enables objects to move."))

;; (defmethod initialize-instance :after ((this stats) &key))
