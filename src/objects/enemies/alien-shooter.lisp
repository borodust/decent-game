(cl:in-package :decent-game)

(defclass alien-shooter (alien movable)
  ()
  (:default-initargs
   :hp-max 15
   :strength 5
   :movement-speed 50)
  (:documentation "A small alien enemy, which shoots thorns."))
