(cl:in-package :decent-game)


(defclass alien-stinger (alien movable)
  ()
  (:default-initargs
   :hp-max 25
   :strength 10
   :movement-speed 40)
  (:documentation "A medium sized alien enemy, which stings downwards."))
