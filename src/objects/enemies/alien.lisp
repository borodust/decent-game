(cl:in-package :decent-game)


(defclass alien (enemy)
  ()
  (:default-initargs
   :direction -1)
  (:documentation "Contains common properties of aliens."))
