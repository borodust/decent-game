(cl:in-package :decent-game)

;;;
;;; HITBOX
;;;
(defclass hitbox () ())


(defmethod collide :around ((this hitbox) anything)
  nil)

(defmethod collide :around (anything (this hitbox))
  nil)
