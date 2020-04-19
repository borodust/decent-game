(cl:in-package :decent-game)

;;;
;;; HITBOX
;;;
(defclass hitbox ()
  ((owner :initarg :owner :initform nil :reader owner-of)))


(defmethod collide :around ((this hitbox) anything)
  (call-next-method)
  nil)

(defmethod collide :around (anything (this hitbox))
  (call-next-method)
  nil)


(defclass player-hitbox (hitbox) ())


(defclass enemy-hitbox (hitbox) ())
