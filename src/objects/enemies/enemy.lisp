(cl:in-package :decent-game)


(defclass enemy () ())


(defun make-enemies (&key world enemy-type-list)
  (let ((enemies nil))
    (dolist (enemy-type enemy-type-list enemies)
      (push (make-instance enemy-type :world world) enemies))))


(defun render-enemies (enemy-list)
  (dolist (enemy enemy-list)
    (render enemy)))


(defun make-enemy (type world &key position)
  (make-instance type :world world :position position))


(defun register-enemy-damage (enemy)
  (shout "ENEMY DAMAGED"))


(defmethod collide :after ((this enemy-hitbox) (that player-bullet))
  (register-enemy-damage this)
  (destroy-bullet that))


(defmethod collide :after ((that player-bullet) (this enemy-hitbox))
  (register-enemy-damage this)
  (destroy-bullet that))
