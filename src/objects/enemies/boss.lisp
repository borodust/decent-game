(cl:in-package :decent-game)


(define-animation boss-idle (asset-path "img/boss/boss-idle.png") :frames 2)
(define-animation boss-attack (asset-path "img/boss/boss-attack.png") :frames 4)
(define-animation boss-hurt (asset-path "img/boss/boss-hurt.png") :frames 1)
(define-animation boss-dying (asset-path "img/boss/boss-dying.png") :frames 3)

(define-resource-pack boss-resources (boss-life-bar-resources)
  'boss-idle
  'boss-attack
  'boss-hurt
  'boss-dying)

(defclass boss (enemy)
  ()
  (:default-initargs
   :hp-max 10
   :direction -1)
  (:documentation "The final boss."))

(defmethod make-fighter-body ((this boss) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                10
                                :owner this
                                :mass 2)))
    (setf (body-position body) (or position (gk:vec2 200 30)))
    body))

(defun make-boss ()
  (make-instance 'boss))
