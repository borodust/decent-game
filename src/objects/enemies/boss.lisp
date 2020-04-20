(cl:in-package :decent-game)


(define-animation boss-idle (asset-path "img/boss/boss-idle.png") :frames 2)
(define-animation boss-attack (asset-path "img/boss/boss-attack.png") :frames 5)
(define-animation boss-hurt (asset-path "img/boss/boss-hurt.png") :frames 1)
(define-animation boss-dying (asset-path "img/boss/boss-dying.png") :frames 3)

(define-resource-pack boss-resources (boss-life-bar-resources)
  'boss-idle
  'boss-attack
  'boss-hurt
  'boss-dying)

(defclass boss (enemy ground-fighter)
  ()
  (:default-initargs
   :hp-max 10
   :direction -1)
  (:documentation "The final boss."))

(defmethod provide-fighter-body ((this boss) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                5
                                :owner this
                                :mass 1)))
    (setf (body-position body) (or position (gk:vec2 120 20)))
    (attach-box-shape body 24 22 :sensor (make-instance 'enemy-hitbox :owner this)
                                 :radius 5
                                 :offset (gk:vec2 -12 -3))
    body))

(defun make-boss ()
  (make-instance 'boss))


(defmethod render ((this boss) &key)
  (with-slots (states body) this
    (let ((position (body-position body)))
      (gk:with-pushed-canvas ()
       (gk:translate-canvas (- (gk:x position) 14) (- (gk:y position) 12))
       (let ((time (now)))
         (cond (t (draw-animation 'boss-idle time +zero-pos+))))))))
