(cl:in-package :decent-game)

(defparameter *alien-stinger-movement-speed* 40)

(define-animation alien-stinger-0-fly-0
    (asset-path "img/alien-stinger-0/alien-stinger-0-fly-0.png")
  :frames 2)

(define-animation alien-stinger-0-attack-sting-0
    (asset-path "img/alien-stinger-0/alien-stinger-0-attack-sting-0.png")
  :frames 2)

(define-animation alien-stinger-0-hurt
    (asset-path "img/alien-stinger-0/alien-stinger-0-hurt.png")
  :frames 1)

(define-animation alien-stinger-0-dying
    (asset-path "img/alien-stinger-0/alien-stinger-0-dying.png")
  :frames 4)

(define-resource-pack alien-stinger-0-resources ()
  'alien-stinger-0-hurt
  'alien-stinger-0-dying
  'alien-stinger-0-fly-0
  'alien-stinger-0-attack-sting-0)


(defclass alien-stinger (alien fighter)
  ()
  (:default-initargs
   :hp-max 1)
  (:documentation "A medium sized alien enemy, which stings downwards."))


(defmethod observe ((this alien-stinger))
  (let* ((player-pos (position-of (player-of *world*)))
         (alien-pos (position-of this))
         (target-vec (b:subt player-pos alien-pos))
         (target-vel (gk:mult (gk:normalize target-vec) *alien-stinger-movement-speed*))
         (alien-vel (velocity-of this)))
    (apply-force (body-of this) (gk:div (gk:subt target-vel alien-vel) *observation-step*))))


(defmethod provide-fighter-body ((this alien-stinger) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                5
                                :owner this
                                :mass 1)))
    (setf (body-position body) position)
    (attach-box-shape body 16 32 :sensor (make-instance 'enemy-hitbox :owner this)
                                 :radius 5
                                 :offset (gk:vec2 -8 -5))
    body))

;;; rendering
(defmethod render ((this alien-stinger) &key)
  (with-slots (body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 15) (- (gk:y position) 5)))
    (let ((time (bodge-util:real-time-seconds)))
      (if (moving-right-p this)
          (cond ((dying-p this) (draw-animation 'alien-stinger-0-dying time +zero-pos+ :mirror-x t))
                ((hurt-p this) (draw-animation 'alien-stinger-0-hurt time +zero-pos+ :mirror-x t))
                (t (draw-animation 'alien-stinger-0-fly-0 time +zero-pos+ :mirror-x t)))
          (cond ((dying-p this) (draw-animation 'alien-stinger-0-dying time +zero-pos+))
                ((hurt-p this) (draw-animation 'alien-stinger-0-hurt time +zero-pos+))
                (t (draw-animation 'alien-stinger-0-fly-0 time +zero-pos+)))))))
