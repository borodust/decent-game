(cl:in-package :decent-game)


(defparameter *alien-shooter-sense-field-radius* 80)
(defparameter *alien-shooter-shoot-radius* 60)
(defparameter *alien-shooter-close-radius* 30)
(defparameter *alien-shooter-movement-speed* 30)
(defparameter *alien-shooter-shooting-delay* 0.8)


(define-animation alien-shooter-0-walk-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-walk-0.png")
  :frames 3)

(define-animation alien-shooter-0-attack-shoot-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-attack-ranged-0.png")
  :frames 2)

(define-animation alien-shooter-0-hurt
    (asset-path "img/alien-shooter-0/alien-shooter-0-hurt.png")
  :frames 1)

(define-animation alien-shooter-0-dying
    (asset-path "img/alien-shooter-0/alien-shooter-0-dying.png")
  :frames 4)

(define-animation alien-shooter-0-attack-projectile-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-projectile-0.png")
  :frames 1)

(define-resource-pack alien-shooter-0-resources ()
  'alien-shooter-0-walk-0
  'alien-shooter-0-attack-shoot-0
  'alien-shooter-0-hurt
  'alien-shooter-0-dying
  'alien-shooter-0-attack-projectile-0)


(defclass alien-shooter (alien ground-fighter)
  ((last-shot :initform nil))
  (:default-initargs
   :hp-max 2
   :strength 5
   :attack-speed 12                     ; frames between attacks
   :movement-speed 50)
  (:documentation "A small alien enemy, which shoots thorns."))


(defmethod render-bullet ((this enemy-bullet))
  (gk:draw-image +zero-pos+ 'alien-shooter-0-attack-projectile-0))


(defmethod provide-fighter-body ((this alien-shooter) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                5
                                :owner this
                                :mass 1)))
    (setf (body-position body) (or position (gk:vec2 120 20)))
    (attach-box-shape body 24 22 :sensor (make-instance 'enemy-hitbox :owner this)
                                 :radius 5
                                 :offset (gk:vec2 -12 -3))
    body))


(defmethod shoot ((this alien-shooter))
  (with-slots (last-direction last-shot) this
    (with-world (world)
      (unless (zerop last-direction)
        (let ((sign (/ last-direction (abs last-direction)))
              (pos (body-position (body-of this))))
          (spawn-bullet 'enemy-bullet world
                        (gk:add pos (gk:vec2 10 10))
                        (gk:vec2 (* sign 500) 0))
          (setf last-shot (bodge-util:real-time-seconds)))))))


(defun start-alien-shooting (shooter)
  (with-slots (last-shot) shooter
    (unless last-shot
      (setf last-shot 0))))


(defun stop-alien-shooting (shooter)
  (with-slots (last-shot) shooter
    (setf last-shot nil)))


(defmethod observe ((this alien-shooter))
  (with-slots (last-shot) this
    (let* ((player-pos (position-of (player-of *world*)))
           (alien-pos (position-of this))
           (target-vec (b:subt alien-pos player-pos))
           (len (b:vector-length target-vec)))
      (flet ((follow-player ()
               (cond
                 ((< len *alien-shooter-close-radius*) (stop-moving this))
                 ((> (gk:x target-vec) 0) (stop-moving this) (move-left this))
                 ((< (gk:x target-vec) 0) (stop-moving this) (move-right this))))
             (shoot-player ()
               (stop-moving this)
               (start-alien-shooting this))
             (wander-around ()
               (stop-alien-shooting this)
               (stop-moving this)))
        (cond
          ((< len *alien-shooter-shoot-radius*) (shoot-player) (follow-player))
          ((< len *alien-shooter-sense-field-radius*) (shoot-player))
          (t (wander-around)))))
    (when (and last-shot (> (- (bodge-util:real-time-seconds) last-shot)
                            *alien-shooter-shooting-delay*))
      (shoot this))))


;;; rendering
(defmethod render ((this alien-shooter) &key)
  (with-slots (body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 14) (- (gk:y position) 5)))
    (let ((time (bodge-util:real-time-seconds)))
      (cond ((facing-right-p this)
             (cond
               ((dying-p this) (draw-animation 'alien-shooter-0-dying time +zero-pos+ :mirror-x t))
               ((hurt-p this) (draw-animation 'alien-shooter-0-hurt time +zero-pos+ :mirror-x t))
               (t (draw-animation 'alien-shooter-0-walk-0 time +zero-pos+ :mirror-x t))))
            ((facing-left-p this)
             (cond
               ((dying-p this) (draw-animation 'alien-shooter-0-dying time +zero-pos+))
               ((hurt-p this) (draw-animation 'alien-shooter-0-hurt time +zero-pos+))
               (t (draw-animation 'alien-shooter-0-walk-0 time +zero-pos+))))
            (t (error "Alien-shooter should only be able to either face left or right.")))
      )))


(defmethod speed-of ((this alien-shooter))
  *alien-shooter-movement-speed*)
