(cl:in-package :decent-game)


(defparameter *alien-shooter-sense-field-radius* 80)
(defparameter *alien-shooter-shoot-radius* 60)
(defparameter *alien-shooter-close-radius* 30)
(defparameter *alien-shooter-movement-speed* 30)


(define-animation alien-shooter-0-walk-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-walk-0.png")
  :frames 3)

(define-animation alien-shooter-0-attack-shoot-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-attack-ranged-0.png")
  :frames 2)

(define-animation alien-shooter-0-attack-projectile-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-projectile-0.png")
  :frames 1)

(define-resource-pack alien-shooter-0-resources ()
  'alien-shooter-0-walk-0
  'alien-shooter-0-attack-shoot-0
  'alien-shooter-0-attack-projectile-0)


(defclass alien-shooter (alien ground-fighter)
  ()
  (:default-initargs
   :hp-max 15
   :strength 5
   :attack-speed 12 ; frames between attacks
   :movement-speed 50)
  (:documentation "A small alien enemy, which shoots thorns."))


(defmethod make-fighter-body ((this alien-shooter) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                5
                                :owner this
                                :mass 1)))
    (setf (body-position body) (or position (gk:vec2 120 20)))
    body))


(defmethod shoot ((shooter alien-shooter) (shot fighter))
  "Make the `shooter' shoot the `shot'."
  ;; TODO
  nil)


(defmethod observe ((this alien-shooter))
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
             (stop-moving this))
           (wander-around ()))
      (cond
        ((< len *alien-shooter-shoot-radius*) (shoot-player) (follow-player))
        ((< len *alien-shooter-sense-field-radius*) (follow-player))
        (t (wander-around))))))


;;; rendering
(defmethod render ((this alien-shooter) &key)
  (with-slots (body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 14) (- (gk:y position) 5)))
    (let ((time (bodge-util:real-time-seconds)))
      (draw-animation 'alien-shooter-0-walk-0 time +zero-pos+))))


(defmethod speed-of ((this alien-shooter))
  *alien-shooter-movement-speed*)
