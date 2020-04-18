(cl:in-package :decent-game)

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


(defclass alien-shooter (alien)
  ()
  (:default-initargs
   :hp-max 15
   :strength 5
   :attack-speed 12 ; frames between attacks
   :movement-speed 50)
  (:documentation "A small alien enemy, which shoots thorns."))


(defmethod make-fighter-body ((this alien-shooter) &key world position)
  (let ((body (make-box-body (universe-of world)
                             28
                             14
                             :owner this
                             :mass 1)))
    (setf (body-position body) (or position (gk:vec2 120 20)))
    body))


(defmethod shoot ((shooter alien-shooter) (shot fighter))
  "Make the `shooter' shoot the `shot'."
  ;; TODO
  nil)

;;; collision
(defmethod collide ((this alien-shooter) (that world))
  (with-slots (movement-speed) this
    (setf (collision-friction) 60)
    (setf (collision-surface-velocity) (gk:vec2 0 0)))
  t)

(defmethod collide ((that world) (this alien-shooter))
  (collide this that))


(defmethod process-collision ((this alien-shooter) (that world))
  (with-slots (body) this
    (setf (body-angular-velocity body) 0)))


(defmethod process-collision ((that world) (this alien-shooter))
  (process-collision this that))


;;; rendering
(defmethod render ((this alien-shooter) &key)
  (with-slots (body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (gk:x position) ;; (- (gk:x position) 8)
                           (gk:y position) ;; (- (gk:y position) 5)
                           ))
    (let ((time (bodge-util:real-time-seconds)))
      (draw-animation 'alien-shooter-0-walk-0 time +zero-pos+)
      ;; (cond ((idle-p this)
      ;;        (draw-animation 'player-idle time +zero-pos+))
      ;;       ((moving-left-p this)
      ;;        (gk:with-pushed-canvas ()
      ;;          (draw-animation 'player-walk time +zero-pos+ :mirror-x t)))
      ;;       ((moving-right-p this)
      ;;        (draw-animation 'player-walk time +zero-pos+)))
      )))
