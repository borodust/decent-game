(cl:in-package :decent-game)

(define-animation alien-stinger-0-fly-0
    (asset-path "img/alien-stinger-0/alien-stinger-0-fly-0.png")
  :frames 2)

(define-animation alien-stinger-0-attack-sting-0
    (asset-path "img/alien-stinger-0/alien-stinger-0-attack-sting-0.png")
  :frames 2)

(define-resource-pack alien-stinger-0-resources ()
  'alien-stinger-0-fly-0
  'alien-stinger-0-attack-sting-0)


(defclass alien-stinger (alien)
  ()
  (:default-initargs
   :hp-max 25
   :strength 10
   :movement-speed 40)
  (:documentation "A medium sized alien enemy, which stings downwards."))


(defmethod make-fighter-body ((this alien-stinger) &key world position)
  (let ((body (make-box-body (universe-of world)
                             32
                             32
                             :owner this
                             :mass 1)))
    (setf (body-position body) position)
    body))

;;; collision handling
(defmethod collide ((this alien-stinger) (that world))
  (with-slots (movement-speed) this
    (setf (collision-friction) 60)
    (setf (collision-surface-velocity) (gk:vec2 0 0)))
  t)


(defmethod collide ((that world) (this alien-stinger))
  (collide this that))


(defmethod process-collision ((this alien-stinger) (that world))
  (with-slots (body) this
    (setf (body-angular-velocity body) 0)))


(defmethod process-collision ((that world) (this alien-stinger))
  (process-collision this that))


;;; rendering
(defmethod render ((this alien-stinger) &key)
  (with-slots (body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (gk:x position) ;; (- (gk:x position) 8)
                           (gk:y position) ;; (- (gk:y position) 5)
                           ))
    (let ((time (bodge-util:real-time-seconds)))
      (draw-animation 'alien-stinger-0-fly-0 time +zero-pos+)
      ;; (cond ((idle-p this)
      ;;        (draw-animation 'player-idle time +zero-pos+))
      ;;       ((moving-left-p this)
      ;;        (gk:with-pushed-canvas ()
      ;;          (draw-animation 'player-walk time +zero-pos+ :mirror-x t)))
      ;;       ((moving-right-p this)
      ;;        (draw-animation 'player-walk time +zero-pos+)))
      )))
