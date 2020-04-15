(cl:in-package :decent-game)

(define-animation alien-shooter-0-walk-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-walk-0.png")
  :frames 3)

(define-animation alien-shooter-0-attack-shoot-0
    (asset-path "img/alien-shooter-0/alien-shooter-0-attack-ranged-0.png")
  :frames 2)

(define-resource-pack alien-shooter-0-resources ()
  'alien-shooter-0-walk-0
  'alien-shooter-0-attack-shoot-0)


(defclass alien-shooter (alien movable)
  ()
  (:default-initargs
   :hp-max 15
   :strength 5
   :movement-speed 50)
  (:documentation "A small alien enemy, which shoots thorns."))


(defmethod initialize-instance :after ((this alien-shooter) &key world)
  (with-slots (body) this
    (setf body (make-box-body (universe-of world)
                              28
                              14
                              :owner this
                              :mass 1)
          (body-position body) (gk:vec2 120 20))))


(defmethod render ((this alien-shooter))
  (with-slots (states body) this
    (render body)
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
