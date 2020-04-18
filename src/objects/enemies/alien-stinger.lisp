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
  (let ((body (make-circle-body (universe-of world)
                                10
                                :owner this
                                :mass 1)))
    (setf (body-position body) position)
    body))

;;; rendering
(defmethod render ((this alien-stinger) &key)
  (with-slots (body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (gk:x position) (gk:y position)))
    (let ((time (bodge-util:real-time-seconds)))
      (draw-animation 'alien-stinger-0-fly-0 time +zero-pos+))))
