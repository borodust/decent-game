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


(defclass alien-stinger (alien fighter)
  ()
  (:documentation "A medium sized alien enemy, which stings downwards."))


(defmethod observe ((this alien-stinger))
  (let* ((player-pos (position-of (player-of *world*)))
         (alien-pos (position-of this))
         (target-vec (b:subt alien-pos player-pos))
         (len (b:vector-length target-vec))
         (alien-vel (velocity-of this)))))


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
