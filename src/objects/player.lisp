(cl:in-package :decent-game)


(define-animation player-idle (asset-path "img/player/player-idle-0.png")
  :frames 2)


(define-animation player-walk (asset-path "img/player/player-walk-0.png")
  :frames 3)


(define-animation player-melee (asset-path "img/player/player-attack-melee-0.png")
  :frames 3)


(define-animation player-ranged (asset-path "img/player/player-attack-ranged-0.png")
  :frames 2)


(define-resource-pack player-resources ()
  'player-idle
  'player-walk
  'player-melee
  'player-ranged)


(defclass player ()
  ((state :initform :idle)))


(defun make-player ()
  (make-instance 'player))


(defun move-player-right (player)
  (with-slots (state) player
    (setf state :moving-right)))


(defun move-player-left (player)
  (with-slots (state) player
    (setf state :moving-left)))


(defun stop-player (player)
  (with-slots (state) player
    (setf state :idle)))


(defmethod render ((this player))
  (with-slots (state) this
    (let ((time (bodge-util:real-time-seconds)))
      (case state
        (:idle (draw-animation 'player-idle time *zero-pos*))
        (:moving-right (draw-animation 'player-walk time *zero-pos*))
        (:moving-left (gk:with-pushed-canvas ()
                        (draw-animation 'player-walk time *zero-pos* :mirror-x t)))))))
