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
  ((state :initform :idle)
   (body :initform nil)))


(defmethod initialize-instance :after ((this player) &key world)
  (with-slots (body) this
    (setf body (make-circle-body (universe-of world) 5
                                 :owner this
                                 :mass 1)
          (body-position body) (gk:vec2 50 20))))


(defun make-player (world)
  (make-instance 'player :world world))


(defmethod dispose :after ((this player))
  (with-slots (body) this
   (dispose body)))


(defun move-player-right (player)
  (with-slots (state body) player
    (setf state :moving-right)))


(defun move-player-left (player)
  (with-slots (state body) player
    (setf state :moving-left)))


(defun stop-player (player)
  (with-slots (state body) player
    (setf state :idle)))


(defun jump-player (player)
  (with-slots (state body) player
    (case state
      (:moving-right (apply-force body (gk:mult (gk:vec2 -0.28734788 0.95782626) 12000)))
      (:moving-left (apply-force body (gk:mult (gk:vec2 0.28734788 0.95782626) 12000)))
      (:idle (apply-force body (gk:vec2 0 10000))))))


(defmethod render ((this player))
  (with-slots (state body) this
    (render body)
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 8) (- (gk:y position) 5)))
    (let ((time (bodge-util:real-time-seconds)))
      (case state
        (:idle (draw-animation 'player-idle time +zero-pos+))
        (:moving-right (draw-animation 'player-walk time +zero-pos+))
        (:moving-left (gk:with-pushed-canvas ()
                        (draw-animation 'player-walk time +zero-pos+ :mirror-x t)))))))


(defmethod collide ((this player) (that world))
  (with-slots (state) this
    (setf (collision-friction) 60)
    (case state
      (:idle (setf (collision-surface-velocity) (gk:vec2 0 0)))
      (:moving-right (setf (collision-surface-velocity) (gk:vec2 100 0)))
      (:moving-left (setf (collision-surface-velocity) (gk:vec2 -100 0)))))
  t)


(defmethod collide ((that world) (this player))
  (collide this that))


(defmethod process-collision ((this player) (that world))
  (with-slots (body) this
    (setf (body-angular-velocity body) 0)))


(defmethod process-collision ((that world) (this player))
  (process-collision this that))
