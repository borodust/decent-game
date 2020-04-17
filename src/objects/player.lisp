(cl:in-package :decent-game)

(defparameter *player-movement-speed* 50)
(defparameter *player-jump-strength* 10000)


(define-animation player-idle-right (asset-path "img/player/player-idle-right.png") :frames 2)
(define-animation player-idle-left (asset-path "img/player/player-idle-left.png") :frames 2)
(define-animation player-run-right (asset-path "img/player/player-run-right.png") :frames 4)
(define-animation player-run-left (asset-path "img/player/player-run-left.png") :frames 4)
(define-animation player-slash-right (asset-path "img/player/player-slash-right.png") :frames 2)
(define-animation player-slash-left (asset-path "img/player/player-slash-left.png") :frames 2)
(define-animation player-shoot-right (asset-path "img/player/player-shoot-right.png") :frames 2)
(define-animation player-shoot-left (asset-path "img/player/player-shoot-left.png") :frames 2)
(define-animation player-hurt-right (asset-path "img/player/player-hurt-right.png") :frames 1)
(define-animation player-hurt-left (asset-path "img/player/player-hurt-left.png") :frames 1)
(define-animation player-jump-right (asset-path "img/player/player-jump-right.png") :frames 2)
(define-animation player-jump-left (asset-path "img/player/player-jump-left.png") :frames 2)
(define-animation player-jumping-right (asset-path "img/player/player-jumping-right.png") :frames 1)
(define-animation player-jumping-left (asset-path "img/player/player-jumping-left.png") :frames 1)
(define-animation player-falling-right (asset-path "img/player/player-falling-right.png") :frames 1)
(define-animation player-falling-left (asset-path "img/player/player-falling-left.png") :frames 1)
(define-animation player-dying-right (asset-path "img/player/player-dying-right.png") :frames 3)
(define-animation player-dying-left (asset-path "img/player/player-dying-left.png") :frames 3)
(define-animation player-projectile-0-right (asset-path "img/player/player-projectile-0-right.png") :frames 2)
(define-animation player-projectile-0-left (asset-path "img/player/player-projectile-0-left.png") :frames 2)

(define-resource-pack player-resources ()
  'player-idle-right
  'player-idle-left
  'player-run-right
  'player-run-left
  'player-slash-right
  'player-slash-left
  'player-shoot-right
  'player-shoot-left
  'player-hurt-right
  'player-hurt-left
  'player-jump-right
  'player-jump-left
  'player-jumping-right
  'player-jumping-left
  'player-falling-right
  'player-falling-left
  'player-dying-right
  'player-dying-left
  'player-projectile-0-right
  'player-projectile-0-left)


(defclass player (fighter)
  ((movement-speed :initarg :movement-speed)
   (jump-strength :initarg :jump-strength))
  (:default-initargs
   :direction 1
   :hp-max 100
   :strength 5
   :movement-speed 200
   :jump-strength 10000))


(defmethod initialize-instance :after ((this player) &key world movement-speed position)
  (with-slots (body) this
    (setf body (make-box-body (universe-of world)
                              12
                              28
                              :owner this
                              :mass 1)
          (body-position body) (or position (gk:vec2 50 20)))))


(defun make-player (world &key position)
  (make-instance 'player :world world
                         :movement-speed *player-movement-speed*
                         :jump-strength *player-jump-strength*
                         :position position))


(defmethod dispose :after ((this player))
  (with-slots (body) this
    (dispose body)))


(defun get-player ()
  "Returns player if we're inside a state, which has a player slot.
Returns NIL otherwise."
  (let ((curr-state (gk.fsm:current-state)))
    (if (slot-exists-p curr-state 'player )
        (player curr-state)
        nil)))


(defmethod add-state (state (this player))
  "Adds `state' to the players `states' list.
With the exception of :left and :right. Those are added to `facing'."
  (with-slots (states facing) this
    (if (keywordp state)
        (pushnew state states)
        (error "~&state must be a keyword. Got ~A~%" state))))

(defmethod remove-state (state (this player))
  "Removes `state' to the players `states' list."
  (with-slots (states) this
    (if (keywordp state)
        (setf states (delete state states))
        (error "~&state must be a keyword. Got ~A~%" state))))


(defmethod facing-right-p ((this player))
  (plusp (direction this)))

(defmethod facing-left-p ((this player))
  (minusp (direction this)))


(defmethod running-p ((this player))
  (with-slots (states) this
    (member :running states)))


(defmethod idle-p ((this player))
  (or (null (states this))
      (and (facing-left-p this)
           (facing-right-p this))))


(defmethod jumping-p ((this player))
  (member :jumping (states this)))

(defmethod falling-p ((this player))
  (member :falling (states this)))


(defmethod move-right ((this player))
  nil)

(defmethod stop-move-right ((this player))
  nil)

(defmethod move-left ((this player))
  nil)

(defmethod stop-move-left ((this player))
  nil)


(defmethod stop-running ((this player))
  (remove-state :running this))

(defun stop-player (player)
  (setf (states player) (list)))


(defun jump-player (player)
  (with-slots (states body jump-strength) player
    (let ((jx 0.18734788)
          (jy 0.95782626))
      (if (facing-right-p player)
          (cond ((running-p player)
                 (apply-force body (gk:mult (gk:normalize (gk:vec2 jx jy))
                                            jump-strength)))
                ((idle-p player)
                 (apply-force body (gk:vec2 0 10000))))
          ;; else (player is facing left)
          (cond ((running-p player)
                 (apply-force body (gk:mult (gk:normalize (gk:vec2 (- jx) jy))
                                            jump-strength)))
                ((idle-p player)
                 (apply-force body (gk:vec2 0 10000))))))))


(defmethod collide ((this player) (that obstacle))
  (with-slots (states movement-speed) this
    (setf (collision-friction) 60)
    (if (facing-right-p this)
        (cond ((running-p this)
               (setf (collision-surface-velocity) (gk:vec2 100 0)))
              ((idle-p this)
               (setf (collision-surface-velocity) (gk:vec2 0 0))))
        (cond ((running-p this)
               (setf (collision-surface-velocity) (gk:vec2 (- 100) 0)))
              ((idle-p this)
               (setf (collision-surface-velocity) (gk:vec2 0 0))))))
  t)


(defmethod collide ((that obstacle) (this player))
  (collide this that))


(defmethod process-collision ((this player) (that world))
  (with-slots (body) this
    (setf (body-angular-velocity body) 0)))


(defmethod process-collision ((that world) (this player))
  (process-collision this that))


(defmethod render ((this player))
  (with-slots (states body) this
    (render body)
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 6)
                           (gk:y position)))
    (let ((time (bodge-util:real-time-seconds)))
      (if (facing-right-p this)
          (cond ((idle-p this)
                 (draw-animation 'player-idle-right time +zero-pos+))
                ((running-p this)
                 (draw-animation 'player-run-right time +zero-pos+)))
          (cond ((idle-p this)
                 (draw-animation 'player-idle-left time +zero-pos+))
                ((running-p this)
                 (draw-animation 'player-run-left time +zero-pos+)))))))
