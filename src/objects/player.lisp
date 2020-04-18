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
  ((last-direction :initarg :last-direction
                   :accessor last-direction
                   :documentation "Indicates which direction the player faced last. +1 = right, -1 = left"))
  (:default-initargs
   :direction 0
   :last-direction 1
   :hp-max 100
   :strength 5))


(defmethod initialize-instance :after ((this player) &key world position)
  (with-slots (body) this
    (setf body (make-box-body (universe-of world)
                              12
                              28
                              :owner this
                              :mass 1)
          (body-position body) (or position (gk:vec2 50 20)))))


(defun make-player (world &key position)
  (make-instance 'player :world world
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
        (a:deletef states state)
        (error "~&state must be a keyword. Got ~A~%" state))))


(defmethod facing-right-p ((this player))
  (plusp (last-direction this)))


(defmethod facing-left-p ((this player))
  (minusp (last-direction this)))


(defmethod idle-p ((this player))
  (null (states this)))


(defmethod running-p ((this player))
  (member :running (states this)))


(defmethod jumping-p ((this player))
  (member :jumping (states this)))


(defmethod falling-p ((this player))
  (member :falling (states this)))


(defun update-player-running-state (this new-direction)
  (with-slots (direction last-direction) this
    (incf direction new-direction)
    (if (= 0 direction)
        (remove-state :running this)
        (progn
          (add-state :running this)
          (setf last-direction direction)))))


(defmethod move-right ((this player))
  (update-player-running-state this 1))


(defmethod stop-move-right ((this player))
  (update-player-running-state this -1))


(defmethod move-left ((this player))
  (update-player-running-state this -1))


(defmethod stop-move-left ((this player))
  (update-player-running-state this 1))


(defmethod stop-running ((this player))
  (remove-state :running this))


(defun stop-player (player)
  (setf (states player) (list)))


(defun jump-player (player)
  (with-slots (states body) player
    (let ((jx 0.18734788)
          (jy 0.95782626))
      (cond ((idle-p player)
             (apply-force body (gk:vec2 0 10000)))
            (t (apply-force body (gk:mult (gk:normalize (gk:vec2 (* jx (direction player)) jy))
                                          *player-jump-strength*)))))))


(defmethod collide ((this player) (that obstacle))
  (with-slots (states direction) this
    (setf (collision-friction) 60
          (collision-surface-velocity) (cond
                                         ((> direction 0) (gk:vec2 -100 0))
                                         ((< direction 0) (gk:vec2 100 0))
                                         (t (gk:vec2 0 0)))))
  t)


(defmethod collide ((that obstacle) (this player))
  (collide this that))


(defmethod process-collision ((this player) (that obstacle))
  (with-slots (body) this
    (setf (body-angular-velocity body) 0)))


(defmethod process-collision ((that obstacle) (this player))
  (process-collision this that))


(defmethod render ((this player))
  (with-slots (states body) this
    (render body)
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 6)
                           (gk:y position)))
    (let ((time (bodge-util:real-time-seconds)))
      (cond ((facing-right-p this)
             (if (running-p this)
                 (draw-animation 'player-run-right time +zero-pos+)
                 (draw-animation 'player-idle-right time +zero-pos+)))
            ((facing-left-p this)
             (if (running-p this)
                 (draw-animation 'player-run-left time +zero-pos+)
                 (draw-animation 'player-idle-left time +zero-pos+)))
            (t (error "Play should only be able to either face left or right."))))))
