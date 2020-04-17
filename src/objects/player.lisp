(cl:in-package :decent-game)


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
  ((direction :initarg :direction)
   (movement-speed :initarg :movement-speed)
   (jump-strength :initarg :jump-strength))
  (:default-initargs
   :hp-max 100
   :strength 5
   :movement-speed 200
   :jump-strength 10000))


(defmethod initialize-instance :after ((this player) &key world movement-speed jump-strength)
  (with-slots (body) this
    (setf body (make-box-body (universe-of world)
                              12
                              28
                              :owner this
                              :mass 1)
          (body-position body) (gk:vec2 50 20))))


(defun make-player (&key world movement-speed jump-strength)
  (make-instance 'player :world world
                         :movement-speed movement-speed
                         :jump-strength jump-strength))


(defun get-player ()
  "Returns player if we're inside a state, which has a player slot.
Returns NIL otherwise."
  (let ((curr-state (gk.fsm:current-state)))
    (if (slot-exists-p curr-state 'player )
        (player curr-state)
        nil)))


(defmethod add-state (state (this player))
  "Adds `state' to the players `states' list."
  (with-slots (states) this
   (if (keywordp state)
       (unless (member state states)
        (push state states))
       (error "~&state must be a keyword. Got ~A~%" state))))


(defmethod remove-state (state (this player))
  "Removes `state' to the players `states' list."
  (with-slots (states) this
    (if (keywordp state)
        (setf states (delete state states))
        (error "~&state must be a keyword. Got ~A~%" state))))


(defmethod dispose :after ((this player))
  (with-slots (body) this
   (dispose body)))


(defun move-player-right (player)
  (unless (moving-right-p player)
      (add-state :moving-right player)))

(defun move-player-left (player)
  (unless (moving-left-p player)
    (add-state :moving-left player)))


(defmethod moving-right-p ((this player))
  (with-slots (states)  this
    (and (member :moving-right states)
         (not (member :moving-left states)))))

(defmethod moving-left-p ((this player))
  (with-slots (states)  this
    (and (member :moving-left states)
         (not (member :moving-right states)))))

(defmethod idle-p ((this player))
  (or (null (states this))
      (not (a:xor (member :moving-left (states this))
                  (member :moving-right (states this))))))

(defmethod jumping-p ((this player))
  (member :jumping (states this)))


(defun stop-player (player)
  (setf (states player) (list)))

(defun stop-player-moving-left (player)
  (remove-state :moving-left player))

(defun stop-player-moving-right (player)
  (remove-state :moving-right player))

(defun jump-player (player)
  (unless (jumping-p player)
    (add-state :jumping player)
    (with-slots (states body jump-strength) player
      (let ((jx 0.18734788)
            (jy 0.95782626))
       (cond ((moving-left-p player)
              (apply-force body (gk:mult (gk:normalize (gk:vec2 (- jx) jy))
                                 jump-strength)))
             ((moving-right-p player)
              (apply-force body (gk:mult (gk:normalize (gk:vec2 jx jy))
                                         jump-strength)))
             ((idle-p player)
              (apply-force body (gk:vec2 0 10000))))))))


(defmethod collide ((this player) (that world))
  (with-slots (states movement-speed) this
    (remove-state :jumping this)
    (setf (collision-friction) 60)
    (cond ((moving-left-p this)
           (setf (collision-surface-velocity) (gk:vec2 (- 100) 0)))
          ((moving-right-p this)
           (setf (collision-surface-velocity) (gk:vec2 100 0)))
          ((idle-p this)
           (setf (collision-surface-velocity) (gk:vec2 0 0)))))
  t)


(defmethod collide ((that world) (this player))
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
      (gk:translate-canvas (gk:x position) ;; (- (gk:x position) 8)
                           (gk:y position) ;; (- (gk:y position) 5)
       ))
    (let ((time (bodge-util:real-time-seconds)))
      (cond ((idle-p this)
             (draw-animation 'player-idle-right time +zero-pos+))
            ((moving-left-p this)
             (gk:with-pushed-canvas ()
               (draw-animation 'player-run-left time +zero-pos+)))
            ((moving-right-p this)
             (draw-animation 'player-run-right time +zero-pos+))))))
