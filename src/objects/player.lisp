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


(defclass player (stats movable)
  ((states :initform (list) :accessor states)
   (body :initform nil)
   (jump-force :initarg :jump-force))
  (:default-initargs
   :hp-max 100
   :strength 5
   :movement-speed 200
   :jump-force 10800))


(defmethod initialize-instance :after ((this player) &key world)
  (with-slots (body) this
    (setf body (make-circle-body (universe-of world) 5
                                 :owner this
                                 :mass 1)
          (body-position body) (gk:vec2 50 20))))


(defun make-player (world)
  (make-instance 'player :world world))


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


(defun stop-player (player)
  (setf (states player) (list)))

(defun stop-player-moving-left (player)
  (remove-state :moving-left player))

(defun stop-player-moving-right (player)
  (remove-state :moving-right player))

(defun jump-player (player)
  (add-state :jumping player)
  (with-slots (states body jump-force) player
    (cond ((moving-left-p player)
           (apply-force body (gk:mult (gk:normalize (gk:vec2 -0.28734788 0.95782626))
                                      jump-force)))
          ((moving-right-p player)
           (apply-force body (gk:mult (gk:normalize (gk:vec2  0.28734788 0.95782626))
                                      jump-force)))
          ((idle-p player)
           (apply-force body (gk:vec2 0 10000))))))


(defmethod collide ((this player) (that world))
  (with-slots (states) this
    (remove-state :jumping this)
    (setf (collision-friction) 60)
    (cond ((moving-left-p this)
           (setf (collision-surface-velocity) (gk:vec2 -100 0)))
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
      (gk:translate-canvas (- (gk:x position) 8) (- (gk:y position) 5)))
    (let ((time (bodge-util:real-time-seconds)))
      (cond ((idle-p this)
             (draw-animation 'player-idle time +zero-pos+))
            ((moving-left-p this)
             (gk:with-pushed-canvas ()
               (draw-animation 'player-walk time +zero-pos+ :mirror-x t)))
            ((moving-right-p this)
             (draw-animation 'player-walk time +zero-pos+))))))
