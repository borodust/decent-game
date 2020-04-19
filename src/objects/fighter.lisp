(cl:in-package :decent-game)


(defgeneric position-of (object))
(defgeneric velocity-of (object))
(defgeneric speed-of (object)
  (:method (object)
    (declare (ignore object))
    0))

(defgeneric shoot (object))

;;;
;;; HITBOX
;;;
(defclass hitbox () ())


(defmethod collide :around ((this hitbox) anything)
  nil)

(defmethod collide :around (anything (this hitbox))
  nil)

;;;
;;; GENERIC FIGHTER
;;;
(defclass fighter (stats)
  ((states :initform (list) :accessor states)
   (body :initform nil :reader body-of))
  (:documentation "Something that can fight, has a body and stats."))

(defgeneric provide-fighter-body (fighter &key &allow-other-keys))

(defmethod initialize-instance :after ((this fighter) &rest args &key &allow-other-keys)
  (with-slots (body) this
    (setf body (apply #'provide-fighter-body this args))))

(defmethod damage-for (amount (this fighter))
  "Damages the `hp' of `this' for `amount'. `This' can't fall below 0 hp."
  (with-slots (hp) this
    (setf hp (a:clamp amount amount hp))))

(defmethod heal-for (amount (this fighter))
  "Heals the `hp' of `this' for `amount'. `This' can't heal beyond `hp-max'."
  (with-slots (hp hp-max) this
    (incf hp (a:clamp amount amount (- hp-max hp)))))

(defmethod increase-max-hp-by (x (this fighter))
  (with-slots (hp-max) this
   (incf hp-max x)))


(defmethod add-state (state (this fighter))
  "Adds `state' to the players `states' list.
With the exception of :left and :right. Those are added to `facing'."
  (with-slots (states) this
    (if (keywordp state)
        (pushnew state states)
        (error "~&state must be a keyword. Got ~A~%" state))))


(defmethod remove-state (state (this fighter))
  "Removes `state' to the players `states' list."
  (with-slots (states) this
    (if (keywordp state)
        (a:deletef states state)
        (error "~&state must be a keyword. Got ~A~%" state))))


(defmethod render :around ((this fighter) &key)
  (with-slots (body) this
    (call-next-method)
    (when *debug-rendering*
      (render body))))


(defmethod position-of ((this fighter))
  (with-slots (body) this
    (body-position body)))


(defmethod velocity-of ((this fighter))
  (with-slots (body) this
    (body-linear-velocity body)))

(defmethod moving-right-p ((this fighter))
  (let ((x-vel (gk:x (velocity-of this))))
    (and (plusp x-vel)
         (> x-vel .1))))

(defmethod moving-left-p ((this fighter))
  (let ((x-vel (gk:x (velocity-of this))))
    (and (minusp x-vel)
         (< x-vel -.1))))


;;;
;;; GROUND FIGHTER
;;;
(defclass ground-fighter (fighter)
  ((direction :initarg :direction
              :accessor direction
              :initform 0
              :documentation "-1 for left, 0 for both, +1 for right")
   (last-direction :initarg :last-direction
                   :accessor last-direction
                   :initform 1
                   :documentation "Indicates which direction the player faced last. +1 = right, -1 = left")))


(defmethod collide :before ((this ground-fighter) (that obstacle))
  (with-slots (states direction) this
    (setf (collision-friction) 60
          (collision-surface-velocity) (cond
                                         ((> direction 0) (gk:vec2 (speed-of this) 0))
                                         ((< direction 0) (gk:vec2 (- (speed-of this)) 0))
                                         (t (gk:vec2 0 0)))))
  t)


(defun maintain-upright-stance (ground-fighter)
  (with-slots (body) ground-fighter
    (setf (body-angular-velocity body) 0
          (body-rotation body) 0)))


(defmethod observe :before ((this ground-fighter))
  (maintain-upright-stance this))


(defmethod facing-right-p ((this ground-fighter))
  (plusp (last-direction this)))


(defmethod facing-left-p ((this ground-fighter))
  (minusp (last-direction this)))


(defmethod idle-p ((this ground-fighter))
  (null (states this)))


(defmethod running-p ((this ground-fighter))
  (member :running (states this)))


(defmethod jumping-p ((this ground-fighter))
  (let ((y-velocity (gk:y (velocity-of this))))
    (and (plusp y-velocity)
         (> y-velocity .1))))


(defmethod falling-p ((this ground-fighter))
  (let ((y-velocity (gk:y (velocity-of this))))
    (and (minusp y-velocity)
         (< y-velocity -.1))))


(defmethod shooting-p ((this ground-fighter))
  (member :shooting (states this)))


(defun update-fighter-running-state (this new-direction)
  (with-slots (direction last-direction) this
    (incf direction new-direction)
    (if (= 0 direction)
        (remove-state :running this)
        (progn
          (add-state :running this)
          (setf last-direction direction)))))


(defun stop-moving (fighter)
  (with-slots (direction) fighter
    (remove-state :running fighter)
    (setf direction 0)))

(defmethod move-right ((this ground-fighter))
  (update-fighter-running-state this 1))


(defmethod stop-move-right ((this ground-fighter))
  (update-fighter-running-state this -1))


(defmethod move-left ((this ground-fighter))
  (update-fighter-running-state this -1))


(defmethod stop-move-left ((this ground-fighter))
  (update-fighter-running-state this 1))


(defmethod stop-running ((this ground-fighter))
  (remove-state :running this))


(defmethod start-shooting ((this ground-fighter))
  (add-state :shooting this))

(defmethod stop-shooting ((this ground-fighter))
  (remove-state :shooting this))
