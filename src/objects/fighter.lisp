(cl:in-package :decent-game)


(defgeneric position-of (object))
(defgeneric speed-of (object)
  (:method (object)
    (declare (ignore object))
    0))

(defclass fighter (stats)
  ((states :initform (list) :accessor states)
   (direction :initarg :direction
              :accessor direction
              :initform (error "The fighter needs a direction he's facing.")
              :documentation "-1 for left, 0 for both, +1 for right")
   (last-direction :initarg :last-direction
                   :accessor last-direction
                   :documentation "Indicates which direction the player faced last. +1 = right, -1 = left")
   (body :initform nil :reader body-of))
  (:documentation "Something that can fight,has a body and stats."))

(defgeneric make-fighter-body (fighter &key &allow-other-keys))

(defmethod initialize-instance :after ((this fighter) &rest args &key &allow-other-keys)
  (with-slots (body) this
    (setf body (apply #'make-fighter-body this args))))

(defmethod damage-for (amount (this fighter))
  "Damages the `hp' of `this' for `amount'. `This' can't fall below 0 hp."
  (with-slots (hp) this
    (setf hp (a:clamp amount hp amount))))

(defmethod heal-for (amount (this fighter))
  "Heals the `hp' of `this' for `amount'. `This' can't heal beyond `hp-max'."
  (with-slots (hp hp-max) this
    (incf hp (a:clamp amount (- hp-max hp) amount))))


(defmethod render :around ((this fighter) &key)
  (with-slots (body) this
    (call-next-method)
    (when *debug-rendering*
      (render body))))


(defmethod position-of ((this fighter))
  (with-slots (body) this
    (body-position body)))



(defmethod collide :before ((this fighter) (that obstacle))
  (with-slots (states direction) this
    (setf (collision-friction) 60
          (collision-surface-velocity) (cond
                                         ((> direction 0) (gk:vec2 (speed-of this) 0))
                                         ((< direction 0) (gk:vec2 (- (speed-of this)) 0))
                                         (t (gk:vec2 0 0)))))
  t)


(defmethod process-collision ((this fighter) (that obstacle))
  (with-slots (body) this
    (setf (body-angular-velocity body) 0)))


(defmethod process-collision ((that obstacle) (this fighter))
  (process-collision this that))


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


(defmethod facing-right-p ((this fighter))
  (plusp (last-direction this)))


(defmethod facing-left-p ((this fighter))
  (minusp (last-direction this)))


(defmethod idle-p ((this fighter))
  (null (states this)))


(defmethod running-p ((this fighter))
  (member :running (states this)))


(defmethod jumping-p ((this fighter))
  (member :jumping (states this)))


(defmethod falling-p ((this fighter))
  (member :falling (states this)))


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

(defmethod move-right ((this fighter))
  (update-fighter-running-state this 1))


(defmethod stop-move-right ((this fighter))
  (update-fighter-running-state this -1))


(defmethod move-left ((this fighter))
  (update-fighter-running-state this -1))


(defmethod stop-move-left ((this fighter))
  (update-fighter-running-state this 1))


(defmethod stop-running ((this fighter))
  (remove-state :running this))
