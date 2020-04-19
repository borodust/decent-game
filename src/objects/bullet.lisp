(cl:in-package :decent-game)


(defparameter *bullet-ttl* 2)


(defgeneric ttl-of (this))


(defclass bullet ()
  ((body :initform nil)
   (spawn-time :initform (bodge-util:real-time-seconds))
   (velocity :initarg :velocity)))


(defmethod ttl-of ((this bullet))
  *bullet-ttl*)


(defmethod initialize-instance :after ((this bullet) &key world position velocity)
  (with-slots (body) this
    (setf body (make-circle-body (universe-of world) 3 :owner this :mass 0.1)
          (body-position body) position)))


(defmethod dispose :after ((this bullet))
  (with-slots (body) this
    (dispose body)))


(defun bullet-destroyed-p (bullet)
  (with-slots (spawn-time) bullet
    (> (- (bodge-util:real-time-seconds) spawn-time) (ttl-of bullet))))


(defun destroy-bullet (bullet)
  (with-slots (spawn-time) bullet
    (setf spawn-time 0)))


(defmethod collide :around ((this bullet) anything)
  (call-next-method)
  nil)

(defmethod collide :around (anything (this bullet))
  (call-next-method)
  nil)

(defmethod collide :after ((this bullet) (that obstacle))
  (declare (ignore that))
  (destroy-bullet this))


(defmethod collide :after ((that obstacle) (this bullet))
  (declare (ignore that))
  (destroy-bullet this))


(defmethod collide :after ((this bullet) (that hitbox))
  (declare (ignore that))
  (destroy-bullet this))


(defmethod collide :after ((that hitbox) (this bullet))
  (declare (ignore that))
  (destroy-bullet this))


(defmethod observe ((this bullet))
  (with-slots (body velocity) this
    (setf (body-linear-velocity body) velocity)))


(defgeneric render-bullet (object)
  (:method (object) (declare (ignore object))))


(defmethod render ((this bullet) &key)
  (with-slots (body) this
    (unless (bullet-destroyed-p this)
      (let ((pos (body-position body)))
        (gk:translate-canvas (gk:x pos) (gk:y pos)))
      (render-bullet this))
    (when *debug-rendering*
      (render body))))


(defun make-bullet (bullet-class world position velocity)
  (make-instance bullet-class :world world
                              :position position
                              :velocity velocity))
