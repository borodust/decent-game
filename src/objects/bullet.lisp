(cl:in-package :decent-game)


(defparameter *bullet-ttl* 2)


(defgeneric ttl-of (this))


(defclass bullet ()
  ((body :initform nil)
   (spawn-time :initform (bodge-util:real-time-seconds))))


(defmethod ttl-of ((this bullet))
  *bullet-ttl*)


(defmethod initialize-instance :after ((this bullet) &key world position velocity)
  (with-slots (body) this
    (setf body (make-circle-body (universe-of world) 3 :mass 0.1)
          (body-position body) position
          (body-linear-velocity body) velocity)))


(defmethod dispose :after ((this bullet))
  (with-slots (body) this
    (dispose body)))


(defmethod observe ((this bullet))
  (with-slots (spawn-time) this
    ))


(defgeneric render-bullet (object)
  (:method (object) (declare (ignore object))))


(defmethod render ((this bullet) &key)
  (with-slots (body) this
    (let ((pos (body-position body)))
      (gk:translate-canvas (gk:x pos) (gk:y pos)))
    (render-bullet this)
    (when *debug-rendering*
      (render body))))


(defun make-bullet (bullet-class world position velocity)
  (make-instance bullet-class :world world
                              :position position
                              :velocity velocity))
