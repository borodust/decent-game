(cl:in-package :decent-game)


(defclass world ()
  ((universe :initform nil :reader universe-of)
   (ground :initform nil)))


(defmethod initialize-instance :after ((this world) &key)
  (with-slots (universe ground) this
    (setf universe (make-universe (gk:vec2 0 -100))
          ground (make-box-body universe 100 10
                                :kinematic t))))


(defun make-world ()
  (make-instance 'world))


(defmethod dispose :after ((this universe))
  (with-slots (universe ground) this
    (dispose ground)
    (dispose universe)))


(defun observe-world (world)
  (with-slots (universe) world
    (observe-universe universe)))


(defmethod render ((this world))
  (with-slots (ground) this
    (render ground)))
