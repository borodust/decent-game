(cl:in-package :decent-game)


(defclass world ()
  ((universe :initform nil :reader universe-of)
   (ground :initform nil)
   (obstacles :initform nil)))


(defun make-obstacle (world origin width height)
  (let ((o (make-box-body (universe-of world) width height :kinematic t :owner world)))
    (setf (body-position o) origin)
    o))


(defmethod initialize-instance :after ((this world) &key)
  (with-slots (universe ground obstacles) this
    (setf universe (make-universe (gk:vec2 0 -100))
          ground (make-box-body universe 10000 10
                                :kinematic t
                                :owner this)
          (body-position ground) (gk:vec2 -1000 0)
          obstacles (list (make-obstacle this (gk:vec2 120 40) 100 10)))))


(defun make-world ()
  (make-instance 'world))


(defmethod dispose :after ((this universe))
  (with-slots (universe ground obstacles) this
    (dispose ground)
    (dispose universe)
    (loop for obstacle in obstacles
          do (dispose obstacle))))


(defun observe-world (world)
  (with-slots (universe) world
    (observe-universe universe)))


(defmethod render ((this world))
  (with-slots (ground obstacles) this
    (render ground)
    (loop for obstacle in obstacles
          do (render obstacle))))


(defmethod collide ((this world) (that world))
  nil)
