(cl:in-package :decent-game)


(defclass world ()
  ((universe :initform nil :reader universe-of)
   (level :initform nil :reader level-of)
   (player :initform nil :reader player-of)))


(defun make-obstacle (world origin width height)
  (let ((o (make-box-body (universe-of world) width height :kinematic t :owner world)))
    (setf (body-position o) origin)
    o))


(defmethod initialize-instance :after ((this world) &key level-name)
  (with-slots (universe level player) this
    (setf universe (make-universe (gk:vec2 0 -100))
          level (make-level level-name universe)
          player (make-player this :position (player-spawn-position-of level)))))


(defun make-world (level-name)
  (make-instance 'world :level-name level-name))


(defmethod dispose :after ((this world))
  (with-slots (universe level player) this
    (dispose player)
    (dispose level)
    (dispose universe)))


(defun observe-world (world)
  (with-slots (universe) world
    (observe-universe universe)))


(defmethod render ((this world) &key)
  (with-slots (level player) this
    (render level :kind :background)
    (with-slots (body) player
      (gk:translate-canvas (truncate (+ (- (gk:x (body-position body))) 100)) 0))
    (render level :kind :foreground)
    (render player)))
