(cl:in-package :decent-game)


(defclass world ()
  ((universe :initform nil :reader universe-of)
   (level :initform nil :reader level-of)
   (player :initform nil :reader player-of)
   (enemies :initform nil)))


(defmethod initialize-instance :after ((this world) &key level-name)
  (with-slots (universe level player) this
    (setf universe (make-universe (gk:vec2 0 -100))
          level (make-level level-name universe)
          player (make-player this :position (player-spawn-position-of level)))))


(defun make-world (level-name)
  (make-instance 'world :level-name level-name))


(defmethod dispose :after ((this world))
  (with-slots (universe level player enemies) this
    (dispose player)
    (loop for enemy in enemies
          do (dispose enemy))
    (dispose level)
    (dispose universe)))


(defun observe-world (world)
  (with-slots (universe) world
    (observe-universe universe)))


(defmethod render ((this world) &key)
  (with-slots (level player enemies) this
    (render level :kind :background)
    (with-slots (body) player
      (gk:translate-canvas (truncate (+ (- (gk:x (body-position body))) 100)) 0))
    (render level :kind :foreground)
    (when *debug-rendering*
      (render level :kind :control-plane))
    (loop for enemy in enemies
          do (render enemy))
    (render player)))


(defun spawn-enemy (world type spawn-name &key offset)
  (with-slots (level enemies) world
    (a:when-let ((pos (find-enemy-spawn (level-of world) spawn-name)))
      (push (make-enemy type world :position (gk:add pos (or offset +zero-pos+))) enemies))))
