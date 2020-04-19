(cl:in-package :decent-game)


(defparameter *observation-step* 0.01)
(defparameter *observation-repeats* 4)


(defgeneric world-of (state))

(defclass world ()
  ((universe :initform nil :reader universe-of)
   (level :initform nil :reader level-of)
   (player :initform nil :reader player-of)
   (enemies :initform nil)
   (bullets :initform nil)))


(defmethod initialize-instance :after ((this world) &key level-name)
  (with-slots (universe level player) this
    (setf universe (make-universe (gk:vec2 0 -100))
          level (make-level level-name universe)
          player (make-player this :position (player-spawn-position-of level)))))


(defun make-world (level-name)
  (make-instance 'world :level-name level-name))


(defmethod dispose :after ((this world))
  (with-slots (universe level player enemies bullets) this
    (dispose player)
    (loop for enemy in enemies
          do (dispose enemy))
    (loop for bullet in bullets
          do (dispose bullet))
    (dispose level)
    (dispose universe)))


(defun observe-world (world)
  (with-slots (universe player enemies bullets) world
    (let ((*world* world))
      (observe-universe universe *observation-step* *observation-repeats*)
      (observe player)
      (loop for enemy in enemies
            do (observe enemy))
      (loop for bullet in bullets
            do (if (bullet-destroyed-p bullet)
                   (progn
                     (a:deletef bullets bullet)
                     (dispose bullet))
                   (observe bullet))))))


(defmethod render ((this world) &key)
  (with-slots (level player enemies bullets) this
    (render level :kind :background)
    (with-slots (body) player
      (gk:translate-canvas (truncate (+ (- (gk:x (body-position body))) 100)) 0))
    (render level :kind :foreground)
    (when *debug-rendering*
      (render level :kind :control-plane))
    (loop for enemy in enemies
          do (render enemy))
    (loop for bullet in bullets
          do (render bullet))
    (render player)))


(defun spawn-enemy (world type spawn-name &key offset)
  (with-slots (level enemies) world
    (a:when-let ((pos (find-enemy-spawn (level-of world) spawn-name)))
      (push (make-enemy type world :position (gk:add pos (or offset +zero-pos+))) enemies))))


(defun spawn-bullet (bullet-class world position velocity)
  (with-slots (bullets) world
    (push (make-bullet bullet-class world position velocity) bullets))
  (values))


(defmacro with-world ((world) &body body)
  `(let ((,world (world-of (gk.fsm:current-state))))
     ,@body))
