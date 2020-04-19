(cl:in-package :decent-game)

(defparameter *player-movement-speed* 100)
(defparameter *player-jump-strength* 10000)


(define-animation player-idle-right (asset-path "img/player/player-idle-right.png") :frames 2)
(define-animation player-idle-left (asset-path "img/player/player-idle-left.png") :frames 2)
(define-animation player-run-right (asset-path "img/player/player-run-right.png") :frames 4)
(define-animation player-run-left (asset-path "img/player/player-run-left.png") :frames 4)
(define-animation player-slash-right (asset-path "img/player/player-slash-right.png") :frames 2)
(define-animation player-slash-left (asset-path "img/player/player-slash-left.png") :frames 2)
(define-animation player-shoot-right (asset-path "img/player/player-shoot-right.png") :frames 2)
(define-animation player-shoot-left (asset-path "img/player/player-shoot-left.png") :frames 2)
(define-animation player-hurt-right (asset-path "img/player/player-hurt-right.png") :frames 1)
(define-animation player-hurt-left (asset-path "img/player/player-hurt-left.png") :frames 1)
(define-animation player-jump-right (asset-path "img/player/player-jump-right.png") :frames 2)
(define-animation player-jump-left (asset-path "img/player/player-jump-left.png") :frames 2)
(define-animation player-jumping-right (asset-path "img/player/player-jumping-right.png") :frames 1)
(define-animation player-jumping-left (asset-path "img/player/player-jumping-left.png") :frames 1)
(define-animation player-falling-right (asset-path "img/player/player-falling-right.png") :frames 1)
(define-animation player-falling-left (asset-path "img/player/player-falling-left.png") :frames 1)
(define-animation player-dying-right (asset-path "img/player/player-dying-right.png") :frames 3)
(define-animation player-dying-left (asset-path "img/player/player-dying-left.png") :frames 3)
(define-animation player-projectile-0-right (asset-path "img/player/player-projectile-0-right.png") :frames 2)
(define-animation player-projectile-0-left (asset-path "img/player/player-projectile-0-left.png") :frames 2)

(define-resource-pack player-resources ()
  'player-idle-right
  'player-idle-left
  'player-run-right
  'player-run-left
  'player-slash-right
  'player-slash-left
  'player-shoot-right
  'player-shoot-left
  'player-hurt-right
  'player-hurt-left
  'player-jump-right
  'player-jump-left
  'player-jumping-right
  'player-jumping-left
  'player-falling-right
  'player-falling-left
  'player-dying-right
  'player-dying-left
  'player-projectile-0-right
  'player-projectile-0-left)


(defclass player (ground-fighter)
  ()
  (:default-initargs
   :direction 0
   :last-direction 1
   :hp-max 100
   :strength 5))


(defmethod provide-fighter-body ((this player) &key world position)
  (let ((body (make-circle-body (universe-of world)
                                5
                                :owner this
                                :mass 1)))
    (setf (body-position body) (or position (gk:vec2 50 20)))
    body))


(defun make-player (world &key position)
  (make-instance 'player :world world
                         :position position))


(defmethod dispose :after ((this player))
  (with-slots (body) this
    (dispose body)))


(defun stop-player (player)
  (setf (states player) (list)))


(defun jump-player (player)
  (with-slots (states body direction) player
    (cond ((idle-p player)
           (apply-force body (gk:vec2 0 *player-jump-strength*)))
          (t (let ((reverse-thrust (gk:vec2 (* -0.3713907 direction) 0.92847675))) ;; 0.4 1
               (apply-force body (gk:mult reverse-thrust *player-jump-strength*)))))))


(defmethod collide :after ((this sensor) (that player))
  (trigger-sensor-event this))


(defmethod render ((this player) &key)
  (with-slots (states body) this
    (let ((position (body-position body)))
      (gk:translate-canvas (- (gk:x position) 15)
                           (- (gk:y position) 5)))
    (let ((time (bodge-util:real-time-seconds)))
      (cond ((facing-right-p this)
             (if (running-p this)
                 (draw-animation 'player-run-right time +zero-pos+)
                 (draw-animation 'player-idle-right time +zero-pos+)))
            ((facing-left-p this)
             (if (running-p this)
                 (draw-animation 'player-run-left time +zero-pos+)
                 (draw-animation 'player-idle-left time +zero-pos+)))
            (t (error "Play should only be able to either face left or right."))))))


(defmethod speed-of ((this player))
  *player-movement-speed*)
