(cl:in-package :decent-game)

(define-animation background-area-0 (asset-path "img/backgrounds/area-0/background-area-0.png")  :frames 1)


(define-resource-pack animation-debug-resources (player-resources
                                                 alien-stinger-0-resources
                                                 alien-shooter-0-resources)
  'background-area-0)


;;;
;;; INIT
;;;
(defclass init-animation-debug-screen () ())


(defmethod gk:post-initialize ((this init-animation-debug-screen))
  (gk.fsm:transition-to 'loading-screen :pack 'animation-debug-resources
                                        :next-state 'animation-debug-screen))


;;;
;;; ANIMATION
;;;
(defclass animation-debug-screen (state-input-handler)
  ((pack :initarg :pack)))


(defmethod gk:pre-destroy ((this animation-debug-screen))
  (with-slots (pack) this
    (dispose-resource-pack pack)))


(defmethod gk:draw ((this animation-debug-screen))
  (let ((time (bodge-util:real-time-seconds)))
    (draw-animation 'background-area-0 time +zero-pos+)

    (draw-animation 'player-jump-right time +zero-pos+)
    (draw-animation 'player-jumping-right time (gk:add +zero-pos+ (gk:vec2  32 0)))
    (draw-animation 'player-falling-right time (gk:add +zero-pos+ (gk:vec2  64 0)))
    (draw-animation 'player-run-right     time (gk:add +zero-pos+ (gk:vec2  96 0)))
    (draw-animation 'player-hurt-right    time (gk:add +zero-pos+ (gk:vec2 128 0)))
    (draw-animation 'player-slash-right   time (gk:add +zero-pos+ (gk:vec2 150 0)))
    (draw-animation 'player-shoot-right   time (gk:add +zero-pos+ (gk:vec2 182 0)))
    (draw-animation 'player-projectile-0-right time (gk:add +zero-pos+ (gk:vec2 224 16)))
    (draw-animation 'player-dying-right time (gk:add +zero-pos+ (gk:vec2 182 32)))

    (draw-animation 'alien-shooter-0-walk-0 time         (gk:add +zero-pos+ (gk:vec2 0 120)))
    (draw-animation 'alien-shooter-0-attack-shoot-0 time (gk:add +zero-pos+ (gk:vec2 32 120)))

    (draw-animation 'alien-stinger-0-fly-0 time (gk:add +zero-pos+ (gk:vec2 0 64)))
    (draw-animation 'alien-stinger-0-attack-sting-0 time (gk:add +zero-pos+ (gk:vec2 32 64)))
    ))


;;; input handling
(defmethod gk.input:button-released ((this animation-debug-screen) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))
