(cl:in-package :decent-game)


(define-animation :alien-shooter-0 (asset-path "img/alien-shooter-0/alien-shooter-0-walk-0.png")
  :frames 3)


(defclass animation-debug-screen (state-input-handler) ())


(defmethod gk:draw ((this animation-debug-screen))
  (let ((time (bodge-util:real-time-seconds)))
    (draw-animation :alien-shooter-0 time *zero-pos*)))
