(cl:in-package :decent-game)


(define-animation debug-animation (asset-path "img/alien-shooter-0/alien-shooter-0-walk-0.png")
  :frames 3)


(define-resource-pack animation-debug-resources ()
  'debug-animation)


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
    (draw-animation 'debug-animation time *zero-pos*)))
