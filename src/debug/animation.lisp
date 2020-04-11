(cl:in-package :decent-game)


(define-animation :alien-shooter-0 (asset-path "img/alien-shooter-0/alien-shooter-0-walk-0.png")
  :frames 3)


(defclass animation-debug-screen (state-input-handler)
  ((ready :initform nil)))


(defmethod gk:post-initialize ((this animation-debug-screen))
  (with-slots (ready) this
    (gk:prepare-resources :alien-shooter-0)))


(defmethod gk:notice-resources ((this animation-debug-screen) &rest names)
  (declare (ignore names))
  (with-slots (ready) this
    (setf ready t)))


(defmethod gk:draw ((this animation-debug-screen))
  (with-slots (ready) this
    (if ready
        (let ((time (bodge-util:real-time-seconds)))
          (draw-animation :alien-shooter-0 time *zero-pos*))
        (gk:draw-text "Loading" *zero-pos*))))
