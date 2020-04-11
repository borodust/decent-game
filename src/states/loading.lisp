(cl:in-package :decent-game)


(defclass loading-screen (state-input-handler)
  ((resources :initform '(:bold-pixel-operator
                          :pixel-operator
                          :menu-theme))
   (total :initform 0)))


(defmethod gk:post-initialize ((this loading-screen))
  (with-slots (resources total) this
    (setf total (length resources))
    (loop for resource in resources
          do (gk:prepare-resources resource))))


(defmethod gk:notice-resources ((this loading-screen) &rest names)
  (with-slots (resources) this
    (loop for name in names
          do (a:removef resources name))))


(defmethod gk:act ((this loading-screen))
  (with-slots (resources) this
    (unless resources
      (gk.fsm:transition-to 'main-menu))))


(defmethod gk:draw ((this loading-screen))
  (with-slots (resources total) this
    (let ((time (bodge-util:real-time-seconds)))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas (+ 100 (* (sin time) 20))
                             (+ 72 (* (cos time) 20)))
        (gk:draw-text "LOADING" *zero-pos*))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas 230 6)
        (gk:scale-canvas 0.5 0.5)
        (gk:draw-text (format nil "~A %" (* (truncate (/ (- total (length resources)) total)) 100))
                      *zero-pos*)))))
