(cl:in-package :decent-game)


(defclass loading-screen (state-input-handler)
  ((resources :initform (error ":resources missing") :initarg :resources)
   (next-state :initform (error ":next-state missing") :initarg :next-state)
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
  (with-slots (resources next-state) this
    (unless resources
      (gk.fsm:transition-to next-state))))


(defmethod gk:draw ((this loading-screen))
  (with-slots (resources total) this
    (let ((time (bodge-util:real-time-seconds)))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas (+ 128 (* (sin time) 20))
                             (+ 72 (* (cos time) 20)))
        (gk:draw-circle *zero-pos* (+ 5 (* (abs (* (sin time) (cos time))) 6)) :fill-paint *black*))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas 230 6)
        (gk:scale-canvas 0.5 0.5)
        (let ((resources-loaded-fract (if (> total 0)
                                          (/ (- total (length resources)) total)
                                          1)))
          (gk:draw-text (format nil "~A %" (truncate (* resources-loaded-fract 100))) *zero-pos*))))))
