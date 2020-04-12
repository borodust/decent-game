(cl:in-package :decent-game)


(defclass loading-screen (state-input-handler)
  ((pack-names :initarg :packs :initform (error ":packs missing"))
   (packs :initform nil)
   (next-state :initform (error ":next-state missing") :initarg :next-state)
   (prepared-percentage :initform 0)))


(defmethod gk:post-initialize ((this loading-screen))
  (with-slots (pack-names packs) this
    (loop for name in pack-names
          do (push (load-resource-pack name) packs))))


(defmethod gk:act ((this loading-screen))
  (with-slots (packs next-state prepared-percentage) this
    (multiple-value-bind (count total)
        (loop for pack in packs
              sum (pack-prepared-count pack) into count
              sum (pack-total-count pack) into total
              finally (return (values count total)))
      (setf prepared-percentage (truncate (* (if (> total 0)
                                                 (/ count total)
                                                 1)
                                             100)))
      (when (= count total)
        (gk.fsm:transition-to next-state :packs packs)))))


(defmethod gk:draw ((this loading-screen))
  (with-slots (resources prepared-percentage) this
    (let ((time (bodge-util:real-time-seconds)))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas (+ 128 (* (sin time) 20))
                             (+ 72 (* (cos time) 20)))
        (gk:draw-circle *zero-pos* (+ 5 (* (abs (* (sin time) (cos time))) 6)) :fill-paint *black*))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas 230 6)
        (gk:scale-canvas 0.5 0.5)
        (gk:draw-text (format nil "~A %" prepared-percentage) *zero-pos*)))))
