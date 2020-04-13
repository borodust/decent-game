(cl:in-package :decent-game)


(defclass loading-screen (state-input-handler)
  ((pack-name :initarg :pack :initform (error ":pack missing"))
   (pack :initform nil)
   (next-state :initform (error ":next-state missing") :initarg :next-state)
   (prepared-percentage :initform 0)))


(defmethod gk:post-initialize ((this loading-screen))
  (with-slots (pack-name pack) this
    (setf pack (load-resource-pack pack-name))))


(defmethod gk:act ((this loading-screen))
  (with-slots (pack next-state prepared-percentage) this
    (let ((count (pack-prepared-count pack))
          (total (pack-total-count pack)))
      (setf prepared-percentage (truncate (* (if (> total 0)
                                                 (/ count total)
                                                 1)
                                             100)))
      (when (= count total)
        (gk.fsm:transition-to next-state :pack pack)))))


(defmethod gk:draw ((this loading-screen))
  (with-slots (prepared-percentage) this
    (let ((time (bodge-util:real-time-seconds)))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas (+ 128 (* (sin time) 20))
                             (+ 72 (* (cos time) 20)))
        (gk:draw-circle +zero-pos+ (+ 5 (* (abs (* (sin time) (cos time))) 6)) :fill-paint +black+))
      (gk:with-pushed-canvas ()
        (gk:translate-canvas 230 6)
        (gk:scale-canvas 0.5 0.5)
        (gk:draw-text (format nil "~A %" prepared-percentage) +zero-pos+)))))
