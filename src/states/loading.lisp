(cl:in-package :decent-game)


(defclass loading-screen (state-input-handler) ())


(defmethod gk.input:button-pressed ((this loading-screen) (key (eql :enter)))
  (gk.fsm:transition-to 'main-menu))


(defmethod gk:draw ((this loading-screen))
  (let ((time (bodge-util:real-time-seconds)))
    (gk:with-pushed-canvas ()
      (gk:translate-canvas (+ 100 (* (sin time) 20))
                           (+ 72 (* (cos time) 20)))
      (gk:draw-text "LOADING" *zero-pos*))
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 25 25)
      (gk:draw-text "PRESS ENTER FOR MAIN MENU" *zero-pos*))))
