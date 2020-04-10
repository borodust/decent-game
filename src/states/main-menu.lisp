(cl:in-package :decent-game)


(defclass main-menu (state-input-handler) ())


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :space)))
  (gk.fsm:transition-to 'loading-screen))


(defmethod gk:draw ((this main-menu))
  (gk:with-pushed-canvas ()
    (gk:translate-canvas 25 280)
    (gk:scale-canvas 3 3)
    (gk:draw-text "PRESS SPACE FOR LOADING SCREEN" *zero-pos*)))
