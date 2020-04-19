(cl:in-package :decent-game)


(defclass init-screen (base-loading-screen) ()
  (:default-initargs :pack 'loading-screen-resources))



(defmethod on-load ((this init-screen))
  (gk.fsm:transition-to 'loading-screen
                        :pack 'main-menu-resources
                        :next-state 'main-menu))


(defmethod gk:draw ((this init-screen))
  (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+))
