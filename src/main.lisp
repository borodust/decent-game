(cl:in-package :decent-game)

(gk:defgame decent-game (gk.fsm:fistmachine)
  ()
  (:viewport-width 768)
  (:viewport-height 432)
  (:canvas-width 256)
  (:canvas-height 144)
  (:viewport-title "DECENT-GAME")
  (:depends-on ge.phy:physics-system)
  (:prepare-resources nil)
  (:default-initargs :initial-state 'init-screen))


(defmethod gk:notice-resources ((this decent-game) &rest names)
  (shout "Resources loaded: ~{~A ~}" names)
  (apply #'notice-pack-resources names))


(defmethod gk:pre-destroy :after ((this decent-game))
  (dispose-pack-resources))


(defun play ()
  (gk:start 'decent-game))

(defmethod gk:act ((this decent-game))
  (process-timers))
