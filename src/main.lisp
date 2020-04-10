(cl:in-package :decent-game)

(gk:defgame decent-game (gk.fsm:fistmachine)
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "DECENT-GAME")
  (:depends-on ge.phy:physics-system)
  (:prepare-resources nil))


(defun play ()
  (gk:start 'decent-game))