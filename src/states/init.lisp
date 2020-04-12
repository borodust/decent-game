(cl:in-package :decent-game)


(defclass init-screen () ())


(defmethod gk:post-initialize ((this init-screen))
  (gk.fsm:transition-to 'loading-screen
                        :packs '(font-resources main-menu-resources)
                        :next-state 'main-menu))
