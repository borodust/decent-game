(cl:in-package :decent-game)


(defclass init-screen () ())


(defmethod gk:post-initialize ((this init-screen))
  (gk.fsm:transition-to 'loading-screen
                        :resources '(:bold-pixel-operator
                                     :pixel-operator
                                     :menu-theme)
                        :next-state 'main-menu))
