(cl:in-package :decent-game)


(define-resource-pack gameplay-debug-resources (player-resources))


(defclass init-gameplay-debug-screen () ())


(defmethod gk:post-initialize ((this init-gameplay-debug-screen))
  (gk.fsm:transition-to 'loading-screen :pack 'gameplay-debug-resources
                                        :next-state 'gameplay-debug-screen))



(defclass gameplay-debug-screen (state-input-handler)
  ((player :initform (make-player))))



(defmethod initialize-instance :after ((this gameplay-debug-screen) &key pack))


(defmethod gk:post-initialize ((this gameplay-debug-screen)))


(defmethod gk:act ((this gameplay-debug-screen)))


(defmethod gk:draw ((this gameplay-debug-screen))
  (with-slots (player) this
    (gk:translate-canvas 50 0)
    (render player)))


(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :d)))
  (with-slots (player) this
    (move-player-right player)))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :d)))
  (with-slots (player) this
    (stop-player player)))


(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :a)))
  (with-slots (player) this
    (move-player-left player)))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :a)))
  (with-slots (player) this
    (stop-player player)))
