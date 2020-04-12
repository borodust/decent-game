(cl:in-package :decent-game)


(defclass gameplay-debug-screen (state-input-handler)
  ())


(defmethod gk:post-initialize ((this gameplay-debug-screen)))


(defmethod gk:act ((this gameplay-debug-screen)))


(defmethod gk:draw ((this gameplay-debug-screen))
  (with-slots () this))
