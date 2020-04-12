(cl:in-package :decent-game)


(define-resource-pack gameplay-debug-resources ())


(defclass gameplay-debug-screen (state-input-handler)
  ((packs :initarg :packs :initform nil)))


(defmethod initialize-instance :after ((this gameplay-debug-screen) &key packs)
  (unless packs
    (gk.fsm:transition-to 'loading-screen :packs '(gameplay-debug-resources))))


(defmethod gk:post-initialize ((this gameplay-debug-screen)))


(defmethod gk:act ((this gameplay-debug-screen)))


(defmethod gk:draw ((this gameplay-debug-screen))
  (with-slots () this))
