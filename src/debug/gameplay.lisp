(cl:in-package :decent-game)


(define-resource-pack gameplay-debug-resources (player-resources))


;;;
;;; INIT
;;;
(defclass init-gameplay-debug-screen () ())


(defmethod gk:post-initialize ((this init-gameplay-debug-screen))
  (gk.fsm:transition-to 'loading-screen :pack 'gameplay-debug-resources
                                        :next-state 'gameplay-debug-screen))


;;;
;;; SCENE
;;;
(defclass gameplay-debug-screen (state-input-handler)
  ((player :initform nil)
   (world :initform nil)
   (pack :initarg :pack)))


(defmethod initialize-instance :after ((this gameplay-debug-screen) &key)
  (with-slots (player world) this
    (setf world (make-world)
          player (make-player world))))


(defmethod gk:post-initialize ((this gameplay-debug-screen)))


(defmethod gk:pre-destroy ((this gameplay-debug-screen))
  (with-slots (player world pack) this
    (dispose player)
    (dispose world)
    (dispose-resource-pack pack)))


(defmethod gk:act ((this gameplay-debug-screen))
  (with-slots (world) this
    (observe-world world)))


(defmethod gk:draw ((this gameplay-debug-screen))
  (with-slots (world player) this
    (render world)
    (render player)))


;;; input handling
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


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :space)))
  (with-slots (player) this
    (jump-player player)))

(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))
