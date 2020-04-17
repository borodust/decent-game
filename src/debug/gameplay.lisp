(cl:in-package :decent-game)


(define-resource-pack gameplay-debug-resources (player-resources
                                                alien-shooter-0-resources
                                                alien-stinger-0-resources))


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
  ((player :initform nil :accessor player)
   (enemies :initform nil :accessor enemies)
   (world :initform nil)
   (pack :initarg :pack)))

(defmethod initialize-instance :after ((this gameplay-debug-screen) &key)
  (with-slots (player enemies world) this
    (setf world (make-world)
          player (make-player world)
          enemies (make-enemies :world world
                                :enemy-type-list
                                '(alien-shooter
                                  alien-stinger)))))


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
  (with-slots (world player enemies) this
    (render world)
    (render player)
    (render-enemies enemies)))


;;; input handling
(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :d)))
  (with-slots (player) this
    (format t "move player right.~%")
    (move-right player)))

(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :d)))
  (with-slots (player) this
    (format t "stop moving player right.~%")
    (stop-move-right player)))


(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :a)))
  (with-slots (player) this
    (format t "move player left.~%")
    (move-left player)))

(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :a)))
  (with-slots (player) this
    (format t "stop moving player left.~%")
    (stop-move-left player)))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :space)))
  (with-slots (player) this
    (jump-player player)))

(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))
