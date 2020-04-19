(cl:in-package :decent-game)


(define-level test-level (asset-path "tld/test/test.sxp")
  (test-level-tilesheet (asset-path "tld/test/tiles.png") "tiles.png"))


(define-resource-pack gameplay-debug-resources (player-resources
                                                alien-shooter-0-resources
                                                alien-stinger-0-resources)
  (level-resources 'test-level))


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
  ((world :initform nil)
   (pack :initarg :pack)))


(defun spawn-flyer (action &key)
  (declare (ignore action))
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 10
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-stinger "flyer-0" :offset offset))))


(defun spawn-shooter (action &key)
  (declare (ignore action))
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 10
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-shooter "shooter-0" :offset offset))))


(defmethod gk:post-initialize ((this gameplay-debug-screen))
  (with-slots (world) this
    (setf world (make-world 'test-level))
    (subscribe-to-event :spawn-flyer-0 'spawn-flyer)
    (subscribe-to-event :spawn-shooter-0 'spawn-shooter)))


(defmethod gk:pre-destroy ((this gameplay-debug-screen))
  (with-slots (world pack) this
    (dispose world)
    (dispose-resource-pack pack)))


(defmethod gk:act ((this gameplay-debug-screen))
  (with-slots (world) this
    (observe-world world)))


(defmethod gk:draw ((this gameplay-debug-screen))
  (with-slots (world) this
    (render world)
    ;;(draw-boss-life-bar boss)
    ;;(draw-player-life-bar player)
    ))


;;; input handling
(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :d)))
  (with-slots (world) this
    (move-right (player-of world))))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :d)))
  (with-slots (world) this
    (stop-move-right (player-of world))))


(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :a)))
  (with-slots (world) this
    (move-left (player-of world))))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :a)))
  (with-slots (world) this
    (stop-move-left (player-of world))))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :space)))
  (with-slots (world) this
    (jump-player (player-of world))))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))
