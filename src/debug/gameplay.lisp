(cl:in-package :decent-game)


(define-level test-level (asset-path "tld/test/test.sxp")
  (test-level-tilesheet (asset-path "tld/test/tiles.png") "tiles.png"))


(define-resource-pack gameplay-debug-resources (boss-resources
                                                player-resources
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
  ((world :initform nil :reader world-of)
   (pack :initarg :pack)))


(defun spawn-debug-flyer (action &key)
  (declare (ignore action))
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 1
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-stinger "flyer-0" :offset offset))))


(defun spawn-debug-shooter (action &key)
  (declare (ignore action))
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 1
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-shooter "shooter-0" :offset offset))))


(defmethod gk:post-initialize ((this gameplay-debug-screen))
  (with-slots (world) this
    (setf world (make-world 'test-level))
    (subscribe-to-event :spawn-flyer-0 'spawn-debug-flyer)
    (subscribe-to-event :spawn-shooter-0 'spawn-debug-shooter)))


(defmethod gk:pre-destroy ((this gameplay-debug-screen))
  (with-slots (world pack) this
    (dispose world)
    (dispose-resource-pack pack)))


(defmethod gk:act ((this gameplay-debug-screen))
  (with-slots (world) this
    ;; (format t "~&player-shooting?: ~A~%" (shooting-p (player-of world)))
    (observe-world world)))


(defmethod gk:draw ((this gameplay-debug-screen))
  (with-slots (world) this
    (render world)
    (draw-player-life-bar (player-of world) (gk:vec2 10 125))
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


(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :enter)))
  (with-slots (world) this
    (start-shooting (player-of world))
    (shoot (player-of world))))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :enter)))
  (with-slots (world) this
    (stop-shooting (player-of world))))


(defmethod gk.input:button-pressed ((this gameplay-debug-screen) (button (eql :j)))
  (with-slots (world) this
    (start-shooting (player-of world))
    (shoot (player-of world))))


(defmethod gk.input:button-released ((this gameplay-debug-screen) (button (eql :j)))
  (with-slots (world) this
    (stop-shooting (player-of world))))
