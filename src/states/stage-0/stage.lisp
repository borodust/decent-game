(cl:in-package :decent-game)


(gk:define-sound gameplay-tune (asset-path "snd/music/combat-zone.ogg"))


(define-level stage-0 (asset-path "tld/stage-0/stage.sxp")
  (stage-0-tilesheet (asset-path "tld/stage-0/tiles.png") "tiles.png"))


(define-resource-pack stage-0-resources (player-resources
                                         alien-shooter-0-resources
                                         alien-stinger-0-resources)
  (level-resources 'stage-0)
  'gameplay-tune)


;;;
;;; INIT
;;;
(defclass init-stage-0 () ())


(defmethod gk:post-initialize ((this init-stage-0))
  (gk.fsm:transition-to 'loading-screen :pack 'stage-0-resources
                                        :next-state 'stage-0))

;;;
;;; STAGE 0
;;;
(defclass stage-0 (state-input-handler)
  ((world :initform nil :reader world-of)
   (pack :initarg :pack)))


(defun spawn-flyer (action &key)
  (declare (ignore action))
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 1
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-stinger "flyer-0" :offset offset))))


(defun spawn-shooter (action &key)
  (declare (ignore action))
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 1
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-shooter "shooter-0" :offset offset))))


(defmethod gk:post-initialize ((this stage-0))
  (with-slots (world) this
    (setf world (make-world 'stage-0))
    (gk:play-sound 'gameplay-tune :looped-p t)))


(defmethod gk:pre-destroy ((this stage-0))
  (with-slots (world pack) this
    (gk:stop-sound 'gameplay-tune)
    (dispose world)
    (dispose-resource-pack pack)))


(defmethod gk:act ((this stage-0))
  (with-slots (world) this
    (observe-world world)))


(defmethod gk:draw ((this stage-0))
  (with-slots (world) this
    (render world)
    (draw-player-life-bar (player-of world) (gk:vec2 10 125))))

;;;
;;; input handling
;;;
(defmethod gk.input:button-released ((this stage-0) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))

(defmethod gk.input:button-pressed ((this stage-0) (button (eql :d)))
  (with-slots (world) this
    (move-right (player-of world))))


(defmethod gk.input:button-released ((this stage-0) (button (eql :d)))
  (with-slots (world) this
    (stop-move-right (player-of world))))


(defmethod gk.input:button-pressed ((this stage-0) (button (eql :a)))
  (with-slots (world) this
    (move-left (player-of world))))


(defmethod gk.input:button-released ((this stage-0) (button (eql :a)))
  (with-slots (world) this
    (stop-move-left (player-of world))))


(defmethod gk.input:button-released ((this stage-0) (button (eql :space)))
  (with-slots (world) this
    (jump-player (player-of world))))


(defmethod gk.input:button-pressed ((this stage-0) (button (eql :enter)))
  (with-slots (world) this
    (start-shooting (player-of world))
    (shoot (player-of world))))


(defmethod gk.input:button-released ((this stage-0) (button (eql :enter)))
  (with-slots (world) this
    (stop-shooting (player-of world))))


(defmethod gk.input:button-pressed ((this stage-0) (button (eql :j)))
  (with-slots (world) this
    (start-shooting (player-of world))
    (shoot (player-of world))))


(defmethod gk.input:button-released ((this stage-0) (button (eql :j)))
  (with-slots (world) this
    (stop-shooting (player-of world))))
