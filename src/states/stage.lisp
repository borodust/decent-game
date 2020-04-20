(cl:in-package :decent-game)


;;;
;;; INIT
;;;
(defclass init-stage (loading-screen) ())


;;;
;;; PREPARE
;;;
(defgeneric provide-world (prepare-stage))


(defclass prepare-stage ()
  ((pack :initarg :pack :initform (error ":pack missing"))
   (stage :initarg :stage :initform (error ":stage missing"))
   (level :initarg :level :initform nil)))


(defmethod provide-world ((this prepare-stage))
  (with-slots (level) this
    (make-world level)))


(defmethod gk:post-initialize :after ((this prepare-stage))
  (with-slots (pack stage) this
    (gk.fsm:transition-to stage :world (provide-world this) :pack pack)))


;;;
;;; DESTROY
;;;
(defclass destroy-stage ()
  ((world :initarg :world :initform (error ":world missing"))
   (pack :initarg :pack :initform (error ":pack missing"))
   (next :initarg :next :initform (error ":next missing"))))


(defmethod gk:post-initialize :after ((this destroy-stage))
  (with-slots (world pack next) this
    (dispose world)
    (dispose-resource-pack pack)
    (gk.fsm:transition-to next)))

;;;
;;; PLAY
;;;
(defclass stage (state-input-handler)
  ((world :initarg :world :initform (error ":world missing") :reader world-of)
   (pack :initarg :pack :initform (error ":pack missing"))
   (init :initarg :init :initform (error ":init missing"))
   (next :initarg :next :initform (error ":next missing"))
   (destroy :initarg :destroy :initform (error ":destroy missing"))))


(defun spawn-flyer (spawn)
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 1
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-stinger spawn :offset offset))))


(defun spawn-shooter (spawn)
  (with-slots (world) (gk.fsm:current-state)
    (loop repeat 1
          for offset = (gk:vec2 (- (random 10) 5) (- (random 10) 5))
          do (spawn-enemy world 'alien-shooter spawn :offset offset))))


(defun call-enemy (action &key spawn type)
  (declare (ignore action))
  (a:switch (type :test #'equal)
    ("shooter" (spawn-shooter spawn))
    ("stinger" (spawn-shooter spawn))))


(defun kill-player (action &key)
  (declare (ignore action))
  (with-slots (init) (gk.fsm:current-state)
    (gk.fsm:transition-to init)))


(defun finish-level (action &key)
  (declare (ignore action))
  (with-slots (next destroy world pack) (gk.fsm:current-state)
    (gk.fsm:transition-to destroy :next next :world world :pack pack)))


(defmethod gk:post-initialize :before ((this stage))
  (with-slots (world) this
    (subscribe-to-event :player-death 'kill-player)
    (subscribe-to-event :enemy-call 'call-enemy)
    (subscribe-to-event :level-finished 'finish-level)))


(defmethod gk:pre-destroy :after ((this stage))
  (with-slots (world pack) this
    (unsubscribe-from-event :player-death 'kill-player)
    (unsubscribe-from-event :enemy-call 'call-enemy)
    (unsubscribe-from-event :level-finished 'finish-level)))


(defmethod gk:act :before ((this stage))
  (with-slots (world) this
    (observe-world world)))


(defmethod gk:draw :before ((this stage))
  (with-slots (world) this
    (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+)
    (render world)
    (when (boss-exists-p world)
      (draw-boss-life-bar (get-boss-of world) (gk:vec2 156 126)))
    (draw-player-life-bar (player-of world) (gk:vec2 10 125))))

;;;
;;; input handling
;;;
(defmethod gk.input:button-released ((this stage) (button (eql :escape)))
  (with-slots (world pack) this
    (gk.fsm:transition-to 'pause-screen :world world :pack pack)))


(defmethod gk.input:button-pressed ((this stage) (button (eql :d)))
  (with-slots (world) this
    (move-right (player-of world))))


(defmethod gk.input:button-released ((this stage) (button (eql :d)))
  (with-slots (world) this
    (stop-move-right (player-of world))))


(defmethod gk.input:button-pressed ((this stage) (button (eql :a)))
  (with-slots (world) this
    (move-left (player-of world))))


(defmethod gk.input:button-released ((this stage) (button (eql :a)))
  (with-slots (world) this
    (stop-move-left (player-of world))))


(defmethod gk.input:button-released ((this stage) (button (eql :space)))
  (with-slots (world) this
    (jump-player (player-of world))))


(defmethod gk.input:button-pressed ((this stage) (button (eql :enter)))
  (with-slots (world) this
    (start-shooting (player-of world))
    (shoot (player-of world))))


(defmethod gk.input:button-released ((this stage) (button (eql :enter)))
  (with-slots (world) this
    (stop-shooting (player-of world))))


(defmethod gk.input:button-pressed ((this stage) (button (eql :j)))
  (with-slots (world) this
    (start-shooting (player-of world))
    (shoot (player-of world))))


(defmethod gk.input:button-released ((this stage) (button (eql :j)))
  (with-slots (world) this
    (stop-shooting (player-of world))))
