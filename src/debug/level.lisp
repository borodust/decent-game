(cl:in-package :decent-game)


(gk:define-text test-level (asset-path "tld/test/test.sxp"))

(gk:define-image test-level-tilesheet (asset-path "tld/test/tiles.png")
  :use-nearest-interpolation t)


(define-resource-pack level-debug-resources (player-resources)
  'test-level
  'test-level-tilesheet)


;;;
;;; INIT
;;;
(defclass init-level-debug-screen () ())


(defmethod gk:post-initialize ((this init-level-debug-screen))
  (gk.fsm:transition-to 'loading-screen :pack 'level-debug-resources
                                        :next-state 'level-debug-screen))


;;;
;;; ANIMATION
;;;
(defclass level-debug-screen (state-input-handler)
  ((pack :initarg :pack)
   (level :initform nil)
   (player :initform nil)
   (world :initform nil)))


(defmethod gk:post-initialize ((this level-debug-screen))
  (with-slots (level player world) this
    (with-input-from-string (in (gk:get-text 'test-level))
      (setf level (make-level (uiop:with-safe-io-syntax ()
                                (read in))
                              (list "tiles.png" 'test-level-tilesheet))
            world (make-world)
            player (make-player world :position (player-spawn-position-of level))))))


(defmethod gk:pre-destroy ((this level-debug-screen))
  (with-slots (pack player world) this
    (dispose player)
    (dispose world)
    (dispose-resource-pack pack)))


(defmethod gk:draw ((this level-debug-screen))
  (with-slots (level player world) this
    (render level)
    (render player)
    (render world)))


;;; input handling
(defmethod gk.input:button-released ((this level-debug-screen) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))
