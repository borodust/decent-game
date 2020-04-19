(cl:in-package :decent-game)


(gk:define-image :loading-screen-bar-overlay (asset-path "img/hud/loading-screen-bar-overlay.png")
  :use-nearest-interpolation t)

(define-animation loading-screen-animation (asset-path "img/hud/loading-screen-anim-sprite-sheet.png")
  :frames 12)

(define-resource-pack loading-screen-resources ()
  'loading-screen-animation
  :loading-screen-bar-overlay
  :bold-pixel-operator)


(defclass loading-screen (base-loading-screen)
  ((next-state :initform (error ":next-state missing") :initarg :next-state)))


(defmethod gk:draw ((this loading-screen))
  (with-slots (prepared-percentage) this
    (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+)
    (gk:with-pushed-canvas ()
      (draw-loading-bar prepared-percentage (gk:vec2 8 8) :bar-height 16 :bar-width 240)
      (gk:draw-image +zero-pos+ :loading-screen-bar-overlay))

    (gk:with-pushed-canvas ()
      (gk:translate-canvas 9 10)
      (gk:scale-canvas 0.5 0.5)
      (gk:draw-text (format nil "~A%" prepared-percentage)
                    +zero-pos+
                    :fill-color +black+
                    :font (gk:make-font :bold-pixel-operator 24)))
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 112 56)
      (draw-animation 'loading-screen-animation (now) +zero-pos+))))


(defmethod on-load ((this loading-screen))
  (with-slots (next-state) this
    (gk.fsm:transition-to next-state :pack (pack-of this))))
