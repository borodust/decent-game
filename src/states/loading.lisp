(cl:in-package :decent-game)


(gk:define-image :loading-screen-bar-overlay (asset-path "img/hud/loading-screen-bar-overlay.png"))

(define-resource-pack loading-screen-resources ()
  :loading-screen-bar-overlay)


(defclass loading-screen (base-loading-screen)
  ((next-state :initform (error ":next-state missing") :initarg :next-state)))


(defmethod gk:draw ((this loading-screen))
  (with-slots (prepared-percentage) this
    (let ((time (bodge-util:real-time-seconds)))
      (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+)
      (gk:with-pushed-canvas ()
        (gk:translate-canvas (+ 128 (* (sin time) 20))
                             (+ 72 (* (cos time) 20)))
        (gk:draw-circle +zero-pos+ (+ 5 (* (abs (* (sin time) (cos time))) 6)) :fill-paint (hexcolor "#CB0035")))
      (gk:with-pushed-canvas ()
        (draw-loading-bar prepared-percentage (gk:vec2 8 8) :bar-height 16 :bar-width 240)
        ;; (gk:draw-image +zero-pos+ :loading-screen-bar-overlay)
        )

      (gk:with-pushed-canvas ()
        (gk:translate-canvas 6 5)
        (gk:scale-canvas 0.5 0.5)
        (gk:draw-text (format nil "~A%" prepared-percentage)
                      +zero-pos+
                      :fill-color +black+)))))


(defmethod on-load ((this loading-screen))
  (with-slots (next-state) this
    (gk.fsm:transition-to next-state :pack (pack-of this))))
