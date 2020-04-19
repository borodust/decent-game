(cl:in-package :decent-game)


(defclass init-screen (base-loading-screen) ()
  (:default-initargs
   :pack-name nil))


(defmethod gk:post-initialize ((this init-screen))
  (with-slots (pack-name pack) this
    (setf pack (load-resource-pack pack-name))
    ;; (unless pack-name
    ;;   (error ":pack-name must not be nil"))
    )

  (gk.fsm:transition-to 'loading-screen
                        :pack 'main-menu-resources
                        :next-state 'main-menu))


(defmethod gk:draw ((this init-screen))
  (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+))
