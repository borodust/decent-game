(cl:in-package :decent-game)


(define-resource-pack main-menu-resources (font-resources)
  :menu-theme)


(defclass main-menu (state-input-handler)
  ((menu :initform nil)))


(defmethod initialize-instance :after ((this main-menu) &key pack)
  (declare (ignore pack))
  (with-slots (menu) this
    (flet ((%gameplay-debug-screen ()
             (gk.fsm:transition-to 'init-gameplay-debug-screen))
           (%animation-debug-screen ()
             (gk.fsm:transition-to 'init-animation-debug-screen))
           (%dialogue-debug-screen ()
             (gk.fsm:transition-to 'dialogue-debug-screen))
           (%loading-screen ()
             (gk.fsm:transition-to 'init-screen))
           (%exit ()
             (gk:stop)))
      (setf menu (make-instance 'menu :items (list "DEBUG-GAMEPLAY" #'%gameplay-debug-screen
                                                   "DEBUG-ANIMATION" #'%animation-debug-screen
                                                   "DEBUG-DIALOGUE" #'%dialogue-debug-screen
                                                   "LOADING SCREEN" #'%loading-screen
                                                   "EXIT" #'%exit))))))


(defmethod gk:post-initialize ((this main-menu))
  (gk:play-sound :menu-theme :looped-p t))


(defmethod gk:pre-destroy ((this main-menu))
  (gk:stop-sound :menu-theme))


;;; Input Handling
(defmethod gk.input:button-pressed ((this main-menu) (key (eql :down)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:dpad-changed ((this main-menu) (key (eql :down)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :j)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :up)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:dpad-changed ((this main-menu) (key (eql :up)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))

(defmethod gk.input:button-pressed ((this main-menu) (key (eql :k)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :gamepad-a)))
  (with-slots (menu) this
    (invoke-menu-item-action menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :enter)))
  (with-slots (menu) this
    (invoke-menu-item-action menu)))


;;; draw
(defmethod gk:draw ((this main-menu))
  (with-slots (menu) this
    (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+)
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 60 140)
      (render menu))))
