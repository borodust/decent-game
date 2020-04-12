(cl:in-package :decent-game)


(defclass main-menu (state-input-handler)
  ((menu :initform nil)))


(defmethod initialize-instance :after ((this main-menu) &key)
  (with-slots (menu) this
    (flet ((%loading-screen ()
             (gk.fsm:transition-to 'loading-screen :resources nil :next-state 'main-menu))
           (%exit ()
             (gk:stop)))
      (setf menu (make-instance 'menu :items (list "LOADING SCREEN" #'%loading-screen
                                                   "EXIT" #'%exit))))))


(defmethod gk:post-initialize ((this main-menu))
  (gk:play-sound :menu-theme :looped-p t))


(defmethod gk:pre-destroy ((this main-menu))
  (gk:stop-sound :menu-theme))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :down)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:dpad-changed ((this main-menu) (key (eql :down)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :up)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:dpad-changed ((this main-menu) (key (eql :up)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :gamepad-a)))
  (with-slots (menu) this
    (invoke-menu-item-action menu)))


(defmethod gk.input:button-pressed ((this main-menu) (key (eql :enter)))
  (with-slots (menu) this
    (invoke-menu-item-action menu)))


(defmethod gk:draw ((this main-menu))
  (with-slots (menu) this
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 80 60)
      (render menu))))
