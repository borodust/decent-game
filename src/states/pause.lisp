(cl:in-package :decent-game)


(defclass pause-screen (state-input-handler)
  ((world :initarg :world :initform (error ":world missing") :reader world-of)
   (pack :initarg :pack :initform (error ":pack missing"))
   (menu :initform nil)))


(defmethod initialize-instance :after ((this pause-screen) &key)
  (with-slots (menu pack world) this
    (flet ((%resume-stage ()
             (gk.fsm:transition-to 'stage-0 :world world :pack pack))
           (%restart-stage ()
             (gk.fsm:transition-to 'init-stage-0))
           (%main-menu ()
             (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                                   :next-state 'main-menu))
           (%exit ()
             (gk:stop)))
      (setf menu (make-instance 'menu :items (list "RESUME STAGE" #'%resume-stage
                                                   "RESTART STAGE" #'%restart-stage
                                                   "MAIN MENU" #'%main-menu
                                                   "EXIT" #'%exit))))))


;;; Input Handling
(defmethod gk.input:button-pressed ((this pause-screen) (key (eql :down)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:dpad-changed ((this pause-screen) (key (eql :down)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:button-pressed ((this pause-screen) (key (eql :j)))
  (with-slots (menu) this
    (select-next-menu-item menu)))


(defmethod gk.input:button-pressed ((this pause-screen) (key (eql :up)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:dpad-changed ((this pause-screen) (key (eql :up)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:button-pressed ((this pause-screen) (key (eql :k)))
  (with-slots (menu) this
    (select-prev-menu-item menu)))


(defmethod gk.input:button-pressed ((this pause-screen) (key (eql :enter)))
  (with-slots (menu) this
    (invoke-menu-item-action menu)))


;;; draw
(defmethod gk:draw ((this pause-screen))
  (with-slots (menu world) this
    (render world)
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 60 24)
      (gk:draw-rect +zero-pos+ 140 100
                    :fill-paint (alphacolor .8 +black+)
                    :stroke-paint (alphacolor .5 +color-alien-dark+)
                    :thickness 4)
      (gk:with-pushed-canvas ()
        (gk:translate-canvas 26 100)
        (render menu)))))
