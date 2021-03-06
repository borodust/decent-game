(cl:in-package :decent-game)

(gk:define-sound menu-theme (asset-path "snd/music/intro.wav"))
(gk:define-sound credits-tune (asset-path "snd/music/Hip2BCube_-_P_10.ogg"))


(define-resource-pack main-menu-resources (font-resources)
  'menu-theme
  'credits-tune)


(defclass main-menu (state-input-handler)
  ((menu :initform nil)))


(defmethod initialize-instance :after ((this main-menu) &key pack)
  (declare (ignore pack))
  (with-slots (menu) this
    (flet ((%start-game ()
             (gk.fsm:transition-to 'init-stage-0))
           ;; (%gameplay-debug-screen ()
           ;;   (gk.fsm:transition-to 'init-gameplay-debug-screen))
           ;; (%animation-debug-screen ()
           ;;   (gk.fsm:transition-to 'init-animation-debug-screen))
           ;; (%dialogue-debug-screen ()
           ;;   (gk.fsm:transition-to 'dialogue-debug-screen))
           (%credits-screen ()
             (gk.fsm:transition-to 'credits-screen))
           (%exit ()
             (gk:stop)))
      (setf menu (make-instance 'menu :items (list "START GAME" #'%start-game
                                                   ;; "DEBUG-GAMEPLAY" #'%gameplay-debug-screen
                                                   ;; "DEBUG-ANIMATION" #'%animation-debug-screen
                                                   ;; "DEBUG-DIALOGUE" #'%dialogue-debug-screen
                                                   "CREDITS" #'%credits-screen
                                                   "EXIT" #'%exit))))))


(defmethod gk:post-initialize ((this main-menu))
  (gk:play-sound 'menu-theme :looped-p t))


(defmethod gk:pre-destroy ((this main-menu))
  (gk:stop-sound 'menu-theme))


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
      (gk:translate-canvas 90 100)
      (render menu))))
