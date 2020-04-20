(cl:in-package :decent-game)

(defparameter *credits-pos* (gk:vec2 16 124))

(defclass credits-screen (state-input-handler)
  ((credits-finished-p :initform nil :accessor credits-finished-p)))


(defmethod gk:act ((this credits-screen))
  (cond ((and (> (gk:y *credits-pos*) 420)
              (not (credits-finished-p this)))
         (setf (credits-finished-p this) t))
        ((not (credits-finished-p this))
         (incf (gk:y *credits-pos*) .25))
        (t nil)))

(defmethod gk:post-initialize ((this credits-screen))
  (setf *credits-pos* (gk:vec2 16 124)))

(defmethod gk:draw ((this credits-screen))
  (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+)
  (draw-multiline-text "Copyright (C) 2020

  decent-username,
  Ryan Burnside,
  David O'Toole,
  readeval,
  Pavel Korolev.

Game Director : Pavel Korolev

Development & Engine Director :
 Pavel Korolev

Graphics & Level Director :
 decent-username

Concept Art & Design :
 Ryan Burnside

Assistant Producer and Story :
 readeval

Music Director:
 readeval

Original Music Score by:
 DJXXSS, Tomorrow Mars

Additional Music licensed from:
 Austin Coco

Additional testing by:
 Plastson"
                       *credits-pos*
                       :font (gk:make-font 'pixel-operator 6)
                       :line-height 8
                       :fill-color +color-alien-dark+))


(defmethod gk.input:button-pressed ((this credits-screen) (key (eql :down)))
  (scroll-down this))


(defmethod gk.input:dpad-changed ((this credits-screen) (key (eql :down)))
  (scroll-down this))


(defmethod gk.input:button-pressed ((this credits-screen) (key (eql :j)))
  (scroll-down this))

(defmethod gk.input:button-pressed ((this credits-screen) (key (eql :k)))
  (scroll-up this))


(defmethod gk.input:button-pressed ((this credits-screen) (key (eql :up)))
  (scroll-up this))


(defmethod gk.input:dpad-changed ((this credits-screen) (key (eql :up)))
  (scroll-up this))


(defmethod gk.input:button-pressed ((this credits-screen) (key (eql :escape)))
  (gk.fsm:transition-to 'main-menu))

(defmethod scroll-up ((this credits-screen))
  (decf (gk:y *credits-pos*) 5))


(defmethod scroll-down ((this credits-screen))
  (incf (gk:y *credits-pos*) 5))
