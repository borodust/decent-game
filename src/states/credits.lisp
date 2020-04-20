(cl:in-package :decent-game)

(defvar *credits-pos* (gk:vec2 32 124))

(defclass credits-screen (state-input-handler)
  ())


;; Copyright (C) 2020 decent-username, Ryan Burnside, David O'Toole,                        readeval, Pavel Korolev.

;; Game Director : Pavel Korolev

;; Development & Engine Director : Pavel Korolev

;; Graphics & Level Director : decent-username

;; Concept Art & Design : Ryan Burnside

;; Assistant Producer and Story : readeval

;; Music Director: readeval

;; Original Music Score by: DJXXSS, Tomorrow Mars

;; Additional Music licensed from: Austin Coco

;; Additional testing by: Plastson


(defmethod gk:act ((this credits-screen)))


(defmethod gk:draw ((this credits-screen))
  (gk:draw-rect +zero-pos+ 256 144 :fill-paint +black+)
  (draw-multiline-text "some text \n with\n new\nlines"
                       *credits-pos*
                       :font (gk:make-font 'pixel-operator 12)
                       :line-height 16
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



(defmethod scroll-up ((this credits-screen)))
(defmethod scroll-down ((this credits-screen)))
