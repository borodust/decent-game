(cl:in-package :decent-game)


;;;
;;; OSD
;;;
(defclass gameplay-dialogue ()
  ((dialogue :initarg :dialogue)))


(defmethod gk:draw ((this gameplay-dialogue)))

;;;
;;; CUTSCENE
;;;
(defclass cutscene-dialogue ()
  ())


(defmethod gk:draw ((this cutscene-dialogue)))


;;;
;;; DIALOGUE
;;;
(defclass dialogue-screen (state-input-handler)
  ((dialogue :initform nil)
   (selected :initform 0)
   (box-font :initform (gk:make-font :bold-pixel-operator 8))
   (choice-font :initform (gk:make-font :pixel-operator 8))))


(defmethod initialize-instance :after ((this dialogue-screen) &key event)
  (with-slots (dialogue selected) this
    (setf dialogue event
          selected 0)))


(defun select-next-dialogue-choice (dialogue-screen)
  (with-slots (selected) dialogue-screen
    (incf selected)))


(defun select-prev-dialogue-choice (dialogue-screen)
  (with-slots (selected) dialogue-screen
    (decf selected)))


(defmethod gk:act ((this dialogue-screen))
  (with-slots (dialogue selected) this
    (let ((active-choice-count (loop for choice in (dialogue-choices dialogue)
                                     when (dialogue-choice-active-p dialogue choice)
                                       count choice)))
      (when (and (> active-choice-count 0)
                 (or (>= selected active-choice-count)
                     (< selected 0)))
        (setf selected (mod selected active-choice-count))))))


(defmethod gk:draw ((this dialogue-screen))
  (with-slots (dialogue selected box-font choice-font) this
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 10 94)
      (gk:draw-rect *zero-pos* 236 40 :stroke-paint *black*)
      (gk:translate-canvas 5 24)
      (draw-multiline-text (dialogue-text dialogue) *zero-pos*
                           :font box-font
                           :line-height 16))
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 75 75)
      (loop with i = 0
            for choice in (dialogue-choices dialogue)
            for text = (dialogue-choice-text dialogue choice)
            when (and text (dialogue-choice-active-p dialogue choice))
              do (when (eq selected i)
                   (gk:with-pushed-canvas ()
                     (gk:translate-canvas -20 0)
                     (gk:draw-text ">>" *zero-pos* :font box-font)))
                 (gk:draw-text text *zero-pos* :font choice-font)
                 (gk:translate-canvas 0 -10)
                 (incf i)))))


(defmethod gk.input:button-pressed ((this dialogue-screen) (key (eql :up)))
  (select-prev-dialogue-choice this))


(defmethod gk.input:button-pressed ((this dialogue-screen) (key (eql :down)))
  (select-next-dialogue-choice this))
