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
  ((dialogue :initform nil)))


(defmethod initialize-instance :after ((this dialogue-screen) &key event)
  (with-slots (dialogue) this
    (unless event
      (error ":event missing"))
    (setf dialogue (make-dialogue event))))


(defmethod gk:act ((this dialogue-screen))
  (with-slots (dialogue) this
    (maintain-dialogue dialogue)))


(defmethod gk:draw ((this dialogue-screen))
  (with-slots (dialogue) this
    (render dialogue)))


(defmethod gk.input:button-pressed ((this dialogue-screen) (key (eql :up)))
  (with-slots (dialogue) this
    (select-prev-dialogue-choice dialogue)))


(defmethod gk.input:button-pressed ((this dialogue-screen) (key (eql :down)))
  (with-slots (dialogue) this
    (select-next-dialogue-choice dialogue)))


(defmethod gk.input:button-pressed ((this dialogue-screen) (key (eql :enter)))
  (with-slots (dialogue) this
    (invoke-dialogue-choice dialogue)))
