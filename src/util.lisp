(cl:in-package :decent-game)


(defclass state-input-handler (gk.input:input-handler) ())


(defvar *zero-pos* (gk:vec2 0 0))


(defvar *black* (gk:vec4 0 0 0 1))


(defmethod gk:post-initialize :around ((this state-input-handler))
  (gk.input:activate-input-handler this)
  (call-next-method))


(defmethod gk:pre-destroy :around ((this state-input-handler))
  (call-next-method)
  (gk.input:deactivate-input-handler this))


(defgeneric render (object)
  (:method (object) (declare (ignore object))))


(defmethod render :around (object)
  (gk:with-pushed-canvas ()
    (call-next-method)))


(defun shout (control &rest args)
  (format t "~&~A" (apply #'format nil control args))
  (finish-output t))


(defun draw-multiline-text (text position &key font line-height)
  (let ((total-height 0)
        (line-height (- (or line-height 0))))
    (gk:with-pushed-canvas ()
      (bodge-util:dolines (line text)
        (gk:draw-text line position :font font)
        (gk:translate-canvas 0 line-height)
        (incf total-height line-height)))
    total-height))
