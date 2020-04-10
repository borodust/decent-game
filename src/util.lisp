(cl:in-package :decent-game)


(defclass state-input-handler (gk.input:input-handler) ())


(defvar *zero-pos* (gk:vec2 0 0))


(defmethod gk:post-initialize :around ((this state-input-handler))
  (gk.input:activate-input-handler this)
  (call-next-method))


(defmethod gk:pre-destroy :around ((this state-input-handler))
  (call-next-method)
  (gk.input:deactivate-input-handler this))
