(cl:in-package :decent-game)


(defclass base-loading-screen (state-input-handler)
  ((pack-name :initarg :pack :initform (error ":pack missing"))
   (pack :initform nil)
   (next-state :initform (error ":next-state missing") :initarg :next-state)
   (prepared-percentage :initform 100 ; 100 is the default, because if no resources are specified, there's nothing to load
                        :documentation "Indicated what percentage of the resources has already benn loaded."))
  (:documentation "doc"))


(defmethod gk:post-initialize ((this base-loading-screen))
  (with-slots (pack-name pack) this
    (unless pack-name
      (error ":pack-name must not be nil"))
    (setf pack (load-resource-pack pack-name))))


(defmethod gk:act ((this base-loading-screen))
  (with-slots (pack next-state prepared-percentage) this
    (let ((count (pack-prepared-count pack))
          (total (pack-total-count pack)))
      (setf prepared-percentage (truncate (* (if (> total 0)
                                                 (/ count total)
                                                 1)
                                             100)))
      (when (= count total)
        (gk.fsm:transition-to next-state :pack pack)))))
