(cl:in-package :decent-game)


(defgeneric dialogue-type (event))
(defgeneric dialogue-image (event))
(defgeneric dialogue-choices (event))
(defgeneric dialogue-text (event))

(defgeneric dialogue-choice-id (event choice))
(defgeneric dialogue-choice-test (event choice))
(defgeneric dialogue-choice-text (event choice))


(defgeneric trigger-dialogue (event-id &key &allow-other-keys))


(defun dialogue-choice-active-p (event choice)
  (or (null (dialogue-choice-test event choice))
      (funcall (dialogue-choice-test event choice))))


(defmacro defdialogue (event-and-opts &body text)
  (a:with-gensyms (this-event-type this-choice-type choices)
    (destructuring-bind (event &rest opts) (a:ensure-list event-and-opts)
      (labels ((%generate-method (name value)
                 `(defmethod ,name ((,this-event-type (eql ',event)))
                    (declare (ignore ,this-event-type))
                    ,@value))
               (%generate-choice-list (definition)
                 `(list ,@(loop for (choice . text) on definition by #'cddr
                                collect (destructuring-bind (event &rest values) (a:ensure-list choice)
                                          (declare (ignore values))
                                          event))))
               (%generate-choice-method (name choice value)
                 `(defmethod ,name ((,this-event-type (eql ',event))
                                    (,this-choice-type (eql ',choice)))
                    (declare (ignore ,this-event-type ,this-choice-type))
                    ,@value))
               (%generate-choice-methods (definition)
                 (loop for (choice . text) on definition by #'cddr
                       append (destructuring-bind (event &key test) (a:ensure-list choice)
                                `(,(%generate-choice-method 'dialogue-choice-id event (list event))
                                  ,(%generate-choice-method 'dialogue-choice-test event (list test))
                                  ,(%generate-choice-method 'dialogue-choice-text event
                                                            (list (first text))))))))
        `(progn
           ,(%generate-method 'dialogue-text text)
           ,@(loop for (opt . values) in opts
                   for method = (ecase opt
                                  (:type (%generate-method 'dialogue-kind (list (first values))))
                                  (:image (%generate-method 'dialogue-image (list (first values))))
                                  (:choices `(progn
                                               (let ((,choices ,(%generate-choice-list values)))
                                                 ,(%generate-method 'dialogue-choices (list choices)))
                                               ,@(%generate-choice-methods values))))
                   collect method)
           (defmethod trigger-dialogue ((,this-event-type (eql ',event)) &key)
             (declare (ignore ,this-event-type))
             (gk.fsm:transition-to 'dialogue-screen :event ,event))
           (subscribe-to-event ,event 'trigger-dialogue))))))


(defclass dialogue ()
  ((dialogue :initform nil)
   (selected :initform 0)
   (box-font :initform (gk:make-font 'bold-pixel-operator 8))
   (choice-font :initform (gk:make-font 'pixel-operator 8))))


(defmethod initialize-instance :after ((this dialogue) &key event)
  (with-slots (dialogue selected) this
    (setf dialogue event
          selected 0)))


(defun make-dialogue (event)
  (make-instance 'dialogue :event event))


(defun select-next-dialogue-choice (dialogue)
  (with-slots (selected) dialogue
    (incf selected)))


(defun select-prev-dialogue-choice (dialogue)
  (with-slots (selected) dialogue
    (decf selected)))


(defun invoke-dialogue-choice (dialogue)
  (with-slots (dialogue selected) dialogue
    (loop with i = 0
          for choice in (dialogue-choices dialogue)
          if (and (= selected i)
                  (dialogue-choice-active-p dialogue choice))
            return (trigger-event choice)
          else
            do (incf i))))


(defun maintain-dialogue (dialogue)
  (with-slots (dialogue selected) dialogue
    (let ((active-choice-count (loop for choice in (dialogue-choices dialogue)
                                     when (dialogue-choice-active-p dialogue choice)
                                       count choice)))
      (when (and (> active-choice-count 0)
                 (or (>= selected active-choice-count)
                     (< selected 0)))
        (setf selected (mod selected active-choice-count))))))


(defmethod render ((this dialogue) &key)
  (with-slots (dialogue selected box-font choice-font) this
    (gk:with-pushed-canvas ()
      (gk:translate-canvas 10 94)
      (gk:draw-rect +zero-pos+ 236 40 :stroke-paint +black+)
      (gk:translate-canvas 5 24)
      (draw-multiline-text (dialogue-text dialogue) +zero-pos+
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
                     (gk:draw-text ">>" +zero-pos+ :font box-font)))
                 (let ((height
                         (draw-multiline-text text +zero-pos+
                                              :font choice-font
                                              :line-height 14)))
                   (gk:translate-canvas 0 height))
                 (incf i)))))
