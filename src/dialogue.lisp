(cl:in-package :decent-game)

(defgeneric dialogue-type (event))
(defgeneric dialogue-image (event))
(defgeneric dialogue-choices (event))
(defgeneric dialogue-text (event))

(defgeneric dialogue-chioce-id (event choice))
(defgeneric dialogue-chioce-test (event choice))
(defgeneric dialogue-chioce-text (event choice))


(defun dialogue-choice-active-p (event choice)
  (or (null (dialogue-choice-test event choice))
      (funcall (dialogue-choice-test event choice))))


(defmacro defdialogue (event-and-opts &body text)
  (a:with-gensyms (this-event-type this-choice-type choices)
    (destructuring-bind (event &rest opts) (a:ensure-list event-and-opts)
      (labels ((%generate-method (name value)
                 `(defmethod ,name ((,this-event-type (eql ,event)))
                    (declare (ignore ,this-event-type))
                    ,@value))
               (%generate-choice-list (definition)
                 `(list ,@(loop for (choice . text) on definition by #'cddr
                                collect (destructuring-bind (event &rest values) (a:ensure-list choice)
                                          (declare (ignore values))
                                          event))))
               (%generate-choice-method (name choice value)
                 `(defmethod ,name ((,this-event-type (eql ,event))
                                    (,this-choice-type (eql ,choice)))
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
                   collect method))))))
