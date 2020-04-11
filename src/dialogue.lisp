(cl:in-package :decent-game)


(defvar *dialogues* nil)

(defclass dialogue ()
  ((type :initform :osd)
   (image :initform nil)
   (choices :initform nil)
   (text :initarg :text)))


(defclass dialogue-choice ()
  ((test :initarg :test)
   (text :initarg :text)))


(defgeneric make-event-dialogue (event))


(defmacro defdialogue (event-and-opts &body text)
  (a:with-gensyms (this-event-type dialogue)
    (flet ((%generate-setter (slot value)
             `(setf (slot-value ,dialogue ',slot) ,value))
           (%generate-choices (definition)
             `(list ,@(loop for (choice . text) on definition by #'cddr
                            collect (destructuring-bind (event &key test) (a:ensure-list choice)
                                      `(cons ,event (make-instance 'dialogue-choice
                                                                   :test ,test
                                                                   :text ,(first text))))))))
      (destructuring-bind (event &rest opts) (a:ensure-list event-and-opts)
        `(progn
           (defmethod make-event-dialogue ((,this-event-type (eql ,event)))
             (declare (ignore ,this-event-type))
             (let ((,dialogue (make-instance 'dialogue :text ,@text)))
               ,@(loop for (opt . values) in opts
                       for setter = (ecase opt
                                      (:type (%generate-setter 'type (first values)))
                                      (:image (%generate-setter 'image (first values)))
                                      (:choices (%generate-setter 'choices (%generate-choices values))))
                       when setter
                         collect setter)
               ,dialogue))
           (pushnew ',event *dialogues*))))))
