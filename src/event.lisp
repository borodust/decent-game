(cl:in-package :decent-game)


(defvar *event-table* (make-hash-table))


(defun subscribe-to-event (event-id action)
  (pushnew action (gethash *event-table* event-id)))


(defun trigger-event (event-id)
  (loop for action in (gethash *event-table* event-id)
        do (etypecase action
             (symbol (funcall (symbol-function action)))
             (function (funcall action)))))
