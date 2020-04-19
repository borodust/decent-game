(cl:in-package :decent-game)


(defvar *event-table* (make-hash-table))


(defun subscribe-to-event (event-id action)
  (pushnew action (gethash event-id *event-table*)))


(defun unsubscribe-from-event (event-id action)
  (a:deletef (gethash event-id *event-table*) action))


(defun trigger-event (event-id &rest args &key &allow-other-keys)
  (loop for action in (gethash event-id *event-table*)
        do (etypecase action
             (symbol (apply (symbol-function action) event-id :allow-other-keys t args))
             (function (apply action event-id :allow-other-keys t args)))))
