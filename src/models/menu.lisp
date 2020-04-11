(cl:in-package :decent-game)


(defparameter *menu-item-selector-pos* (gk:vec2 -20 0))


(defclass menu ()
  ((items :initform nil)
   (selected :initform nil)))


(defmethod initialize-instance :after ((this menu) &key items)
  (with-slots (selected (this-items items)) this
    (unless items
      (error ":items should not be nil"))
    (let* ((item-alist (a:plist-alist items))
           (len (length item-alist)))
      (setf this-items (make-array len :initial-contents item-alist)
            selected 0))))


(defun select-next-menu-item (menu)
  (with-slots (selected items) menu
    (setf selected (mod (1+ selected) (length items)))))


(defun select-prev-menu-item (menu)
  (with-slots (selected items) menu
    (setf selected (mod (1- selected) (length items)))))


(defun invoke-menu-item-action (menu)
  (with-slots (selected items) menu
    (funcall (cdr (aref items selected)))))


(defmethod render ((this menu))
  (with-slots (selected items) this
    (loop for (name . action) across items
          for i from 0
          do (gk:translate-canvas 0 -20)
             (when (= selected i)
               (gk:draw-text ">>" *menu-item-selector-pos*))
             (gk:draw-text name *zero-pos*))))
