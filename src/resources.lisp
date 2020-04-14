(cl:in-package :decent-game)


(defun asset-path (pathname)
  (asdf:system-relative-pathname :decent-game (merge-pathnames pathname "assets/")))


(gk:define-font :bold-pixel-operator (asset-path "fnt/pixel-operator/PixelOperatorMono8-Bold.ttf"))
(gk:define-font :pixel-operator (asset-path "fnt/pixel-operator/PixelOperatorMono8.ttf"))

(gk:define-sound :menu-theme (asset-path "snd/intro.ogg"))


(defvar *pack-table* (make-hash-table))
(defvar *resource-table* (make-hash-table))


(defun register-resource (name)
  (setf (gethash name *resource-table*) t))

(defun remove-resource (name)
  (remhash name *resource-table*))

(defun resource-loaded-p (name)
  (gethash name *resource-table*))


(defclass resource-pack ()
  ((prepared :initform nil)
   (count :initform 0)
   (total :initform 0)))


(defgeneric resource-pack-resources (pack)
  (:method-combination append))


(defun filter-resource-pack-resources (pack)
  (remove-duplicates (resource-pack-resources pack)))


(defun pack-resource-prepared-p (pack resource)
  (with-slots (prepared) pack
    (member resource prepared)))


(defun notice-pack-resource (pack resource)
  (with-slots (prepared count) pack
    (let ((resources (filter-resource-pack-resources pack)))
      (when (and (member resource resources)
                 (not (member resource prepared)))
        (push resource prepared)
        (incf count)))))


(defmethod initialize-instance :after ((this resource-pack) &key)
  (with-slots (prepared total count) this
    (let ((resources (filter-resource-pack-resources this)))
      (setf total (length resources))
      (loop for resource in resources
            do (let ((packs (push this (gethash resource *pack-table*))))
                 (unless (second packs)
                   (gk:prepare-resources resource)))
               (when (resource-loaded-p resource)
                 (notice-pack-resource this resource))))))


(defun load-resource-pack (name)
  (if (null name)
      (format t "~&Can't load resource-pack with class-name of ~A.~% IGNORED" name)
      (make-instance name)))


(defun pack-prepared-p (pack)
  (with-slots (count total) pack
    (= count total)))


(defun pack-total-count (pack)
  (with-slots (total) pack
    total))


(defun pack-prepared-count (pack)
  (with-slots (count) pack
    count))


(defun dispose-resource-pack (pack)
  (let ((resources (filter-resource-pack-resources pack)))
    (loop for name in resources
          unless (a:deletef (gethash name *pack-table*) pack)
            do (remove-resource name)
               (gk:dispose-resources name)
               (remhash name *pack-table*))))


(defun notice-pack-resources (&rest names)
  (loop for name in names
        do (register-resource name)
           (loop for pack in (gethash name *pack-table*)
                 do (notice-pack-resource pack name))))


(defun dispose-pack-resources ()
  (apply #'gk:dispose-resources
         (loop for resource being the hash-key of *pack-table*
               collect resource))
  (clrhash *pack-table*)
  (clrhash *resource-table*))


(defmacro define-resource-pack (name (&rest parent-classes) &body resources)
  (a:with-gensyms (this resource-list)
    `(progn
       (defclass ,name (,@(if parent-classes
                              parent-classes
                              (list 'resource-pack)))
         ())
       (let ((,resource-list (list ,@resources)))
         (defmethod resource-pack-resources append ((,this ,name))
           (declare (ignore ,this))
           ,resource-list)))))
