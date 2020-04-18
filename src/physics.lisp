(cl:in-package :decent-game)


(defparameter *universe-scale* 10)


(defparameter *body-fill-color* (gk:vec4 1 0 0 0.5))
(defparameter *body-stroke-color* (gk:vec4 1 0 0 0.8))


;;;
;;; UNIVERSE
;;;
(defgeneric collide (this that)
  (:method (this that)
    (declare (ignore this that))
    t))

(defgeneric process-collision (this that)
  (:method (this that) (declare (ignore this that))))


(defclass universe (b:disposable)
  ((universe :initform nil :reader %universe-of)))


(defmethod initialize-instance :after ((this universe) &key gravity)
  (with-slots (universe) this
    (flet ((%collide (this that)
             (collide (b.phy:shape-substance this) (b.phy:shape-substance that)))
           (%process-collision (this that)
             (process-collision (b.phy:shape-substance this) (b.phy:shape-substance that))))
      (setf universe (b.phy:make-universe :2d
                                          :on-pre-solve #'%collide
                                          :on-post-solve #'%process-collision)
            (b.phy:gravity universe) (b:div gravity *universe-scale*)))))


(b:define-destructor universe (universe)
  (b:dispose universe))


(defmethod dispose :after ((this universe))
  (b:dispose this))


(defun make-universe (gravity)
  (make-instance 'universe :gravity gravity))


(defun observe-universe (universe step &optional (repeats 1))
  (with-slots (universe) universe
    (loop repeat repeats
          do (b.phy:observe-universe universe step))))


;;;
;;; BODY
;;;
(defclass body (b:disposable)
  ((body :initform nil)
   (shape :initform nil)))


(defgeneric provide-body (object universe &key &allow-other-keys))
(defgeneric provide-shape (object body universe &key &allow-other-keys))


(defun body-position (body)
  (with-slots (body) body
    (b:mult (b.phy:body-position body) *universe-scale*)))


(defun (setf body-position) (value body)
  (with-slots (body) body
    (setf (b.phy:body-position body) (b:div value *universe-scale*))))


(defun body-linear-velocity (body)
  (with-slots (body) body
    (b:mult (b.phy:body-linear-velocity body) *universe-scale*)))


(defun (setf body-linear-velocity) (value body)
  (with-slots (body) body
    (setf (b.phy:body-linear-velocity body) (b:div value *universe-scale*))))


(defun body-angular-velocity (body)
  (with-slots (body) body
    (b:mult (b.phy:body-linear-velocity body) *universe-scale*)))


(defun (setf body-angular-velocity) (value body)
  (with-slots (body) body
    (setf (b.phy:body-angular-velocity body) (/ value *universe-scale*))))


(defun apply-force (body force)
  (with-slots (body) body
    (b.phy:apply-force body (b:div force *universe-scale*))))


(defun apply-torque (body force)
  (with-slots (body) body
    (b.phy:apply-torque body (/ force *universe-scale*))))


(defun (setf collision-surface-velocity) (value)
  (setf (b.phy:collision-surface-velocity) (b:div value *universe-scale*)))


(defun (setf collision-friction) (value)
  (setf (b.phy:collision-friction) (/ value *universe-scale*)))


(defmethod provide-body ((this body) universe &key kinematic)
  (if kinematic
      (b.phy:make-kinematic-body universe)
      (b.phy:make-rigid-body universe)))


(defmethod initialize-instance :after ((this body) &rest args &key universe)
  (unless universe
    (error ":universe missing"))
  (with-slots (body shape) this
    (setf body (apply #'provide-body this universe args)
          shape (apply #'provide-shape this body universe args))))


(b:define-destructor body (body shape)
  (b:dispose shape)
  (b:dispose body))


(defmethod dispose :after ((this body))
  (b:dispose this))


;;;
;;; BOX
;;;
(defclass box-body (body)
  ((width :initarg :width)
   (height :initarg :height)))


(defmethod provide-shape ((this box-body) body universe &key width height substance mass)
  (let* ((w (/ width *universe-scale*))
         (h (/ height *universe-scale*))
         (shape (b.phy:make-box-shape universe
                                      w h
                                      :body body
                                      :substance substance
                                      :offset (b:vec2 (/ width 2 *universe-scale*)
                                                      (/ height 2 *universe-scale*)))))
    (when mass
      (b.phy:infuse-box-mass body mass w h))
    shape))


(defun make-box-body (universe width height &key kinematic owner mass)
  (make-instance 'box-body :universe (%universe-of universe)
                           :width width
                           :height height
                           :substance owner
                           :mass mass
                           :kinematic kinematic
                           :allow-other-keys t))


(defmethod render ((this box-body) &key color)
  (with-slots (width height) this
    (when *debug-rendering*
      (let ((pos (body-position this)))
        (gk:draw-rect pos width height
                      :fill-paint (or color *body-fill-color*))))))


;;;
;;; CIRCLE
;;;
(defclass circle-body (body)
  ((radius :initarg :radius)))


(defmethod provide-shape ((this circle-body) body universe &key radius substance mass)
  (let* ((r (/ radius *universe-scale*))
         (shape (b.phy:make-circle-shape universe r
                                         :body body
                                         :substance substance)))
    (when mass
      (b.phy:infuse-circle-mass body mass r))
    shape))


(defun make-circle-body (universe radius &key owner mass)
  (make-instance 'circle-body :universe (%universe-of universe)
                              :radius radius
                              :substance owner
                              :mass mass
                              :allow-other-keys t))


(defmethod render ((this circle-body) &key)
  (with-slots (radius) this
    (when *debug-rendering*
      (let ((pos (body-position this)))
        (gk:draw-circle pos radius
                        :fill-paint *body-fill-color*)))))
