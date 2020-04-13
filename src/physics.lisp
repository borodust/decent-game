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


(defun observe-universe (universe)
  (with-slots (universe) universe
    (b.phy:observe-universe universe 0.010)))


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


(defmethod provide-shape ((this box-body) body universe &key width height substance)
  (b.phy:make-box-shape universe
                        (/ width *universe-scale*) (/ height *universe-scale*)
                        :body body
                        :substance substance
                        :offset (b:vec2 (/ width 2 *universe-scale*)
                                        (/ height 2 *universe-scale*))))


(defun make-box-body (universe width height &key kinematic owner)
  (make-instance 'box-body :universe (%universe-of universe)
                           :width width
                           :height height
                           :substance owner
                           :kinematic kinematic
                           :allow-other-keys t))


(defmethod render ((this box-body))
  (with-slots (width height) this
    (let ((pos (body-position this)))
      (gk:draw-rect pos width height
                    :fill-paint *body-fill-color*
                    :stroke-paint *body-stroke-color*))))


;;;
;;; CIRCLE
;;;
(defclass circle-body (body)
  ((radius :initarg :radius)))


(defmethod provide-shape ((this circle-body) body universe &key radius substance)
  (b.phy:make-circle-shape universe (/ radius *universe-scale*)
                           :body body
                           :substance substance))


(defun make-circle-body (universe radius &key owner)
  (make-instance 'circle-body :universe (%universe-of universe)
                              :radius radius
                              :substance owner
                              :allow-other-keys t))


(defmethod render ((this circle-body))
  (with-slots (radius) this
    (let ((pos (body-position this)))
      (gk:draw-circle pos radius
                      :fill-paint *body-fill-color*
                      :stroke-paint *body-stroke-color*))))
