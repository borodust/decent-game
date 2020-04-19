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
;;; SHAPE
;;;
(defclass shape (b:disposable)
  ((handle :initarg :handle :reader handle-of)
   (offset :initarg :offset)))


(b:define-destructor shape (handle)
  (b:dispose handle))


(defmethod dispose :after ((this shape))
  (b:dispose this))


(defclass box-shape (shape)
  ((width :initarg :width)
   (height :initarg :height)))


(defun make-box-shape (universe body width height substance offset radius)
  (let* ((w (/ width *universe-scale*))
         (h (/ height *universe-scale*))
         (offset (or offset +zero-pos+))
         (handle (b.phy:make-box-shape universe
                                       w h
                                       :body body
                                       :substance substance
                                       :radius radius
                                       :offset (b:vec2
                                                (/ (+ (/ width 2) (gk:x offset)) *universe-scale*)
                                                (/ (+ (/ height 2) (gk:y offset)) *universe-scale*)))))
    (make-instance 'box-shape
                   :handle handle
                   :width width
                   :height height
                   :offset offset)))


(defmethod render ((this box-shape) &key position color)
  (with-slots (offset width height) this
    (let ((pos (or position +zero-pos+)))
      (gk:translate-canvas (+ (gk:x offset) (gk:x pos))
                           (+ (gk:y offset) (gk:y pos)))
      (gk:draw-rect +zero-pos+ width height
                    :fill-paint (or color *body-fill-color*)))))


(defclass circle-shape (shape)
  ((radius :initarg :radius)))


(defun make-circle-shape (universe body radius substance offset)
  (let* ((r (/ radius *universe-scale*))
         (offset (or offset +zero-pos+))
         (handle (b.phy:make-circle-shape universe r
                                          :offset (b:div offset *universe-scale*)
                                          :body body
                                          :substance substance)))
    (make-instance 'circle-shape
                   :handle handle
                   :radius radius
                   :offset offset)))


(defmethod render ((this circle-shape) &key position color)
  (with-slots (offset radius) this
    (let ((pos (or position +zero-pos+)))
      (gk:translate-canvas (+ (gk:x offset) (gk:x pos))
                           (+ (gk:y offset) (gk:y pos)))
      (gk:draw-circle +zero-pos+ radius
                      :fill-paint (or color *body-fill-color*)))))

;;;
;;; BODY
;;;
(defclass body (b:disposable)
  ((universe :initarg :universe)
   (body :initform nil)
   (shapes :initform (list nil))))


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


(defun attach-box-shape (body width height &key offset sensor radius)
  (with-slots ((this-body body) shapes universe) body
    (let ((shape (make-box-shape universe this-body
                                 width height
                                 (or sensor body)
                                 offset
                                 radius)))
      (push shape (cdr shapes))
      shape)))


(defun attach-circle-shape (body radius &key offset sensor)
  (with-slots ((this-body body) shapes universe) body
    (let ((shape (make-circle-shape universe this-body radius (or sensor body) offset)))
      (push shape (cdr shapes))
      shape)))


(defun detach-shape (body shape)
  (with-slots (shapes) body
    (a:deletef (cdr shapes) shape)
    (dispose shape)))


(defmethod provide-body ((this body) universe &key kinematic)
  (if kinematic
      (b.phy:make-kinematic-body universe)
      (b.phy:make-rigid-body universe)))


(defmethod initialize-instance :after ((this body) &rest args &key universe)
  (unless universe
    (error ":universe missing"))
  (with-slots (body shapes) this
    (setf body (apply #'provide-body this universe args))
    (push (apply #'provide-shape this body universe args) (cdr shapes))))


(b:define-destructor body (body shapes)
  (loop for shape in (rest shapes)
        do (dispose shape))
  (b:dispose body))


(defmethod dispose :after ((this body))
  (b:dispose this))


(defmethod render ((this body) &key color)
  (with-slots (shapes) this
    (when *debug-rendering*
      (let ((pos (body-position this)))
        (loop for shape in (rest shapes)
              do (render shape :position pos :color color))))))

;;;
;;; BOX
;;;
(defclass box-body (body) ())


(defmethod provide-shape ((this box-body) body universe &key width height substance mass offset radius)
  (let ((shape (make-box-shape universe
                               body
                               width height
                               substance
                               offset
                               radius)))
    (when mass
      (b.phy:infuse-box-mass body mass (/ width *universe-scale*) (/ height *universe-scale*)))
    shape))


(defun make-box-body (universe width height &key kinematic owner mass)
  (make-instance 'box-body :universe (%universe-of universe)
                           :width width
                           :height height
                           :substance owner
                           :mass mass
                           :kinematic kinematic
                           :allow-other-keys t))


;;;
;;; CIRCLE
;;;
(defclass circle-body (body) ())


(defmethod provide-shape ((this circle-body) body universe &key radius substance mass offset)
  (let* ((r (/ radius *universe-scale*))
         (shape (make-circle-shape universe body radius (or substance this) offset)))
    (when mass
      (b.phy:infuse-circle-mass body mass r))
    shape))


(defun make-circle-body (universe radius &key owner mass offset)
  (make-instance 'circle-body :universe (%universe-of universe)
                              :radius radius
                              :substance owner
                              :mass mass
                              :offset offset
                              :allow-other-keys t))
