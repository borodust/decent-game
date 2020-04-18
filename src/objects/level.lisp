(cl:in-package :decent-game)


(declaim (special *level-width*
                  *level-height*
                  *image*
                  *tile-width*
                  *tile-height*
                  *tile-map*
                  *image-map*
                  *level-universe*))


(defgeneric level-image-map (level))
(defgeneric level-resources (level))


(defmacro define-level (name path &body images)
  (a:with-gensyms (image-map-var level-resources)
    (let (image-defines image-map image-resources)
      (loop for image-decl in images
            do (destructuring-bind (image-name image-path image-id)
                   image-decl
                 (push image-name image-resources)
                 (push `(gk:define-image ,image-name ,image-path
                          :use-nearest-interpolation t)
                       image-defines)
                 (push `(,image-id ',image-name) image-map)))
      `(progn
         (gk:define-text ,name ,path)
         ,@image-defines
         (let ((,image-map-var (a:plist-hash-table
                                (list ,@(reduce #'append image-map))
                                :test 'equal)))
           (defmethod level-image-map ((this (eql ',name)))
             (declare (ignore this))
             ,image-map-var))
         (let ((,level-resources '(,name ,@image-resources)))
           (defmethod level-resources ((this (eql ',name)))
             ,level-resources))))))


;;;
;;; TILE
;;;
(defclass tile ()
  ((image :initarg :image)
   (origin :initarg :origin)
   (width :initarg :width)
   (height :initarg :height)))


(defmethod render ((this tile) &key)
  (with-slots (image origin width height) this
    (gk:draw-image +zero-pos+ image :origin origin
                                    :width width
                                    :height height)))


(defun find-tile (gid)
  (gethash gid *tile-map*))

;;;
;;; CONTROL
;;;
(defclass control () ())


(defmethod collide :around ((this control) (that control))
  nil)


;;;
;;; SENSOR
;;;
(defclass sensor (control)
  ((body :initform nil)
   (fired :initform nil)
   (event :initarg :event)))


(defmethod initialize-instance :after ((this sensor) &key position width height)
  (with-slots (body) this
    (setf body (make-box-body *level-universe* width height :kinematic t :owner this)
          (body-position body) position)))


(defun trigger-sensor-event (sensor)
  (with-slots (fired event) sensor
    (unless fired
      (setf fired t)
      (trigger-event event))))


(defmethod collide :after ((this sensor) that)
  (trigger-sensor-event this))


(defmethod collide :around ((this sensor) that)
  (call-next-method)
  nil)


(defmethod collide :around (that (this sensor))
  (collide this that))


(defmethod dispose :after ((this sensor))
  (with-slots (body) this
    (dispose body)))


;;;
;;; OBSTACLE
;;;
(defclass obstacle (control)
  ((body :initform nil)))


(defmethod dispose :after ((this obstacle))
  (with-slots (body) this
    (dispose body)))


(defmethod render ((this obstacle) &key)
  (with-slots (body) this
    (render body)))


(defmethod collide ((this obstacle) (that obstacle))
  nil)


(defclass platform (obstacle)
  ((position :initarg :position)))


(defclass rectangle-platform (platform)
  ((width :initarg :width)
   (height :initarg :height)))


(defmethod initialize-instance :after ((this rectangle-platform) &key)
  (with-slots (body width height position) this
    (setf body (make-box-body *level-universe* width height :kinematic t :owner this)
          (body-position body) position)))


(defclass polygon-platform (platform)
  ((point-offsets :initarg :offsets)))


;;;
;;; CONTROL-PLANE
;;;
(defgeneric player-spawn-position-of (object))
(defgeneric find-enemy-spawn (object name))

(defclass control-plane ()
  ((player-spawn :initarg :player-spawn :reader player-spawn-position-of)
   (enemy-spawn-table :initarg :enemy-spawn-table)
   (platforms :initarg :platforms)
   (sensors :initarg :sensors)))


(defmethod dispose :after ((this control-plane))
  (with-slots (platforms sensors) this
    (loop for platform in platforms
          do (dispose platform))
    (loop for object in sensors
          do (dispose object))))


(defmethod render ((this control-plane) &key)
  (with-slots (platforms) this
    (loop for platform in platforms
          do (render platform))))


(defmethod find-enemy-spawn ((this control-plane) name)
  (with-slots (enemy-spawn-table) this
    (gethash name enemy-spawn-table)))


;;;
;;; SCENERY
;;;
(defclass scenery ()
  ((type :initarg :kind :reader kind-of)
   (grid :initarg :grid)))



(defmethod render ((this scenery) &key)
  (with-slots (grid) this
    (loop for x from 0 below (array-dimension grid 0)
          do (loop for y from 0 below (array-dimension grid 1)
                   do (a:when-let ((tile (find-tile (aref grid x y))))
                        (gk:with-pushed-canvas ()
                          (gk:translate-canvas (* x *tile-width*) (* y *tile-height*))
                          (render tile)))))))


;;;
;;; LEVEL
;;;
(defclass level ()
  ((sceneries :initform nil)
   (tile-map :initform (make-hash-table :test #'equal))
   (tile-width :initarg :tile-width)
   (tile-height :initarg :tile-height)
   (control-plane :initform nil)))


(defmethod dispose :after ((this level))
  (with-slots (control-plane) this
    (dispose control-plane)))


(defmethod player-spawn-position-of ((this level))
  (with-slots (control-plane) this
    (player-spawn-position-of control-plane)))


(defmethod initialize-instance :after ((this level) &key layers tiles)
  (with-slots (sceneries tile-map control-plane) this
    (flet ((%copy-to-map (key value)
             (setf (gethash key tile-map) value)))
      (loop for map in tiles
            do (maphash #'%copy-to-map map)))
    (loop for layer in layers
          when (typep layer 'control-plane)
            do (setf control-plane layer)
          when (typep layer 'scenery)
            do (push layer sceneries))
    (a:nreversef sceneries)))


(defmethod render ((this level) &key kind)
  (with-slots (sceneries tile-map tile-width tile-height control-plane) this
    (let ((*tile-map* tile-map)
          (*tile-width* tile-width)
          (*tile-height* tile-height))
      (loop for scenery in sceneries
            when (or (not kind) (eq (kind-of scenery) kind))
              do (render scenery))
      (when (eq kind :control-plane)
        (render control-plane)))))


(defmethod find-enemy-spawn ((this level) name)
  (with-slots (control-plane) this
    (find-enemy-spawn control-plane name)))

;;;
;;; PARSING
;;;
(defun invert-absolute-y (y &optional height)
  (- (* *level-height* *tile-height*) y (or height 0)))


(defun parse-level-properties (props)
  (a:alist-hash-table props :test 'equal))


(defun get-level-property (props key)
  (first (gethash key props)))


(defun parse-platform (&key kind position width height points &allow-other-keys)
  (let ((position (destructuring-bind (x y) position
                    (gk:vec2 x (invert-absolute-y y height))))
        (offsets (loop for point in points
                       collect (apply #'gk:vec2 point))))
    (ecase kind
      (:square (make-instance 'rectangle-platform :position position
                                                  :width width
                                                  :height height))
      (:polygon (make-instance 'polygon-platform :position position
                                                 :point-offsets offsets)))))


(defun parse-sensor (&key kind position width height properties &allow-other-keys)
  (let ((position (destructuring-bind (x y) position
                    (gk:vec2 x (invert-absolute-y y height))))
        (props (parse-level-properties properties)))
    (ecase kind
      (:square (make-instance 'sensor :position position
                                      :width width
                                      :height height
                                      :event (a:make-keyword
                                              (uiop:standard-case-symbol-name
                                               (get-level-property props "event"))))))))


(defun parse-control-plane (&key objects &allow-other-keys)
  (let ((enemy-spawn-table (make-hash-table :test 'equal))
        player-spawn platforms sensors)
    (loop for object in objects
          do (destructuring-bind (&rest args &key position properties
                                  &allow-other-keys)
                 object
               (let* ((props (parse-level-properties properties))
                      (type (get-level-property props "kind"))
                      (position (gk:vec2 (first position)
                                         (invert-absolute-y (second position)))))
                 (a:switch (type :test #'equal)
                   ("player-spawn" (setf player-spawn position))
                   ("enemy-spawn" (setf
                                   (gethash (get-level-property props "enemy") enemy-spawn-table)
                                   position))
                   ("platform" (push (apply #'parse-platform args) platforms))
                   ("sensor" (push (apply #'parse-sensor args) sensors))))))
    (make-instance 'control-plane :player-spawn player-spawn
                                  :enemy-spawn-table enemy-spawn-table
                                  :platforms platforms
                                  :sensors sensors)))


(defun parse-scenery (&key properties gids &allow-other-keys)
  (let ((props (parse-level-properties properties)))
    (make-instance 'scenery
                   :kind (a:switch ((get-level-property props "kind") :test #'equal)
                           ("foreground" :foreground)
                           ("background" :background))
                   :grid (loop with grid = (make-array (list *level-width* *level-height*))
                               for i from 0
                               for gid in gids
                               for y = (- *level-height* 1 (if (= i 0) 0 (truncate (/ i *level-width*))))
                               for x = (mod i *level-width*)
                               do (setf (aref grid x y) gid)
                               finally (return grid)))))


(defun parse-layer (&rest args &key name type &allow-other-keys)
  (cond
    ((eq type :layer)
     (apply #'parse-scenery args))
    ((and (eq type :objgr)
          (equal "control" name))
     (apply #'parse-control-plane args))))


(defun parse-tile (&key image position &allow-other-keys)
  (destructuring-bind (&key source height &allow-other-keys) (or image *image*)
    (a:if-let ((image-resource (gethash source *image-map*)))
      (destructuring-bind (x y) position
        (make-instance 'tile :image image-resource
                             :origin (gk:vec2 x (- height y *tile-height*))
                             :width *tile-width*
                             :height *tile-height*))
      (error "Mapping for ~A tile image is not found" source))))


(defun parse-tileset (&key image tiles tile-width tile-height &allow-other-keys)
  (let ((*tile-width* tile-width)
        (*tile-height* tile-height)
        (*image* image))
    (loop with tile-map = (make-hash-table :test 'equal)
          for (gid tile) in tiles
          do (setf (gethash gid tile-map) (apply #'parse-tile tile))
          finally (return tile-map))))


(defun parse-level (&key layers tilesets width height tile-width tile-height &allow-other-keys)
  (let ((*level-width* width)
        (*level-height* height)
        (*tile-width* tile-width)
        (*tile-height* tile-height))
    (make-instance 'level :layers (loop for layer in layers
                                        for parsed = (apply #'parse-layer layer)
                                        when parsed
                                          collect parsed)
                          :tiles (loop for tileset in tilesets
                                       collect (apply #'parse-tileset tileset))
                          :tile-width tile-width
                          :tile-height tile-height)))


(defun make-level (name universe)
  (let ((*image-map* (level-image-map name))
        (*level-universe* universe))
    (with-input-from-string (in (gk:get-text name))
      (apply #'parse-level (uiop:with-safe-io-syntax () (read in))))))
