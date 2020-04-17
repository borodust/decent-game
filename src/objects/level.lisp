(cl:in-package :decent-game)


(declaim (special *level-width*
                  *level-height*
                  *image*
                  *tile-width*
                  *tile-height*
                  *tile-map*
                  *image-map*))


;;;
;;; TILE
;;;
(defclass tile ()
  ((image :initarg :image)
   (origin :initarg :origin)
   (width :initarg :width)
   (height :initarg :height)))


(defmethod render ((this tile))
  (with-slots (image origin width height) this
    (gk:draw-image +zero-pos+ image :origin origin
                                    :width width
                                    :height height)))


(defun find-tile (gid)
  (gethash gid *tile-map*))


;;;
;;;
;;;
(defclass obstacle () ())
(defclass sensor () ())


(defclass platform (obstacle)
  ((position :initarg :position)))


(defclass rectangle-platform (platform)
  ((width :initarg :width)
   (height :initarg :height)))


(defclass polygon-platform (platform)
  ((point-offsets :initarg :offsets)))


(defun make-platform (&key kind position width height points &allow-other-keys)
  (let ((position (apply #'gk:vec2 position))
        (offsets (loop for point in points
                       collect (apply #'gk:vec2 point))))
    (ecase kind
      (:square (make-instance 'rectangle-platform :position position
                                                  :width width
                                                  :height height))
      (:polygon (make-instance 'polygon-platform :position position
                                                 :point-offsets offsets)))))

;;;
;;; CONTROL-PLANE
;;;
(defgeneric player-spawn-position-of (object))
(defclass control-plane ()
  ((spawn :initarg :spawn :reader player-spawn-position-of)
   (platforms :initarg :platforms)))


;;;
;;; SCENERY
;;;
(defclass scenery ()
  ((grid :initarg :grid)))



(defmethod render ((this scenery))
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


(defmethod render ((this level))
  (with-slots (sceneries tile-map tile-width tile-height) this
    (let ((*tile-map* tile-map)
          (*tile-width* tile-width)
          (*tile-height* tile-height))
      (loop for scenery in sceneries
            do (render scenery)))))



;;;
;;; PARSING
;;;
(defun parse-object (&key kind id name
                       position width height rotation
                       points gid properties
                     &allow-other-keys)
  (format t "~&~A" (list id name kind position width height rotation points gid properties))
  (finish-output))


(defun parse-control-plane (&key objects &allow-other-keys)
  (multiple-value-bind (spawn platforms)
      (loop with spawn and platforms
            for object in objects
            do (destructuring-bind (&rest args &key kind id name
                                                 position width height rotation
                                                 points gid properties
                                    &allow-other-keys)
                   object
                 (let* ((prop-table (a:alist-hash-table properties :test 'equal))
                        (type (first (gethash "kind" prop-table))))
                   (a:switch (type :test #'equal)
                     ("player-spawn" (setf spawn (apply #'gk:vec2 position)))
                     ("platform" (push (apply #'make-platform args) platforms)))))
            finally (return (values spawn platforms)))
    (make-instance 'control-plane :spawn spawn :platforms platforms)))


(defun parse-scenery (&key gids &allow-other-keys)
  (make-instance 'scenery
                 :grid (loop with grid = (make-array (list *level-width* *level-height*))
                             for i from 0
                             for gid in gids
                             for y = (- *level-height* 1 (if (= i 0) 0 (truncate (/ i *level-width*))))
                             for x = (mod i *level-width*)
                             do (setf (aref grid x y) gid)
                             finally (return grid))))


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
        (*level-height* height))
    (make-instance 'level :layers (loop for layer in layers
                                        for parsed = (apply #'parse-layer layer)
                                        when parsed
                                          collect parsed)
                          :tiles (loop for tileset in tilesets
                                       collect (apply #'parse-tileset tileset))
                          :tile-width tile-width
                          :tile-height tile-height)))


(defun make-level (descriptor image-map)
  (let ((*image-map* (a:plist-hash-table image-map :test #'equal)))
    (apply #'parse-level :image-map image-map descriptor)))
