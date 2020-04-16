(uiop:define-package :decent-game.tiled
  (:use :cl :cffi :cffi-c-ref)
  (:export #:convert))


(cl:in-package :decent-game.tiled)


(declaim (special *properties*
                  *objects*
                  *map*))


(defcallback parse-property :void ((property :pointer) (data :pointer))
  (declare (ignore data))
  (c-val ((property %tmx:property))
    (let ((type (property :type)))
      (push (list :name (property :name)
                  :value (c-let ((value %tmx:property-value :from (property :value)))
                           (case type
                             (:int (value :integer))
                             (:bool (unless (= (value :boolean) 0) t))
                             (:float (value :decimal))
                             (:color (value :color))
                             (:string (value :string))
                             (:file (value :file)))))
            *properties*))))


(defun parse-properties (properties)
  (let (*properties*)
    (unless (null-pointer-p properties)
      (%tmx:property-foreach properties
                             (callback parse-property)
                             (null-pointer)))
    *properties*))


(defun parse-layer-content (content)
  (c-let ((tmx %tmx:map :from *map*))
    (c-val ((content (:union %tmx:layer-content)))
      (list :gids (loop for i from 0 below (* (tmx :width) (tmx :height))
                        collect (content :gids * i))))))



(defun parse-poly-shape (content)
  (c-val ((content %tmx:shape))
    (list :points (loop for i from 0 below (content :points-len)
                        collect (list (content :points * i * 0)
                                      (content :points * i * 1))))))

(defun parse-polygon (content)
  (declare (ignore content)))

(defun parse-polyline (content)
  (declare (ignore content)))

(defun parse-text (content)
  (declare (ignore content)))


(defun parse-objects (head)
  (loop for object = head then (c-ref object %tmx:object :next)
        until (null-pointer-p object)
        collect (c-val ((object %tmx:object))
                  (let ((kind (object :obj-type)))
                    (list* :id (object :id)
                           :kind kind
                           :name (object :name)
                           :type (object :type)
                           :position (list (object :x) (object :y))
                           :width (object :width)
                           :height (object :height)
                           :rotation (object :rotation)
                           :properties (parse-properties (object :properties))
                           (case kind
                             (:polygon (parse-poly-shape (object :content :shape)))
                             (:polyline (parse-poly-shape (object :content :shape)))
                             (:tile (object :content :gid))
                             (:text (parse-text (object :content :text)))))))))


(defun parse-object-group-content (content)
  (c-val ((content (:union %tmx:layer-content)))
    (c-let ((object-group %tmx:object-group :from (content :objgr)))
      (list :objects (parse-objects (object-group :head))))))


(defun parse-layer (layer)
  (c-val ((layer %tmx:layer))
    (let ((type (layer :type)))
      (list* :type type
             :name (layer :name)
             :properties (parse-properties (layer :properties))
             (case type
               (:layer (parse-layer-content (layer :content)))
               (:objgr (parse-object-group-content (layer :content))))))))


(defun parse-image (image)
  (unless (null-pointer-p image)
    (c-val ((image %tmx:image))
      (list :source (image :source)
            :width (image :width)
            :height (image :height)))))


(defun parse-tile (tile)
  (unless (null-pointer-p tile)
    (c-val ((tile %tmx:tile))
      (list :id (tile :id)
            :type (tile :type)
            :position (list (tile :ul-x) (tile :ul-y))
            :image (parse-image (tile :image))
            :properties (parse-properties (tile :properties))))))


(defun parse-tiles (tiles count)
  (unless (or (null-pointer-p tiles) (= count 0))
    (c-val ((tiles (:pointer %tmx:tile)))
      (loop for i from 0 below count
            unless (null-pointer-p (tiles i))
              collect (parse-tile (tiles i))))))


(defun parse-tileset (tileset gid-offset)
  (c-val ((tileset %tmx:tileset))
    (list :name (tileset :name)
          :tile-width (tileset :tile-width)
          :tile-height (tileset :tile-height)
          :offset (list (tileset :x-offset) (tileset :y-offset))
          :spacing (tileset :spacing)
          :margin (tileset :margin)
          :image (parse-image (tileset :image))
          :tiles (loop for i from 0 below (tileset :tilecount)
                       unless (null-pointer-p (tileset :tiles * i))
                         collect (list (+ i gid-offset)
                                       (parse-tile (tileset :tiles * i))))
          :properties (parse-properties (tileset :properties)))))


(defun parse-map (tmx)
  (c-val ((tmx %tmx:map))
    (let ((*map* (tmx &)))
      (list :layers (unless (null-pointer-p (tmx :ly-head))
                      (loop for layer = (tmx :ly-head) then (c-ref layer %tmx:layer :next)
                            until (null-pointer-p layer)
                            collect (parse-layer layer)))
            :tilesets (unless (null-pointer-p (tmx :ts-head))
                        (loop for item = (tmx :ts-head) then (c-ref item %tmx:tileset-list :next)
                              until (null-pointer-p item)
                              collect (c-val ((item %tmx:tileset-list))
                                        (parse-tileset (item :tileset) (item :firstgid)))))
            :width (tmx :width)
            :height (tmx :height)
            :tile-width (tmx :tile-width)
            :tile-height (tmx :tile-height)
            :properties (parse-properties (tmx :properties))))))



(defun convert (source-file target-file)
  (let ((tmx (%tmx:load (uiop:native-namestring source-file))))
    (when (null-pointer-p tmx)
      (error "Failed to load tmx map from ~A: ~A"
             source-file (%tmx:strerr)))
    (unwind-protect
         (with-open-file (out target-file :direction :output :if-exists :supersede)
           (prin1 (parse-map tmx) out))
      (%tmx:map-free tmx))))
