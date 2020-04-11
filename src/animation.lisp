(cl:in-package :decent-game)


(defparameter *animation-fps* 6)
(defparameter *animation-sprite-origin* (gk:vec2 0 0))


(defun draw-animation-sprite (image time position frame-count)
  (let ((sprite-width (/ (gk:image-width image) frame-count))
        (current-frame (mod (truncate (* time *animation-fps*)) frame-count)))
    (setf (gk:x *animation-sprite-origin*) (* current-frame sprite-width))
    (bodge-canvas:antialias-shapes nil)
    (gk:draw-image position image :origin *animation-sprite-origin*
                                  :width sprite-width)))


(defgeneric draw-animation (name time position))


(defmacro define-animation (name path &key frames)
  (unless frames
    (error ":frames must be specified"))
  `(progn
     (gk:define-image ,name ,path :use-nearest-interpolation t)
     (defmethod draw-animation ((id (eql ',name)) time position)
       (draw-animation-sprite id time position ,frames))))
