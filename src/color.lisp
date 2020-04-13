(in-package :decent-game)

(defun randcolor ()
  "Returns a random color as a gamekit:vec4."
  (gk:vec4 (/ (random 100) 100)
           (/ (random 100) 100)
           (/ (random 100) 100)
           1))

(defun alphacolor (alpha color)
  "Returns a gamekit:vec4 color with an alpha value of alpha,
without modifying the original argument."
  (let ((new-color color))
    (setf (gk:w new-color) alpha)))


(defun hexcolor (hexcode &optional (a 1))
  "Takes a string which represents a hexadecimal color. (e.g. #ffffff for white)
and returns the appropriate gamekit:vec4, which corresponds to that color.
You can also pass an optional opacity value as a second argument"
  (let* ((color-string (subseq hexcode 1))
         (r (float (/ (parse-integer (subseq color-string 0 2) :radix 16) 255)))
         (g (float (/ (parse-integer (subseq color-string 2 4) :radix 16) 255)))
         (b (float (/ (parse-integer (subseq color-string 4 6) :radix 16) 255))))
    (gk:vec4 r g b a)))


(defvar +black+ (gk:vec4 0 0 0 1))
