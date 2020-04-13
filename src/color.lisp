(in-package :decent-game)

(defun random-color ()
  (gk:vec4 (/ (random 100) 100)
           (/ (random 100) 100)
           (/ (random 100) 100)
           1))

(defun hexcolor (hexcode &optional (a 1))
  (let* ((color-string (subseq hexcode 1))
         (r (float (/ (parse-integer (subseq color-string 0 2) :radix 16) 255)))
         (g (float (/ (parse-integer (subseq color-string 2 4) :radix 16) 255)))
         (b (float (/ (parse-integer (subseq color-string 4 6) :radix 16) 255))))
    (gk:vec4 r g b a)))


(defvar +black+ (gk:vec4 0 0 0 1))
