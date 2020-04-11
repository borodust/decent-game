(cl:in-package :decent-game)


(defun asset-path (pathname)
  (asdf:system-relative-pathname :decent-game (merge-pathnames pathname "assets/")))


(gk:define-font :bold-pixel-operator (asset-path "font/pixel-operator/PixelOperatorMono8-Bold.ttf"))
(gk:define-font :pixel-operator (asset-path "font/pixel-operator/PixelOperatorMono8.ttf"))
