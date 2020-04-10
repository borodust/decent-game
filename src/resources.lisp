(cl:in-package :decent-game)


(defun asset-path (pathname)
  (asdf:system-relative-pathname :decent-game (merge-pathnames pathname "assets/")))
