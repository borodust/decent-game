(cl:pushnew :bodge-gl2 cl:*features*)

(asdf:defsystem :decent-game
  :description "Spring Lisp Game Jam 2020 entry"
  :author "decent-username, Ryan Burnside, David O'Toole, readeval, Pavel Korolev"
  :license "GNUv3"
  :depends-on (#:alexandria
               #:trivial-gamekit
               #:trivial-gamekit-fistmachine
               #:trivial-gamekit-input-handler
               #:cl-bodge/physics
               #:cl-bodge/physics/2d)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "event")
               (:file "spritesheet")
               (:file "dialogue")
               (:file "animation")
               (:module "models"
                :components ((:file "menu")))
               (:module "states"
                :components ((:file "main-menu")
                             (:file "loading")))
               (:file "main")))
