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
               (:file "color")
               (:file "util")
               (:file "resources")
               (:file "physics")
               (:file "event")
               (:file "spritesheet")
               (:file "animation")
               (:module "objects"
                :components ((:file "dialogue")
                             (:file "menu")
                             (:file "world")
                             (:file "projectile")
                             (:file "stats")
                             (:file "fighter")
                             (:file "player")
                             (:module "enemies"
                              :components ((:file "enemy")
                                           (:file "alien")
                                           (:file "alien-stinger")
                                           (:file "alien-shooter")))))
               (:module "states"
                :components ((:file "init")
                             (:file "loading")
                             (:file "main-menu")
                             (:file "dialogue")))
               (:file "text")
               (:file "main")

               (:module "debug"
                :components ((:file "animation")
                             (:file "dialogue")
                             (:file "gameplay")))))
