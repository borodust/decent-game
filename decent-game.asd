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
               #:cl-bodge/physics/2d
               #:cl-tiled)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "color")
               (:file "util")
               (:file "timer-util")
               (:file "resources")
               (:file "physics")
               (:file "event")
               (:file "spritesheet")
               (:file "animation")
               (:file "hud")
               (:module "objects"
                :components ((:file "dialogue")
                             (:file "menu")
                             (:file "level")
                             (:file "world")
                             (:file "stats")
                             (:file "hitbox")
                             (:file "bullet")
                             (:file "fighter")
                             (:file "player")
                             (:module "enemies"
                              :components ((:file "enemy")
                                           (:file "boss")
                                           (:file "alien")
                                           (:file "alien-stinger")
                                           (:file "alien-shooter")))))
               (:module "states"
                :components ((:file "base-loading-screen")
                             (:file "init")
                             (:file "loading")
                             (:file "main-menu")
                             (:file "dialogue")
                             (:file "replay")
                             (:file "pause")
                             (:module "stage-0"
                              :components ((:file "stage")))))
               (:file "text")
               (:file "main")

               (:module "debug"
                :components ((:file "animation")
                             (:file "dialogue")
                             (:file "gameplay")))))



(asdf:defsystem :decent-game/tiled
  :description "Tiled map converter"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (#:bodge-tmx :cffi :cffi-c-ref)
  :pathname "tiled/"
  :serial t
  :components ((:file "exporter")))
