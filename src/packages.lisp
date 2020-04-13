(cl:defpackage :decent-game
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:gk #:trivial-gamekit)
                    (#:gk.fsm #:trivial-gamekit.fistmachine)
                    (#:gk.input #:trivial-gamekit.input-handler)
                    (#:b #:cl-bodge.engine)
                    (#:b.phy #:cl-bodge.physics))
  (:export #:play))
