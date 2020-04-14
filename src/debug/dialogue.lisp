(cl:in-package :decent-game)


(defdialogue (:debug-dialogue
              (:choices :debug-choice-1 "Doh!"
                        :debug-dialogue-cont "Huh?"
                        :debug-choice-3 "Duh..."))
  "This is a debug dialogue.
Best one right?")


(defdialogue (:debug-dialogue-cont
              (:choices :debug-start-game "Okay, fine, but
  get me to the game"
                        :debug-dialogue "No, take me back"))
  "I'm pretty sure this one is
better. Huehuehue")


(defun debug-print-dialogue-event (event &key &allow-other-keys)
  (format t "~&Event triggered: ~A" event)
  (finish-output))


(defun debug-transition-to-game (event &key &allow-other-keys)
  (declare (ignore event))
  (gk.fsm:transition-to 'init-gameplay-debug-screen))


(defclass dialogue-debug-screen (state-input-handler) ())


(defmethod gk:post-initialize ((this dialogue-debug-screen))
  (subscribe-to-event :debug-choice-1 'debug-print-dialogue-event)
  (subscribe-to-event :debug-choice-3 'debug-print-dialogue-event)
  (subscribe-to-event :debug-start-game 'debug-transition-to-game)

  (gk.fsm:transition-to 'dialogue-screen :event :debug-dialogue))

;;; input handling
(defmethod gk.input:button-released ((this dialogue-debug-screen) (button (eql :escape)))
  (gk.fsm:transition-to 'loading-screen :pack 'main-menu-resources
                                        :next-state 'main-menu))
