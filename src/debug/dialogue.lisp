(cl:in-package :decent-game)


(defdialogue (:debug-dialogue
              (:choices :debug-choice-1 "Doh!"
                        :debug-choice-2 "Huh?"
                        :debug-choice-3 "Duh..."))
  "This is debug dialogue.
Best one right?.")


(defclass dialogue-debug-screen (state-input-handler) ())


(defmethod gk:post-initialize ((this dialogue-debug-screen))
  (gk.fsm:transition-to 'dialogue-screen :event :debug-dialogue))
