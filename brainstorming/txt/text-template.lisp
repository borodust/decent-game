;;;TODO if the code is changed and this doesn't work, i'll just re-do it. lemme know.

;; First cut-scene giving premise
(decent-game:defdialogue (:info-on-start

                          (:type :cutscene)
                          ;; in order of complexity - pick one
                          ;; hero lying down,hero dying, hero lying with burning city behind
                          (:image :info-on-start))
  "The sounds of battle fade. You are dying. Why is something you just can't remeber. Aliens, humanoids with machine parts, or was it the other way around?")

;; Second screen for early game premise info
(decent-game:defdialogue (:info-on-start-1

                          (:type :cutscene)
                          ;; in order of complexity - pick one
                          ;; cyborg weapon or device to left, alien weapon or bio-container
                          (:image :info-on-start-1)
                          (:choices
                           (:pick-cyborg :test #'picked-cyborg-p ) "Pick the Cyborg gear"
                           (:pick-alien :test #'picked-alien-p ) "Pick the Alien tech"
                           (:human :test #'human-p "Go without unknown enhancements")))
  "Perhaps there's some life in You left.
Close-by, a cybernetic gear pack and an alien container.
Pick one or go without?")

;;Boss taunts
(decent-game:defdialogue (:first-enemy-kill ;; TODO cheapest event for boss taunts? Random through game?
                          (:type :osd)
                          (:image :boss-image-id)))
"Not You! Again! Let me know when you get tired of dying... Again."

;;; Default template
;;;
(decent-game:defdialogue (:event-0 ;; this is the event that triggers this dialogue
                          ;; type can be :osd or :cutscene
                          ;; :osd is the default if no :type is provided
                          (:type :osd)
                          ;; here's the smallish avatar-like image because :osd
                          (:image :boss-image-id)
                          ;; dialogue might have choices
                          ;; choices can have predicates that control if particular choice is shown or not
                          (:choices
                           ;; this choice also uses a predicated user-have-label-p
                           ;; if it returns true - choice is shown, otherwise choice is hidden
                           (:event-1 :test #'user-have-label-p) "This is the choice 1. This triggers :event-1"
                           :event-2 "This is the choice 2. This triggers :event-2. See next dialogue"))
    "Multiline text goes here.
This is the main text that takes the most space.")

;; several dialogues can be defined in a single file
(decent-game:defdialogue (:event-2 ;; this is the event that triggers this dialogue
                          (:type :cutscene)
                          ;; here's the full screen image because :cutscene
                          (:image :fail-screen)
                          ;; if you want for something to happend when user hits enter
                          ;; other than dialogue closing, you need to provide
                          ;; default choice w/o a text
                          (:choices :event-stop-game))
    "Another multiline text goes here.
It is not autoscrolled, and you would need to define a dialogue for each screen of text.")



;   Load Screen - text menu (main-load-screen)

;; Text Info Screen On Game Start describing game premise. (info-on-start)

;;    ESC screen - text menu (main-esc-menu)

;;    Boss Fight (boss-battle)

;;    Final Outcome Text Summary (final-outcome)

;;    End Credits (end-credits))
