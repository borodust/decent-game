
(decent-game:defdialogue (:event-0 ;; this is the event that triggers this dialogue
                          ;; type can be :osd or :cutscene
                          ;; :osd is the default if no :type is provided
                          (:type :osd)
                          ;; here's the smallish avatar-like image because :osd
                          (:image :boss-image-id)
                          ;; thise is
                          (:choices :event-1 "This is the choice 1. This triggers :event-1"
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
It is not autoscrolling, so you would need to define a dialogue for each screen text.")
