; Load Screen - text menu (main-load-screen)
; Text Info Screen On Game Start describing game premise. (info-on-start)
; ESC screen - text menu (main-esc-menu)
; Death screen - text menu (death-screen)
; Boss Fight (boss-battle)
; Final Outcome Text Summary (final-outcome)
; End Credits (end-credits))
; Template
; https://gitlab.com/borodust/decent-game/-/blob/master/brainstorming/text-template.lisp

;; First cut-scene giving premise
(cl:in-package :decent-game)
(defdialogue (:info-on-start
                          (:type :cutscene)
                          ;; in order of complexity - pick one
                          ;; hero lying down,hero dying, hero lying with burning city behind
                          (:image :info-on-start))
  "The sounds of battle fade. You are dying. Why is something you just can't remeber. Aliens, humanoids with machine parts, or was it the other way around?")

;; Second screen for early game premise info
(defdialogue (:info-on-start-1
                          (:type :cutscene)
                          ;; graphic - in order of complexity - pick one
                          ;; cyborg weapon or device to left, alien weapon or bio-container
                          (:image :info-on-start-1)
                          (:choices
                           (:pick-cyborg :test #'picked-cyborg-p ) "Pick the Cyborg gear"
                           (:pick-alien :test #'picked-alien-p ) "Pick the Alien tech"
                           (:human :test #'human-p "Go without unknown enhancements")))
  "Perhaps there's some life in You left.
Close-by, a cybernetic gear pack and an alien container.
Pick one or go without?")

;; On player death
(defdialogue (:death-screen
              (:type :cutscene)
              ;; graphic - pick one
              ;; player dead , player dead custom, player dead with background, or maybe game paused with death info as OSD and menu?
              (:image :death-screen)
              (:choices
               (:last-checkpoint ) "Load last checkpoint"
               (:restart) "New game / Restart"))
  "You died.")

(defdialogue (:final-outcome
              (:type :cutscene)
              ;; graphics -TODO discuss. at least 2 for happy ending and bad ending? generic gameplay screenshot?
              (:image :final-outcome)
              (:choices
               (:pick-alien :test #'picked-alien-p ) "Using out-of control biotech, You have lost Your humanity for victory."
               (:pick-cyborg :test #'picked-cyborg-p) "By fusing Yourself with machines, You have lost Your humanity for victory"
               (:human :test #'human-p "Through the battles, You managed to keep Your humanity. Will you give others this chance?"))))

;;Boss taunts
(defdialogue (:first-enemy-kill ;; TODO cheapest event for boss taunts? Random through game?
                          (:type :osd)
                          (:image :boss-image-id))
 "Not You! Again! Let me know when you get tired of dying... Again.")
