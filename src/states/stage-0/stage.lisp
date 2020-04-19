(cl:in-package :decent-game)


(gk:define-sound gameplay-tune (asset-path "snd/music/combat-zone.ogg"))


(define-level stage-0 (asset-path "tld/stage-0/stage.sxp")
  (stage-0-tilesheet (asset-path "tld/stage-0/tiles.png") "tiles.png"))


(define-resource-pack stage-0-resources (player-resources
                                         alien-shooter-0-resources
                                         alien-stinger-0-resources)
  (level-resources 'stage-0)
  'gameplay-tune)


;;;
;;; INIT
;;;
(defclass init-stage-0 (init-stage)
  ()
  (:default-initargs :pack 'stage-0-resources :next-state 'prepare-stage-0))


;;;
;;; PREPARE
;;;
(defclass prepare-stage-0 (prepare-stage)
  ()
  (:default-initargs :level 'stage-0
                     :stage 'stage-0))


(defmethod gk:post-initialize ((this prepare-stage-0))
  (gk:play-sound 'gameplay-tune :looped-p t))

;;;
;;; STAGE 0
;;;
(defclass stage-0 (stage)
  ()
  (:default-initargs :init 'init-stage-0))


;;;
;;; DESTROY
;;;
(defclass destroy-stage-0 (destroy-stage)
  ())


(defmethod gk:post-initialize ((this destroy-stage-0))
  (gk:stop-sound 'gameplay-tune))
