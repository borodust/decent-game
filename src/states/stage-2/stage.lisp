(cl:in-package :decent-game)


(gk:define-sound boss-tune (asset-path "snd/music/gigamegasuuperbossbeaty.ogg"))

(define-level stage-2 (asset-path "tld/stage-2/stage.sxp")
  (stage-2-tilesheet (asset-path "tld/stage-2/tiles.png") "tiles.png")
  (boss-stage-2-tilesheet (asset-path "tld/stage-2/boss.png") "tileset-1-with-alpha.png"))


(define-resource-pack stage-2-resources (player-resources
                                         boss-resources)
  (level-resources 'stage-2)
  'boss-tune)


;;;
;;; INIT
;;;
(defclass init-stage-2 (init-stage)
  ()
  (:default-initargs :pack 'stage-2-resources :next-state 'prepare-stage-2))


;;;
;;; PREPARE
;;;
(defclass prepare-stage-2 (prepare-stage)
  ()
  (:default-initargs :level 'stage-2
                     :stage 'stage-2))


(defmethod gk:post-initialize ((this prepare-stage-2))
  (gk:play-sound 'boss-tune :looped-p t))

;;;
;;; STAGE 0
;;;
(defclass stage-2 (stage)
  ()
  (:default-initargs :init 'init-stage-2))


(defmethod gk:post-initialize ((this stage-2))
  (spawn-enemy (world-of this) 'boss "boss"))


;;;
;;; DESTROY
;;;
(defclass destroy-stage-2 (destroy-stage)
  ())


(defmethod gk:post-initialize ((this destroy-stage-2))
  (gk:stop-sound 'boss-tune))
