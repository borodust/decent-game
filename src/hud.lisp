(cl:in-package :decent-game)

(gk:define-image :player-life-bar-left (asset-path "img/hud/player-life-bar/player-life-bar-left.png"))
(gk:define-image :player-life-bar-middle (asset-path "img/hud/player-life-bar/player-life-bar-middle.png"))
(gk:define-image :player-life-bar-right (asset-path "img/hud/player-life-bar/player-life-bar-right.png"))
(define-animation player-life-bar-cell (asset-path "img/hud/player-life-bar/player-life-bar-cell.png") :frames 2)

(define-resource-pack player-life-bar-resources ()
  :player-life-bar-left
  :player-life-bar-middle
  :player-life-bar-right
  'player-life-bar-cell)

(defun draw-player-life-bar (player)
  (with-accessors ((hp hp) (hp-max hp-max)) player
    (format t "hp:~A hp-max~A" hp hp-max)))

(defun draw-boss-life-bar (boss)
  nil)
