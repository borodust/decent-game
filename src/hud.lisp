(cl:in-package :decent-game)

(gk:define-image :player-life-bar-left (asset-path "img/hud/player-life-bar/player-life-bar-left.png")
  :use-nearest-interpolation t)
(gk:define-image :player-life-bar-middle (asset-path "img/hud/player-life-bar/player-life-bar-middle.png")
  :use-nearest-interpolation t)
(gk:define-image :player-life-bar-right (asset-path "img/hud/player-life-bar/player-life-bar-right.png")
  :use-nearest-interpolation t)
(define-animation player-life-bar-cell (asset-path "img/hud/player-life-bar/player-life-bar-cell.png") :frames 2)

(define-resource-pack player-life-bar-resources ()
  :player-life-bar-left
  :player-life-bar-middle
  :player-life-bar-right
  'player-life-bar-cell)

(defun draw-player-life-bar (player position)
  (with-accessors ((hp hp) (hp-max hp-max)) player
    ;; (format t "hp:~A hp-max~A" hp hp-max)
    (gk:with-pushed-canvas ()
      (gk:translate-canvas (gk:x position) (gk:y position))
      (gk:draw-image +zero-pos+ :player-life-bar-left )
      (draw-in-row :player-life-bar-middle hp-max 4 (gk:vec2 13 0))
      (gk:draw-image (gk:vec2 (+ (* hp-max 4) 13) 0) :player-life-bar-right)
      (draw-in-row 'player-life-bar-cell hp 4 (gk:vec2 12 3) :animationp t))))

(defun draw-in-row (to-draw x x-offset pos &key (animationp nil))
  (let ((xpos (gk:x pos))
        (ypos (gk:y pos)))
    (dotimes (i x)
      (if animationp
          (draw-animation to-draw (now) (gk:vec2 (+ xpos (* i x-offset)) ypos))
          (gk:draw-image (gk:vec2 (+ xpos (* i x-offset)) ypos) to-draw)))))

(defun draw-boss-life-bar (boss)
  nil)
