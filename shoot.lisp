(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; The method for when the player ship shoots
(defmethod shoot ((ship player))
  (spawn 'sp-player-shot (ship-main-gun ship) ship (game-context ship))
  (modify-score (game-context ship) -1))

;; The method for when the enemy ship shoots.
(defmethod shoot ((ship enemy))
  (spawn 'sp-enemy-shot (ship-main-gun ship) ship (game-context ship)))
