(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; If something is told to be dead, we kill it and blow it up.
(defmethod die ((ent entity))
  (mark-dead ent)
  (explode ent))

;; When an enemy dies, there is a small chance a power up or a mine
;; gets created in the spot of death.
(defmethod die :after ((ent enemy))
  ;; An entity may die multiple times a frame and for different reasons,
  ;; but we only want to do this once.
  (let ((chance (random 1.0)))
    (cond
      ((< chance .50)
       (spawn 'sp-player-powerup :insts/powerups ent (game-context ent)))
      ((and (>= chance .50) (< chance .75))
       (spawn 'sp-enemy-mine :insts/mines ent (game-context ent))))))

