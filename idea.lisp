(in-package :option-9)

(defmethod idea (ent)
  ;; By default, nothing ever has an idea.
  nil)

;; and enemies have ideas about what they want to do in the world...
(defmethod idea ((ent enemy))
  ;; Instead of doing anything cool like inspect the world and doing
  ;; AI algorithms, we'll just shoot


  ;; HACK: Enemies react immediately to the fact that they need to fly
  ;; a direction. A better solution would be to move towards the
  ;; player as opposed to just left or right depending where I spawned
  ;; in the world. Also, this forces all enemies to fly.... Not good.
  (unless (flyingp ent)
    (setf (flyingp ent) t
          (dfv ent) (with-pvec-accessors (o (matrix-translate-get
                                             (world-basis ent)))
                      (pvec
                       ;; Strafe in a direction related to
                       ;; where I spawned horizontally
                       (if (< ox (per-game-width *game* 50.0))
                           ;; If on the left side, strafe left
                           (per-hz (strafe-left-speed ent))
                           ;; if on the right side, strafe right
                           (per-hz (strafe-right-speed ent)))
                       ;; Forward speed
                       (per-hz (forward-speed ent))
                       ;; Stay in the plane, no up/down speed
                       0d0))))

  (shoot ent :front-weapon-port))
