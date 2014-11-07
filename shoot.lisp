(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; The method for when the player ship shoots
(defmethod shoot ((ship player) port)
  (let* ((turret (turret ship port))
         (payload (payload turret)))
    (when payload

      ;; Ensure we only accept payloads that name instances of thing we must
      ;; spawn for these weapons.
      (assert (not (payload-instance-p turret)))

      ;; The 'sp-player-shot spawn class encompasses all kinds of
      ;; shots the player can make, including: shots, missles,
      ;; mines. The spawn class mainly details to role of the object,
      ;; plus some other useful things an object might need to know
      ;; while spawning.
      (spawn 'sp-player-shot payload turret (game-context ship))
      (modify-score (game-context ship) -1))))

(defmethod shoot ((ship enemy) port)
  (let* ((turret (turret ship port))
         (payload (payload turret)))
    (when payload

      ;; Ensure we only accept payloads that name instances of thing we must
      ;; spawn for these weapons.
      (assert (not (payload-instance-p turret)))

      ;; The 'sp-enemy-shot spawn class encompasses all kinds of
      ;; shots the enemy can make, including: shots, missles,
      ;; mines. The spawn class mainly details to role of the object,
      ;; plus some other useful things an object might need to know
      ;; while spawning.
      (spawn 'sp-enemy-shot payload turret (game-context ship)))))
