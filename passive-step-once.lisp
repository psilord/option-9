(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; Usually nothing has a passive step
(defmethod passive-step-once (object)
  nil)

;; If the ship has a passive gun, then simulate it with the ship as
;; the source charge and other brains as the charges
(defmethod passive-step-once :after ((ent ship))
  (when (ship-passive-gun ent)
    (let ((ents
           (all-entities-in-roles (scene-man (game-context ent))
                                  :player :enemy-mine :enemy :enemy-shot)))
      (generate (ship-passive-gun ent) ent ents))))


