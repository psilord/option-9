(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; Usually nothing has a passive step
(defmethod passive-step-once (object)
  nil)

;; If the ship has a passive gun, then simulate it with the ship as
;; the source charge and other brains as the charges
(defmethod passive-step-once :after ((ent ship))
  (let ((payload (payload (turret ent :passive-weapon-port))))
    (when payload
      (let ((ents
             (all-entities-in-roles (scene-man (game-context ent))
                                    :player :enemy-mine :enemy :enemy-shot)))
        (generate payload ent ents)))))
