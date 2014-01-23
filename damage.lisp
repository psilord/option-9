(in-package :option-9)

;; If two collidables damage each other, one or both can die.
(defmethod damage ((ent-1 collidable) (ent-2 collidable))
  (when (and (alivep ent-1) (alivep ent-2))
    (decf (hit-points ent-1) (damage-points ent-2))
    (when (<= (hit-points ent-1) 0)
      (die ent-1))

    (decf (hit-points ent-2) (damage-points ent-1))
    (when (<= (hit-points ent-2) 0)
      (die ent-2))))

;; If a hardnose-shot has equal or more hit points than the other simple-shot
;; it destroys it and keeps going undamaged.
(defmethod damage :around ((ent-1 hardnose-shot) (ent-2 simple-shot))
  (cond
    ((>= (damage-points ent-1) (hit-points ent-2))
     (die ent-2))
    (t
     ;; otherwise, we damage them both
     (call-next-method))))

;; If a super-shot has equal or more hit points than the other collidable
;; it destroys it and keeps going undamaged.
(defmethod damage :around ((ent-1 super-shot) (ent-2 collidable))
  (cond
    ((>= (damage-points ent-1) (hit-points ent-2))
     (die ent-2))
    (t
     ;; otherwise, we damage them both...
     (call-next-method))))

;; a tesla-field by its very nature will damage the thing it collided with
(defmethod damage ((f tesla-field) (ent collidable))
  (let ((path-contact (contacts f ent)))
    (decf (hit-points ent) (ceiling (number-of-contacts path-contact) 10))
    (when (<= (hit-points ent) 0)
      (die ent))))
