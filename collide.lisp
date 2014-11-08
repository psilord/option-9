(in-package :option-9)

;; In general if something isn't collidable, nothing happens.
(defmethod collide (fist face)
  nil)

;; See if two collidables physically collide.
(defmethod collide ((fist collidable) (face collidable))
  (when (and (alivep fist) (alivep face)
             (vulnerable fist) (vulnerable face))
    (when (< (distance fist face) (max (radius fist) (radius face)))
      ;; tell both objects what they collided with. In practice this
      ;; means that by default, both will explode.
      (perform-collide fist face))))

;; Ships can have passive-guns, so here we compute if the passive gun
;; hit the other collidable. The face will hit back in this
;; context. :)
(defmethod collide :before ((fist collidable) (face ship))
  (when (and (alivep fist) (alivep face)
             (vulnerable fist) (vulnerable face))
    (let ((payload (payload (turret face :passive-weapon-port))))
      (when payload
        (collide payload fist)))))

;; If any field lines hit the face, perform the collision with it.
(defmethod collide ((f tesla-field) (face collidable))
  (when (and (alivep face) (vulnerable face))
    (when (contacts f face)
      (perform-collide f face))))

;; However, for field mines, don't wait until their vulnerable to start
;; pulling them to the user.
(defmethod collide ((f tesla-field) (face field-mine))
  (when (alivep face)
    (when (contacts f face)
      (perform-collide f face))))
