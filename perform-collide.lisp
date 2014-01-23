(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; Primary method is both entities damage each other and one, both, or
;; neither may die.
(defmethod perform-collide ((collider collidable) (collidee collidable))
  (damage collider collidee))

;; Here we handle the processing of a something hitting a ship which might
;; or might not have a shield.
(defmethod perform-collide :around ((collider collidable)
                                    (collidee ship))
  (if (ship-main-shield collidee)
      (multiple-value-bind (absorbedp shield-is-used-up)
          (absorbs collider (ship-main-shield collidee))
        (if absorbedp
            (progn
              (die collider)
              (when shield-is-used-up
                (setf (ship-main-shield collidee) nil)))
            (call-next-method)))

      (call-next-method)))

;; The player gets the new weapon as denoted by the powerup, and the
;; powerup goes immediately stale. NOTE: If I change the collider type
;; to ship here, then ANY ship can get the powerup and its effects (as
;; long as I collide the enemies to the powerups in the main loop of
;; the game. However, I haven't coded the right geometries for the
;; ship shields or a good means to choose between them. So for now,
;; only the player can get powerups.
(defmethod perform-collide ((collider player) (collidee powerup))
  ;; A powerup can only be used ONCE
  (when (not (stalep collidee))

    (mark-stale collidee)

    (when (powerup-main-gun collidee)
      (setf (ship-main-gun collider) (powerup-main-gun collidee)))

    (when (powerup-main-shield collidee)
      (setf (ship-main-shield collider)
            (make-entity (powerup-main-shield collidee)))
      ;; XXX and orient it the same way the collider is, maybe acquiring
      ;; a powerup should be a clos verb.
      (at-location (ship-main-shield collider) collider))

    ;; If the powerup has a health level, apply it to the player.
    (incf (hit-points collider) (powerup-health-level collidee))
    (when (> (hit-points collider) (max-hit-points collider))
      (setf (hit-points collider) (max-hit-points collider)))


    ;; If I already have this weapon, then increase its power if possible.
    (when (powerup-passive-gun collidee)
      (if (ship-passive-gun collider)
          (increase-power (ship-passive-gun collider))
          (progn
            (setf (ship-passive-gun collider)
                  (make-entity (powerup-passive-gun collidee)))
            ;; XXX and orient it the same way the collider is, maybe acquiring
            ;; a passive gun should be a clos verb.
            (at-location (ship-passive-gun collider) collider))))))

;; In general, the field damages the thing it touches.
(defmethod perform-collide ((f tesla-field) (ent collidable))
  ;; First, we cause damage
  (damage f ent)

  ;; Then, we emit some sparks during the collision
  (maphash
   #'(lambda (eid path-contact)
       (with-accessors ((pc-path-ids path-ids)) path-contact
         (dolist (path-index pc-path-ids)
           (with-accessors ((fp-steps steps) (fp-path path))
               (svref (paths f) path-index)

             (unless (or (equal eid :no-collision) (zerop fp-steps))
               (let ((loc (svref fp-path (1- fp-steps))))
                 (when (zerop (random 10))
                   (spawn
                    'sp-sparks :insts/sparks (pvec (x loc) (y loc) (z loc))
                    *game*
                    :num-sparks 1 :ttl-max 5 :velocity-factor 1/20))))))))
   (entity-contacts f)))

;; field mines follow the field back to the generating ship and are
;; NOT damaged by the field, therefore they are a new primary method.
(defmethod perform-collide ((f tesla-field) (ent field-mine))
  (let ((pc (contacts f ent))
        (mx 0d0)
        (my 0d0)
        (mz 0d0))
    ;; add up the inverse direction vectors from the participating
    ;; field paths. We're going to be following the field lines
    ;; back...
    (dolist (path-index (path-ids pc))
      (with-accessors ((fp-steps steps) (fp-path path))
          (svref (paths f) path-index)
        ;; Sum the inverted direction vectors found at the last field
        ;; path step. XXX It turns out when the last step was computed, it
        ;; is often past the object (since it was in the collision radius).
        ;; This messes up the computation of this vector so I use the second
        ;; to the last step, which should be ok in accordance to the algorithm
        ;; which produced the path.
        (when (> fp-steps 1)
          (let ((loc (svref fp-path (1- (1- fp-steps)))))
            (incf mx (* (dx loc) -1d0))
            (incf my (* (dy loc) -1d0))
            (incf mz (* (dz loc) -1d0))))))

    ;; Normalize the vector, set some fraction of it to be the
    ;; direction and speed the mine should translate towards the ship.
    (let ((invdir (pvec)))
      (with-pvec-accessors (inv invdir)
        (multiple-value-bind (nvx nvy) (normalize-vector mx my)
          (setf invx (* nvx .003d0)
                invy (* nvy .003d0)
                invz 0d0)))
      ;; Set it as a hard translation to move the field-mine towards
      ;; the player while following the field lines.
      (setf (dtv ent) invdir))))

