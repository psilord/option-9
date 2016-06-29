(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; Primary method is both entities damage each other and one, both, or
;; neither may die.
(defmethod perform-collide ((collider collidable) (collidee collidable))
  (damage collider collidee))

(defmethod perform-collide :after ((collider shot) (collidee collidable))
  ;; Depending on the charge shoot 1-5 shots in random directions
  (when (> (charge-percentage collider) .25)
    ;; TODO: Sadly, I would need the MOP to do this elegantly or use
    ;; an :append method for each class in the hierarchy to rebuild
    ;; the initialization list using the current values in the object.
    ;; This should assure I get a deep copy.
    (loop repeat (lerp 0 5 (charge-percentage collider) :truncp t) do
         (spawn 'sp-realize
                (instance-name collider)
                (world-basis collider)
                (game-context collider)
                :parent :universe
                :orphan-policy :destroy
                :extra-init
                (list
                 ;; I should have a powerup such that the
                 ;; charge of a shot can propogate to
                 ;; here, causing a fission-type reaction
                 ;; as each charged shot hits other stuff
                 ;; too. This is a cool effect!

                 ;; :charge-percentage 1.0

                 :flyingp (flyingp collider)
                 :local-basis (matrix-copy (local-basis collider))
                 :world-basis (matrix-copy (world-basis collider))
                 :roles (copy-seq (roles collider))
                 :dfv (vcopy (dfv collider))
                 :drv (vcopy (drv collider))
                 ;; and it goes in a random direction.
                 :dr (pvec 0d0 0d0 (random (* 2d0 pi)))
                 :dtv (vcopy (dtv collider))
                 :dv (vcopy (dv collider))
                 :rotatingp (rotatingp collider)
                 :local-basis (mc (local-basis collider))
                 :previous-world-basis (mc (previous-world-basis collider))
                 :previous-world-basis-defined-p (previous-world-basis-defined-p collider))))))

;; Here we handle the processing of a something hitting a ship which might
;; or might not have a shield.
(defmethod perform-collide :around ((collider collidable)
                                    (collidee ship))

  (let ((the-shield (payload (turret collidee :shield-port))))
    (when the-shield
      (multiple-value-bind (absorbedp shield-is-used-up)
          (absorbs collider the-shield)
        (when absorbedp
          (die collider)
          (when shield-is-used-up
            (setf (payload (turret collidee :shield-port)) nil)
            (remove-from-scene (scene-man (game-context collidee))
                               the-shield))
          ;; The :around method consumed the action, so don't continue.
          (return-from perform-collide nil)))))

  ;; If I get here, it means we didn't absorb the shot and must continue
  ;; processing.
  (call-next-method))

;; The player gets the new weapon as denoted by the powerup, and the
;; powerup goes immediately stale. NOTE: If I change the collider type
;; to ship here, then ANY ship can get the powerup and its effects (as
;; long as I collide the enemies to the powerups in the main loop of
;; the game. However, I haven't coded the right geometries for the
;; ship shields or a good means to choose between them. So for now,
;; only the player can get powerups. It is actually an interesting game
;; mechanics decision if enemies can get powerups!
(defmethod perform-collide ((collider player) (collidee powerup))
  ;; A powerup can only be used ONCE
  (when (not (stalep collidee))

    (mark-stale collidee)

    ;; process all turret powerups contained by this powerup.
    (dolist (pup (powerup-turrets collidee))
      (destructuring-bind (port-name turret-name payload) pup
        ;; FIXME: Handle passive powerups or incremental powerups.
        ;; Use a CASE on port-name, maybe? Sounds kinda nasty, but it
        ;; might work for a bit.

        ;; For these forms, a port-name must always exist.
        (assert port-name)
        (let* ((the-turret (turret collider port-name))
               ;; Try and figure out if the possibly generic name
               ;; this payload has should be specialized to an
               ;; instance appropriate for this collider.
               (payload (specialize-generic-instance-name
                         (instance-name collider) payload))

               ;; Note payload here would have been specialized!
               ;; TODO: Ask the current payload, if there is one
               ;; AND IT IS THE SAME, then increase the power of
               ;; the payload
               (payload
                (cond
                  ((and (payload the-turret)
                        (eq (instance-name (payload the-turret)) payload))
                   ;; if we already have one and it matches what we
                   ;; just picked up, then see if it wants to
                   ;; increase its power, and return the same
                   ;; payload.
                   (increase-power (payload the-turret))
                   (payload the-turret))

                  (payload
                   (let ((the-item (make-entity payload)))
                     ;; Set up its initial location, which is
                     ;; where the turret is on the collider!
                     (at-location the-item the-turret)
                     ;; jam it into the scene-tree.
                     (insert-into-scene
                      (scene-man (game-context collider))
                      the-item
                      collider)
                     ;; And get rid of the one currently there
                     ;; from the scene tree and turret!
                     (when (payload the-turret)
                       (remove-from-scene
                        (scene-man (game-context collider))
                        (payload the-turret))
                       (setf (payload the-turret) nil))
                     ;; here is the payload we wish to use.
                     the-item))

                  (t
                   ;; or use the one I already have.
                   (payload the-turret))))


               ;; Maybe I have a new turret to use too!
               (replacement-turret
                (if turret-name
                    (let ((new-turret (make-entity turret-name)))
                      ;; dump current turret.
                      (remove-from-scene (scene-man (game-context collider))
                                         the-turret)
                      (setf (turret collider port-name) nil)
                      ;; shove new one into scene tree.
                      (insert-into-scene
                       (scene-man (game-context collider))
                       new-turret collider)
                      ;; and this is out replacement...
                      new-turret)
                    ;; or just use the old one.
                    the-turret)))

          ;; Ensure the computed payload is assigned into the
          ;; computed turret.
          (setf (payload replacement-turret) payload)

          ;; These turrets use instance names as their payload.
          (setf (payload-instance-p replacement-turret)
                (if (keywordp payload) nil t))

          ;; ensure the ship's port containes the computed turret
          (setf (turret collider port-name) replacement-turret))))

    ;; TODO, If the powerup affects payload charging in a turret, do
    ;; the effect here.
    (dolist (ceff (charging-effects collidee))
      (destructuring-bind (port-name mode) ceff
        (let ((payload (payload (turret collider port-name))))
          (when payload
            ;; When getting the same charging/decaying powerup, we make it
            ;; twice as fast in an increase-power kind of way. TODO: Consider
            ;; using an increase-charge or increase-decay verb.
            (cond
              ((eq mode :charging)
               (setf (chargeablep payload) t)
               (if (null (charge-time payload))
                   (setf (charge-time payload) (in-usecs 2.0))
                   (setf (charge-time payload) (/ (charge-time payload) 2.0))))
              ((eq mode :decaying)
               (setf (decayablep payload) t)
               (if (null (decay-time payload))
                   (setf (decay-time payload) (in-usecs 2.0))
                   (setf (decay-time payload) (/ (decay-time payload) 2.0)))))))))

    ;; If the powerup has a health level, apply it to the player.
    (incf (hit-points collider) (powerup-health-level collidee))
    (when (> (hit-points collider) (max-hit-points collider))
      (setf (hit-points collider) (max-hit-points collider)))

    ))

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
                    :num-sparks 1
                    :ttl-max (in-usecs .075)
                    :velocity-factor .2d0))))))))
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
          (setf invx (* nvx .3d0)
                invy (* nvy .3d0)
                invz 0d0)))
      ;; Set it as a hard translation to move the field-mine towards
      ;; the player while following the field lines.
      (setf (dtv ent) invdir))))
