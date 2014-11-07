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

    ;; TODO convert generic payload to specialized one based upon
    ;; instance name of the collider. This is done for the active
    ;; stuff, but do it for everything.

    ;; process all turret powerups contained by this powerup.
    (dolist (pup (powerup-turrets collidee))
      (destructuring-bind (port-name turret-name payload) pup
        ;; FIXME: Handle passive powerups or incremental powerups.
        ;; Use a CASE on port-name, maybe? Sounds kinda nasty, but it
        ;; might work for a bit.

        ;; For these forms, a port-name must always exist.
        (assert port-name)

        (case port-name
          ((:front-weapon-port :left-weapon-port :center-weapon-port
                               :rear-weapon-port)
           ;; upgrade the turret at the port-name, the payload, or both.
           (let* ((the-turret (turret collider port-name))
                  ;; Try and figure out if the possibly generic name
                  ;; this payload has should be specialized to an
                  ;; instance appropriate for this collider.
                  (payload (specialize-generic-instance-name
                            (instance-name collider) payload))
                  ;; Note payload here would have been specialized!
                  (payload (if payload
                               payload
                               ;; else get the current payload from the
                               ;; named turret.
                               (payload the-turret)))
                  ;; Maybe I have a new turret to use too!
                  (replacement-turret
                   (if turret-name
                       (make-entity turret-name)
                       the-turret)))

             ;; Ensure the computed payload is assigned into the
             ;; computed turret.
             (setf (payload replacement-turret) payload)

             ;; These turrets use instance names as their payload.
             (setf (payload-instance-p replacement-turret)
                   (if (keywordp payload) nil t))

             ;; ensure the ship's port containes the computed turret
             (setf (turret collider port-name) replacement-turret)))

          ((:passive-weapon-port)
           ;; FIXME: Make this deal with turrets properly. This current code
           ;; path is totally screwed.

           ;; If I already have this weapon, then increase its power
           ;; if possible. (Otherwise, replace it with the new one.)

           ;; TODO: fix this to call (INCREASE item payload) when the payload
           ;; of the powerup is identical to the actual type of the current
           ;; passive gun.
           (when (powerup-passive-gun collidee)
             (if (ship-passive-gun collider)
                 (increase-power (ship-passive-gun collider))
                 (progn
                   (setf (ship-passive-gun collider)
                         (make-entity (powerup-passive-gun collidee)))
                   ;; XXX and orient it the same way the collider is,
                   ;; maybe acquiring a passive gun should be a clos
                   ;; verb.
                   (at-location (ship-passive-gun collider) collider)))))

          ((:shield-port)
           (let* ((the-turret (turret collider port-name))
                  ;; Try and figure out if the possibly generic name
                  ;; this payload has should be specialized to an
                  ;; instance appropriate for this collider.
                  (payload (specialize-generic-instance-name
                            (instance-name collider) payload))
                  ;; Note payload here would have been specialized!
                  (payload (if payload
                               (let ((the-shield (make-entity payload)))
                                 ;; Set up its initial location, which is
                                 ;; where the turret is on the collider!
                                 (at-location the-shield the-turret)
                                 ;; jam it into the scene-tree.
                                 (insert-into-scene
                                  (scene-man (game-context collider))
                                  the-shield
                                  collider)
                                 ;; And get rid of the one currently there
                                 ;; from the scene tree and turret!
                                 (when (payload the-turret)
                                   (remove-from-scene
                                    (scene-man (game-context collider))
                                    (payload the-turret))
                                   (setf (payload the-turret) nil))
                                 ;; here is the payload we wish to use.
                                 the-shield)
                               ;; or use the one I already have.
                               (payload the-turret)))
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
             (setf (turret collider port-name) replacement-turret))))))



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
                    :num-sparks 2 :ttl-max 5 :velocity-factor .2d0))))))))
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
