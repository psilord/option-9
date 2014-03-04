(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defmethod walk-frame-hierarchy ((root frame) func)
  "Do a preorder traversal across the object hierarchy from the root applying
the FUNC to each frame as one walks towards the leaves."
  ;; It is important that this walks from the root towards the leaves.
  (funcall func root)

  ;; XXX this loop is kinda slow, maybe I should use some other type of
  ;; structure here.
  (when (children root)
    (loop for c being the hash-values of (children root) do
         (walk-frame-hierarchy c func))))

(defmethod resolve-world-basis ((f frame))
  ;; If I have a parent, then compute my world-coordinates in relation
  ;; to it.  The "universe" frame's world coord is identity and never
  ;; changes, because since everything is a child of it, it would be
  ;; meaningless.
  (if (parent f)
      (pm-mul-into (world-basis f) (world-basis (parent f)) (local-basis f))
      (pm-copy-into (world-basis f) (local-basis f))))

(defmethod resolve-world-basis :after ((s ship))
  ;; If a ship has a shield, then resolve where it is supposed to be.
  ;; flyingp doesn't matter here.
  (let ((shield (ship-main-shield s)))
    (when shield
      (pm-mul-into (world-basis shield)
                   (world-basis (parent s))
                   (local-basis shield)))))

;; XXX Transform each one of these operations into VERBS, it'll make clipping
;; and other effects much easier.
(defmethod update-local-basis ((f frame))

  ;; Add in a one time displacement not related to flying, if any
  (pm-translate-into (local-basis f) (dv f))
  ;; Zero it, so if no other displacements are added, we add nothing.
  (pv-clear-into (dv f))

  ;; Rotate the one time rotation not related to rotatingp, if any.
  (pm-rotate-basis-into (local-basis f) (dr f))
  ;; Then zero it
  (pv-clear-into (dr f))

  ;; Add in the incremental translation not related to the basis
  ;; directions vector and do the decay of it
  (unless (pv-zero-p (dtv f))
    (pm-translate-into (local-basis f) (dtv f))
    (dtv-decay f))

  ;; perform the incremental flying vector.
  (when (flyingp f)
    (pm-fly-into (local-basis f) (dfv f)))

  ;; perform the incremental rotation vector.
  (when (rotatingp f)
    (pm-rotate-basis-into (local-basis f) (drv f))))

;; Perform one physical and temporal step in the simulation
(defmethod active-step-once ((ent frame))

  ;; Update the physical nature
  (update-local-basis ent)

  ;; Update the temporal nature
  (unless (null (ttl ent))
    (when (> (ttl ent) 0)
      (decf (ttl ent))))

  ;; If it is invulnerable, decrement how long it has left to be so.
  (unless (null (inttl ent))
    (when (> (inttl ent) 0)
      (decf (inttl ent)))))

;; The player's world objects get bound to the edges of the screen
;;
;; XXX This is a bit skeevy because I am assuming the parent of the
;; local-basis is the :universe. I should do something more
;; mathematically correct concerning figuring out if I should either
;; apply the flight vector in the first place, or, having been
;; applied, figure out the correction to apply in world space,
;; transforming it back to local axis space, and then applying the
;; correction to the local axis space.
(defmethod active-step-once :after ((ent player))
  (let* ((loc (pm-get-trans (local-basis ent)))
         (new-loc (pv-copy loc)))
    (with-multiple-pvec-accessors ((l loc) (n new-loc))
      (when (< ly .03d0) (setf ny .03d0))
      (when (> ly .95d0) (setf ny .95d0))
      (when (< lx .04d0) (setf nx .04d0))
      (when (> lx .96d0) (setf nx .96d0)))
    (pm-set-trans-into (local-basis ent) new-loc)

    ;; XXX sadly, fix up the shield too. I need to think about this a
    ;; little bit more. This is copied from the (ENT SHIP) defmethod
    ;; and I don't like the fact it was copied and both ran, but this
    ;; one copies different information.
    (let ((shield (ship-main-shield ent)))
      (when shield
        (at-location shield ent)))))

;; When the ttl for any drawable hits zero or its world coordinates
;; leave the arena region, it goes stale and will be removed from the
;; game world.
(defmethod active-step-once :after ((ent drawable))
  (with-pvec-accessors (o (pm-get-trans (world-basis ent)))
    (when (or (and (not (null (ttl ent))) (zerop (ttl ent)))
              (< ox -.05d0) (> ox 1.05d0)
              (< oy -.05d0) (> oy 1.05d0))
      (mark-stale ent))))

(defmethod active-step-once :after ((ent ship))
  ;; Shield are going to be drawn right where the ship is, so after the ship
  ;; has had its local-basis fixed up, copy it over to the shield then tell the
  ;; shield to do its own processing.
  (let ((shield (ship-main-shield ent)))
    (when shield
      (at-location shield ent)
      (active-step-once shield))))


