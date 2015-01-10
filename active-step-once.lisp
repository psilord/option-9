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
      (mmi (world-basis f) (world-basis (parent f)) (local-basis f))
      (mcpi (world-basis f) (local-basis f))))

;; XXX Transform each one of these operations into VERBS, it'll make clipping
;; and other effects much easier.
(defmethod update-local-basis ((f frame))
  ;; TODO, add some local matricies here for memory storage
  ;; optimization with MTR.

  ;; Translate the current frame in the direction of DV.
  (mmi (local-basis f) (mtr (dv f)) (local-basis f))
  ;; Zero it, so if no other displacements are added, we add nothing.
  (pv-clear-into (dv f))

  ;; Rotate the one time local axis rotation not related to rotatingp, if any.
  (mlari (local-basis f) (local-basis f) (dr f))
  ;; Then zero it
  (pv-clear-into (dr f))

  ;; Add in the incremental displacement not related to the basis
  ;; directions vector and do the decay of it
  (unless (pv-zero-p (dtv f))
    (mmi (local-basis f) (mtr (dtv f)) (local-basis f))
    (dtv-decay f))

  ;; perform the incremental flying vector.
  (when (flyingp f)
    (mfi (local-basis f) (local-basis f) (dfv f)))

  ;; perform the incremental local axis rotation vector.
  (when (rotatingp f)
    (mlari (local-basis f) (local-basis f) (drv f))))

;; Perform one physical and temporal step in the simulation
(defmethod active-step-once ((ent frame))

  ;; Update the physical nature
  (update-local-basis ent)

  ;; Update the temporal nature, all in usecs
  (unless (null (ttl ent))
    (when (plusp (ttl ent))
      (decf (ttl ent) *dt-us*)))

  ;; If the charge is charging, then charge it at the rate desired.
  (when (and (chargeablep ent) (chargingp ent))

    ;; This interpolant is the amount of time to add that will get us
    ;; to 1.0 in the charge-time required, but at our simulation
    ;; frequency.
    (incf (charge-percentage ent) (* (/ 1 (charge-time ent)) *dt-us*))
    (when (>= (charge-percentage ent) 1.0)
      (setf (charge-percentage ent) 1.0)))

  ;; If the charge is decaying, then decay it at the rate desired.
  (when (and (decayablep ent) (decayingp ent))
    ;; This interpolant is the amount of time to add that will get us
    ;; to 0.0 in the decay-time required, but at our simulation
    ;; frequency.
    (decf (charge-percentage ent) (* (/ 1 (decay-time ent)) *dt-us*))
    (when (<= (charge-percentage ent) 0.0)
      (setf (charge-percentage ent) 0.0)))

  ;; If it is invulnerable, decrement is usecs how long it has left to be so.
  (unless (null (inttl ent))
    (when (plusp (inttl ent))
      (decf (inttl ent) *dt-us*))))

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
  (let* ((loc (matrix-translate-get (local-basis ent)))
         (new-loc (pv-copy loc)))
    (with-multiple-pvec-accessors ((l loc) (n new-loc))
      (when (< ly 3d0) (setf ny 3d0))
      (when (> ly 95d0) (setf ny 95d0))
      (when (< lx 4d0) (setf nx 4d0))
      (when (> lx 96d0) (setf nx 96d0)))
    (mtrsi (local-basis ent) new-loc)))


;; Show that a shot has been significantly charged by having it leave a trail.
(defmethod active-step-once :after ((shot shot))
  (when (> (charge-percentage shot) .25)
    (spawn 'sp-sparks
           :insts/sparks shot (game-context shot)
           :velocity-factor .1d0
           :ttl-max (in-usecs .2)
           :num-sparks 1)))


;; When the ttl for any drawable hits zero or its world coordinates
;; leave the arena region, it goes stale and will be removed from the
;; game world.
(defmethod active-step-once :after ((ent drawable))
  (with-pvec-accessors (o (matrix-translate-get (world-basis ent)))
    (when (or (and (not (null (ttl ent))) (<= (ttl ent) 0))
              (< ox -5d0) (> ox 105d0)
              (< oy -5d0) (> oy 105d0))
      (mark-stale ent))))
