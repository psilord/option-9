;; Copyright 2010 Peter K. Keller (psilord@cs.wisc.edu)
;;
;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License. You may
;; obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
;; or implied. See the License for the specific language governing
;; permissions and limitations under the License.

(in-package #:option-9)

;; In general, we don't do anything special to any object that we create.
(defmethod make-instance-finish (nothing-to-do)
  nothing-to-do)

;; If any random entity sets a ttl-max and nothing more specific changes this
;; method, then assign a random ttl based upon the ttl-max.
(defmethod make-instance-finish :after ((s ephemeral))
  (when (not (null (ttl-max s)))
    (setf (ttl s) (random (ttl-max s))))
  s)

(defmethod make-instance-finish :after ((e entity))
  (when (auto-finish-construction e)
    (setf (hit-points e) (max-hit-points e)))
  e)

;; A powerup's ttl is the random amount up to ttl-max PLUS a constant second
(defmethod make-instance-finish :after ((p powerup))
  (when (auto-finish-construction p)
    (when (ttl p)
      (incf (ttl p) 60)))
  p)

;; Ships that specify a main-shield via keyword need to have them converted
;; to realized shield objects.
(defmethod make-instance-finish :after ((ent ship))
  (when (auto-finish-construction ent)
    (when (ship-main-shield ent)
      (setf (ship-main-shield ent) (make-entity (ship-main-shield ent))))
    (when (ship-passive-gun ent)
      (setf (ship-passive-gun ent)
            (make-entity (ship-passive-gun ent)))))
  ent)

;; For enemy-3 there is only a 25 percent chance that it actually has
;; the shield specified in the option-9.dat file. If we decide it
;; shouldn't have a shield, the we set the main-shield to null which
;; makes the above generic method a noop.
(defmethod make-instance-finish :before ((ent enemy-3))
  (when (auto-finish-construction ent)
    (when (<= (random 1.0) .75)
      (setf (ship-main-shield ent) nil)))
  ent)


(defmethod make-instance-finish :after ((ent tesla-field))
  (setf (power-range ent) (power-range ent))
  (setf (power-lines ent) (power-lines ent))
  ent)

;; A factory constructor to make me any object of any kind read into
;; *all-entities*. Each entity also knows the game context in which it
;; will be placed. This allows the generic methods of entities to
;; inspect the game universe or insert effects into the game like
;; sparks.
(defun make-entity (kind &rest override-initargs)
  (multiple-value-bind (info present) (gethash kind *all-entities*)
    (assert present)
    (let ((found-kind (cadr (assoc :kind info)))
          (cls (cadr (assoc :class info)))
          (initargs (cdr (assoc :initargs info))))
      (assert (eq found-kind kind))
      (assert cls)
      (make-instance-finish
       (apply #'make-instance cls :game-context *game*
              ;; The values of the override arguments are accepted
              ;; first when in a left to right ordering in the
              ;; argument list in concordance with the ANSI spec.
              (append override-initargs initargs))))))

;; Marking and checking various status about the entities.
(defmethod mark-dead ((ent entity))
  (setf (status ent) :dead))

(defmethod deadp ((ent entity))
  (eq (status ent) :dead))

(defmethod mark-stale ((ent entity))
  (setf (status ent) :stale))

(defmethod stalep ((ent entity))
  (eq (status ent) :stale))

(defmethod mark-alive ((ent entity))
  (setf (status ent) :alive))

(defmethod alivep ((ent entity))
  (eq (status ent) :alive))

(defmethod distance ((a frame) (b frame))
  (let ((non-normalized
         (let ((factor-1 (- (x a) (x b)))
               (factor-2 (- (y a) (y b))))
           (+ (* factor-1 factor-1)
              (* factor-2 factor-2)))))
    (values (sqrt non-normalized) non-normalized)))

;; If something is told to be dead, we kill it and blow it up.
(defmethod die ((ent entity))
  (mark-dead ent)
  (make-explosion ent))

;; When an enemy dies, there is a small chance a power up or a mine
;; gets created in the spot of death.
(defmethod die :after ((ent enemy))
  (let ((chance (random 1.0)))
    (cond
      ((< chance .25)
       (make-powerup ent))
      ((and (>= chance .25) (< chance .50))
       (make-mine ent)))))

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

;; Perform one physical and temporal step in the simulation
(defmethod active-step-once ((ent frame))
  (incf (x ent) (dx ent))
  (incf (y ent) (dy ent))
  (unless (null (ttl ent))
    (when (> (ttl ent) 0)
      (decf (ttl ent)))))

;; The player objects get bound to the edges of the screen
(defmethod active-step-once :after ((ent player))
  (with-accessors ((x x) (y y)) ent
    (when (< y .05) (setf y .05))
    (when (> y .95) (setf y .95))
    (when (< x .03) (setf x .03))
    (when (> x .97) (setf x .97))))

;; When the ttl for any drawable hits zero, it goes stale and is
;; removed from the game world.
(defmethod active-step-once :after ((ent drawable))
  (unless (null (ttl ent))
    (when (zerop (ttl ent))
      (mark-stale ent))))

;; Some shield have internal behavior which needs simulating
(defmethod active-step-once :after ((ent ship))
  (when (ship-main-shield ent)
    (active-step-once (ship-main-shield ent))))

;; Field mines only move once in their direction and then don't move again
;; unless they are being in collision with a field. If so, they are told
;; at that time to move again.
(defmethod active-step-once :after ((ent field-mine))
  (setf (dx ent) 0.0
        (dy ent) 0.0))

;; Base shields don't do anything in their simulation step
(defmethod active-step-once ((s shield))
  nil)

;; Usually nothing has a passive step
(defmethod passive-step-once (object)
  nil)

;; If the ship has a passive gun, then simulate it with the ship as
;; the source charge and other brains as the charges
(defmethod passive-step-once :after ((ent ship))
  (when (ship-passive-gun ent)
    (with-accessors ((players players)
                     (enemy-mines enemy-mines)
                     (enemies enemies)
                     (enemy-shots enemy-shots))
        (game-context ent)

      (let ((ents (loop for i in (list players enemy-mines enemies enemy-shots)
                     appending i)))
        (generate (ship-passive-gun ent) ent ents)))))


(defmethod render ((ent drawable) scale)
  (with-accessors ((x x) (y y) (dx dx) (dy dy)) ent
    (destructuring-bind (sx sy) scale
      ;; render a list of possibly differing primitives associated with this
      ;; shape
      (mapc #'(lambda (primitive)
                (gl:with-primitive (car primitive)
                  ;; render each specific primitive
                  (mapc #'(lambda (vertex/color)
                            (destructuring-bind ((vx vy) (cx cy cz))
                                vertex/color
                              (gl:color cx cy cz)
                              (gl:vertex (+ x (* vx sx)) (+ y (* vy sy)) 0.0)))
                        (cdr primitive))))
            (primitives ent)))))

;; Ships have various other things which need to be renderede as well. So
;; do them...
(defmethod render :after ((s ship) scale)
  ;; If there is a passive-gun (which is field-like) render that.
  (when (ship-passive-gun s)
    (setf (x (ship-passive-gun s)) (x s)
          (y (ship-passive-gun s)) (y s))
    (render (ship-passive-gun s) scale))

  ;; If there is a shield, render that too.
  (when (ship-main-shield s)
    (setf (x (ship-main-shield s)) (x s)
          (y (ship-main-shield s)) (y s))
    (render (ship-main-shield s) scale))

  ;; If the hit-points is not equal to the maximum hit points, then
  ;; render the health bar
  (when (/= (hit-points s) (max-hit-points s))
    (destructuring-bind (xscale yscale) scale
      (gl:line-width 4.0)
      (gl:with-primitive :lines
        (let* ((per (/ (hit-points s) (max-hit-points s)))
               (invper (- 1.0 per)))
          (gl:color 1 1 1)
          ;; start life bar
          (gl:vertex (+ (x s) (* -4 xscale))
                     (+ (y s) (* 5 yscale))
                     0)
          ;; End life bar
          (gl:vertex (+ (x s) (* (- 4 (* 8 invper)) xscale))
                     (+ (y s) (* 5 yscale))
                     0)

          ;; start filler
          (gl:color .2 .2 .2)
          (gl:vertex (+ (x s) (* (- 4 (* 8 invper)) xscale))
                     (+ (y s) (* 5 yscale))
                     0)

          (gl:vertex (+ (x s) (* 4 xscale))
                     (+ (y s) (* 5 yscale))
                     0)))
      (gl:line-width 1.0))))


;; See if two collidables physically collide.
(defmethod collide ((fist collidable) (face collidable))
  (when (and (alivep fist) (alivep face))
    (when (< (distance fist face) (max (radius fist) (radius face)))
      ;; tell both objects what they collided with. In practice this
      ;; means that by default, both will explode.
      (perform-collide fist face))))

;; Primary method is both entities damage each other and one, both, or
;; neither may die.
(defmethod perform-collide ((collider collidable) (collidee collidable))
  (damage collider collidee))

;; By default, the shield will absorb the collider.
(defmethod absorbs (collider (collidee shield))
  (when (> (shots-absorbed collidee) 0)
    (decf (shots-absorbed collidee)))
  (values t (zerop (shots-absorbed collidee))))

;; However, if a ship hits a shot-shield, the shield doesn't stop it
;; and the shield is destroyed.
(defmethod absorbs ((collider ship) (collidee shot-shield))
  (values nil t))

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
            (make-entity (powerup-main-shield collidee))))

    ;; If the powerup has a health level, apply it to the player.
    (incf (hit-points collider) (powerup-health-level collidee))
    (when (> (hit-points collider) (max-hit-points collider))
      (setf (hit-points collider) (max-hit-points collider)))


    ;; If I already have this weapon, then increase its power if possible.
    (when (powerup-passive-gun collidee)
      (if (ship-passive-gun collider)
          (increase-power-randomly (ship-passive-gun collider))
          (setf (ship-passive-gun collider)
                (make-entity (powerup-passive-gun collidee)))))))

;; The method for when the player ship shoots
(defmethod shoot ((ship player))
  (let ((shot (make-entity (ship-main-gun ship)
                           :x (x ship) :y (+ (y ship) .03)
                           :dx 0 :dy .022)))
    (push shot (player-shots (game-context ship))))
  (modify-score (game-context ship) -1))

;; The method for when the enemy ship shoots.
(defmethod shoot ((ship enemy))
  (let ((shot (make-entity (ship-main-gun ship)
                           :x (x ship) :y (- (y ship) .03)
                           :dx 0
                           :dy (+ (- (+ .005 (random .005))) (dy ship)))))
    (push shot (enemy-shots (game-context ship)))))

;; By default, nothing thinks
(defmethod think (ent)
  nil)

;; But enemies think!
(defmethod think ((ent enemy))
  (when (until-next-action ent)
    (cond
      ((zerop (until-next-action ent))
       (idea ent)
       (setf (until-next-action ent) (+ 15 (random 105))))
      (t
       (decf (until-next-action ent))))))

;; and enemies have ideas about what they want to do in the world...
(defmethod idea ((ent enemy))
  ;; Instead of doing anything cool like inspect the world, we'll just shoot
  (shoot ent))

;; This takes a relative filename based at the installation location
;; of the package.
(defun load-all-entities (filename)
  (let ((entity-hash (make-hash-table :test #'eq))
        (entities
         (with-open-file (strm
                          (asdf:system-relative-pathname :option-9 filename)
                          :direction :input
                          :if-does-not-exist :error)
           ;; Read the symbols from the point of view of this package
           ;; so later when we make-instance it'll work even if the
           ;; user only "used" our package.
           (let ((*package* (find-package 'option-9)))
             (read strm)))))

    (assert (eq (car entities) :entities))

    (loop for i in (cdr entities) do
         (setf (gethash (cadr (assoc :kind i)) entity-hash) i))
    entity-hash))


