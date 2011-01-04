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

(defclass entity ()
  ((%game-context :initarg :game-context
                  :initform nil
                  :accessor game-context)
   (%points :initarg :points
            :initform 0
            :accessor points)
   (%status :initarg :status
            :initform :alive
            :accessor status)
   (%initial-sparks :initarg :initial-sparks
                    :initform 10
                    :accessor initial-sparks)
   (%additional-sparks :initarg :additional-sparks
                       :initform 50
                       :accessor additional-sparks)
   ;; Unless we specify otherwise, we always try to run whatever
   ;; finishing constructor work we need to on a class by class basis.
   (%auto-finish-construction :initargs :auto-finish-construction
                              :initform t
                              :accessor auto-finish-construction))
  (:documentation "The Entity Class"))

(defclass frame ()
  ( ;; temporal simulation variables
   (%ttl :initarg :ttl
         :initform nil
         :accessor ttl)
   (%ttl-max :initarg :ttl-max
             :initform nil
             :accessor ttl-max)
   ;; physical simulation variables
   (%x :initarg :x
       :initform 0
       :accessor x)
   (%y :initarg :y
       :initform 0
       :accessor y)
   (%dx :initarg :dx
        :initform 0
        :accessor dx)
   (%dy :initarg :dy
        :initform 0
        :accessor dy))
  (:documentation "The Frame Class"))

(defclass shape ()
  ((%primitives :initarg :primitives
                :initform nil
                :accessor primitives))
  (:documentation "The Shape Class"))

(defclass drawable (entity frame shape)
  ()
  (:documentation "The Drawable Class"))

(defclass collidable (drawable)
  ((%radius :initarg :radius
            :initform 0
            :accessor radius))
  (:documentation "The Collidable Class"))

(defclass digit (drawable)
  ()
  (:documentation "The Digit Class"))

(defclass spark (drawable)
  ()
  (:documentation "The Spark Class"))

(defclass brain (collidable)
  ((%until-next-action :initarg :until-next-action
                       :initform 0
                       :accessor until-next-action))
  (:documentation "The Brain Class"))

(defclass powerup (brain)
  ((%main-gun :initarg :main-gun
              :initform nil
              :accessor powerup-main-gun)
   (%main-shield :initarg :main-shield
                 :initform nil
                 :accessor powerup-main-shield))
  (:documentation "The Powerup class"))

(defclass ship (brain)
  ((%main-gun :initarg :main-gun
              :initform nil
              :accessor ship-main-gun)
   (%main-shield :initarg :main-shield
                 :initform nil
                 :accessor ship-main-shield))
  (:documentation "The Ship Class"))

(defclass player (ship)
  ()
  (:documentation "The Player Class"))

(defclass enemy (ship)
  ()
  (:documentation "The Enemy Base Class"))

(defclass enemy-1 (enemy)
  ()
  (:documentation "The Enemy 1 Class"))

(defclass enemy-2 (enemy)
  ()
  (:documentation "The Enemy 2 Class"))

(defclass enemy-3 (enemy)
  ()
  (:documentation "The Enemy 3 Class"))

(defclass shot (brain)
  ()
  (:documentation "The Shot Class"))

(defclass simple-shot (shot)
  ()
  (:documentation "The Simple Shot Class"))

(defclass hardnose-shot (shot)
  ()
  (:documentation "Shots which aren't destroyed by bullets!"))

(defclass super-shot (shot)
  ()
  (:documentation "Shots which aren't destroyed by ships!"))

(defclass shield (brain)
  ((%shots-absorbed :initarg :shots-absorbed
                    :initform 5
                    :accessor shots-absorbed))
  (:documentation "The Shield Base Class"))

(defclass shot-shield (shield)
  ()
  (:documentation "The Shot Shield Class"))

(defclass ship-shield (shield)
  ()
  (:documentation "The Ship Shield Class"))

(defgeneric make-instance-finish (object)
  (:documentation
   "Sometimes a class constructor needs a little extra class specific
work to finish its construction."))

;; In general, we don't do anything special to any object that we create.
(defmethod make-instance-finish (nothing-to-do)
  nothing-to-do)

;; If any random entity sets a ttl-max and nothing more specific changes this
;; method, then assign a random ttl based upon the ttl-max.
(defmethod make-instance-finish ((s entity))
  (when (auto-finish-construction s)
    (when (not (null (ttl-max s)))
      (setf (ttl s) (random (ttl-max s)))))
  s)

;; A powerup's ttl is the random amount up to ttl-max PLUS a constant second
(defmethod make-instance-finish :after ((p powerup))
  (when (auto-finish-construction p)
    (when (ttl p)
      (incf (ttl p) 60)))
  p)

;; Ships that specify a main-shield via keyword need to have them converted
;; to realized shield objects.
(defmethod make-instance-finish ((ent ship))
  (when (auto-finish-construction ent)
    (when (ship-main-shield ent)
      (setf (ship-main-shield ent) (make-entity (ship-main-shield ent)))))
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

(defgeneric mark-dead (entity)
  (:documentation
   "This marks an object as dead."))

(defgeneric deadp (entity)
  (:documentation
   "Returns T if the object is dead."))

(defgeneric mark-stale (entity)
  (:documentation
   "This marks an object as stale (which means out of bounds or the time
to live expired."))

(defgeneric stalep (entity)
  (:documentation
   "Returns T if the object is stale"))

(defgeneric mark-alive (entity)
  (:documentation
   "This marks an object as alive. Isn't called right now since object default
to being alive when they are created."))

(defgeneric alivep (entity)
  (:documentation
   "Returns T is the object is alive."))

(defgeneric distance (left-frame right-frame)
  (:documentation
   "Computes the distance between the origins of two frames"))

(defgeneric step-once (frame)
  (:documentation
   "Performs one step in the simulation of this entity. By default it
will move the x y location of the entity by dx dy in the frame and
will decrease the ttl towards zero if present. It is intended that
before or after methods are used in more specific objects to take
advantage of the simulation step."))

(defgeneric render (drawable scale)
  (:documentation
   "Renders the entity shape with respect to the frame at the scale desired."))

(defgeneric collide (left-collidable right-collidable)
  (:documentation
   "If the left and right entities collide, then invoke their
perform-collide methods."))

(defgeneric perform-collide (collider collidee)
  (:documentation
   "Perform whatever effects need to happen now that it is known this entity
collided with something."))

(defgeneric shoot (ship)
  (:documentation
   "The ship, be it player or enemy or something else, shoots its main gun."))

(defgeneric absorbs (collider shield)
  (:documentation
   "Should return true if the shield absorbs a specific collider"))

(defgeneric think (brain)
  (:documentation
   "If anything needs to think about a future or current action to take, this
is where it is done."))

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
  (sqrt (+ (expt (- (x a) (x b)) 2)
           (expt (- (y a) (y b)) 2))))

;; Perform one physical and temporal step in the simulation
(defmethod step-once ((ent frame))
  (incf (x ent) (dx ent))
  (incf (y ent) (dy ent))
  (unless (null (ttl ent))
    (when (> (ttl ent) 0)
      (decf (ttl ent)))))

;; The player objects get bound to the edges of the screen
(defmethod step-once :after ((ent player))
  (with-accessors ((x x) (y y)) ent
    (when (< y .05) (setf y .05))
    (when (> y .95) (setf y .95))
    (when (< x .03) (setf x .03))
    (when (> x .97) (setf x .97))))

(defmethod step-once :before ((ent enemy))
  (when (> (until-next-action ent) 0)
    (decf (until-next-action ent))))

(defmethod step-once :after ((ent enemy))
  (when (zerop (until-next-action ent))
    (think ent)
    (setf (until-next-action ent) (+ 15 (random 105)))))

;; When the ttl for any drawable hits zero, it goes stale and is
;; removed from the game world.
(defmethod step-once :after ((ent drawable))
  (unless (null (ttl ent))
    (when (zerop (ttl ent))
      (mark-stale ent))))

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

;; if a ship has a shield, render the ship and then the shield (which is
;; at the same place in the world as the ship).
(defmethod render :after ((s ship) scale)
  (when (ship-main-shield s)
    (setf (x (ship-main-shield s)) (x s)
          (y (ship-main-shield s)) (y s))
    (render (ship-main-shield s) scale)))

;; See if two collidables actually collide.
(defmethod collide ((fist collidable) (face collidable))
  (when (and (alivep fist) (alivep face))
    (when (< (distance fist face) (max (radius fist) (radius face)))
      ;; tell both objects what they collided with. In practice this
      ;; means that by default, both will explode.
      (perform-collide fist face))))

;; Primary method is both entities die and explode.
(defmethod perform-collide ((collider collidable) (collidee collidable))
  (mark-dead collider)
  (mark-dead collidee)
  (make-explosion collider)
  (make-explosion collidee))

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
              (mark-dead collider)
              (make-explosion collider)
              (when shield-is-used-up
                (setf (ship-main-shield collidee) nil)))
            (call-next-method)))

      (call-next-method)))

;; When colliding with an enemy, there is a small chance a power up gets
;; created in the place of an enemy.
(defmethod perform-collide :after ((collider collidable)
                                   (collidee enemy))
  (declare (ignorable collider))
  (when (< (random 1.0) .25)
    (make-powerup collidee)))

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
            (make-entity (powerup-main-shield collidee))))))

;; Hardnose shots destroy simple shots and keep going!
(defmethod perform-collide ((collider hardnose-shot)
                            (collidee simple-shot))
  (declare (ignorable collider))
  (mark-dead collidee)
  (make-explosion collidee))

;; Super shots destroy everything and keep going!
(defmethod perform-collide ((collider super-shot)
                            (collidee collidable))
  (declare (ignorable collider))
  (mark-dead collidee)
  (make-explosion collidee))

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

(defmethod think ((ent enemy))
  ;; Instead of doing anything cool like inspect the world, we'll just shoot
  (shoot ent))

(defun load-all-entities (filename)
  (let ((entity-hash (make-hash-table :test #'eq))
        (entities
         (with-open-file (strm filename :direction :input
                               :if-does-not-exist :error)
           ;; Read the symbols from the point of view of this package
           ;; so later when we make-instance it'll work even if the
           ;; user only "used" our package.
           (let ((*package* (find-package 'option-9)))
             (read strm)))))

    (assert (eq (car entities) :entities))

    (loop for i in (cdr entities)
       do
       (setf (gethash (cadr (assoc :kind i)) entity-hash) i))
    entity-hash))



