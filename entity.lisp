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
  ((game-context :initarg :game-context
                 :initform nil
                 :accessor entity-game-context)
   (points :initarg :points
           :initform 0
           :accessor entity-points)
   (status :initarg :status
           :initform :alive
           :accessor entity-status)
   (initial-sparks :initarg :initial-sparks
                   :initform 10
                   :accessor entity-initial-sparks)
   (additional-sparks :initarg :additional-sparks
                      :initform 50
                      :accessor entity-additional-sparks)
   ;; Unless we specify otherwise, we always try to run wahatever
   ;; finishing constructor work we need to on a class by class basis.
   (auto-finish-construction :initargs :auto-finish-construction
                             :initform t
                             :accessor entity-auto-finish-construction))
  (:documentation "The Entity Class"))

(defclass frame ()
  ( ;; temporal simulation variables
   (ttl :initarg :ttl
        :initform nil
        :accessor frame-ttl)
   (ttl-max :initarg :ttl-max
            :initform nil
            :accessor frame-ttl-max)
   ;; physical simulation variables
   (x :initarg :x
      :initform 0
      :accessor frame-x)
   (y :initarg :y
      :initform 0
      :accessor frame-y)
   (dx :initarg :dx
       :initform 0
       :accessor frame-dx)
   (dy :initarg :dy
       :initform 0
       :accessor frame-dy))
  (:documentation "The Frame Class"))

(defclass shape ()
  ((primitives :initarg :primitives
               :initform nil
               :accessor shape-primitives))
  (:documentation "The Shape Class"))

(defclass drawable (entity frame shape)
  ()
  (:documentation "The Drawable Class"))

(defclass collidable (drawable)
  ((radius :initarg :radius
           :initform 0
           :accessor collidable-radius))
  (:documentation "The Collidable Class"))

(defclass digit (drawable)
  ()
  (:documentation "The Digit Class"))

(defclass spark (drawable)
  ()
  (:documentation "The Spark Class"))

(defclass brain (collidable)
  ((until-next-action :initarg :until-next-action
                      :initform 0
                      :accessor brain-until-next-action))
  (:documentation "The Brain Class"))

(defclass powerup (brain)
  ((main-gun :initarg :main-gun
             :initform nil
             :accessor powerup-main-gun)
   (main-shield :initarg :main-shield
                :initform nil
                :accessor powerup-main-shield))
  (:documentation "The Powerup class"))

(defclass ship (brain)
  ((main-gun :initarg :main-gun
             :initform nil
             :accessor ship-main-gun)
   (main-shield :initarg :main-shield
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
  ((shots-absorbed :initarg :shots-absorbed
                   :initform 5
                   :accessor shield-shots-absorbed))
  (:documentation "The Shield Base Class"))

(defclass shot-shield (shield)
  ()
  (:documentation "The Shot Shield Class"))

(defclass ship-shield (shield)
  ()
  (:documentation "The Ship Shield Class"))

(defgeneric make-entity-finish (object)
  (:documentation
   "Sometimes a class constructor needs a little extra class specific
work to finish its construction."))

;; Take the default arguments, and the arguments I supplied, and
;; override any default arguments with supplied ones and return the
;; list.
(defun override-args (default overrides)
  (let ((arghash (make-hash-table))
        (default-key/value (group default 2))
        (overrides-key/value (group overrides 2)))

    ;; First, we put all default argument/values into a hash table.
    (loop for (key value) in default-key/value
       do (setf (gethash key arghash) value))

    ;; Then we overlay the overrides
    (loop for (key value) in overrides-key/value
       do (setf (gethash key arghash) value))

    ;; Then we collect everything into the correctly applicable initarg form.
    (loop for k being the hash-keys in arghash using (hash-value v)
       appending `(,k ,v))))

;; In general, we don't do anything special to any object that we create.
(defmethod make-entity-finish (nothing-to-do)
  nothing-to-do)

;; If any random entity sets a ttl-max and nothing more specific changes this
;; method, then assign a random ttl based upon the ttl-max.
(defmethod make-entity-finish ((s entity))
  (when (entity-auto-finish-construction s)
    (with-accessors ((ttl frame-ttl) (ttl-max frame-ttl-max)) s
      (when (not (null ttl-max))
        (setf ttl (random ttl-max)))))
  s)

;; A powerup's ttl is the random amount up to ttl-max PLUS a constant second
(defmethod make-entity-finish :after ((p powerup))
  (when (entity-auto-finish-construction p)
    (with-accessors ((ttl frame-ttl) (ttl-max frame-ttl-max)) p
      (when ttl
        (incf ttl 60))))
  p)

;; Ships that specify a main-shield via keyword need to have them converted
;; to realized shield objects.
(defmethod make-entity-finish ((ent ship))
  (when (entity-auto-finish-construction ent)
    (with-accessors ((main-shield ship-main-shield)) ent
      (when main-shield
        (setf main-shield (make-entity main-shield)))))
  ent)

;; For enemy-3 there is only a 25 percent chance that it actually has
;; the shield specified in the option-9.dat file. If we decide it
;; shouldn't have a shield, the we set the main-shield to null which
;; makes the above generic method a noop.
(defmethod make-entity-finish :before ((ent enemy-3))
  (when (entity-auto-finish-construction ent)
    (with-accessors ((main-shield ship-main-shield)) ent
      (when (<= (random 1.0) .75)
        (setf main-shield nil))))
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
      (make-entity-finish
       (apply #'make-instance cls :game-context *game*
              (override-args initargs override-initargs))))))

(defgeneric step-entity (frame)
  (:documentation
   "Performs one step in the simulation of this entity. By default it
will move the x y location of the entity by dx dy in the frame. It in
intended that before or after methods are used in more specific objects
to take advantage of the simulation step."))

(defgeneric render-entity (drawable scale)
  (:documentation
   "Renders the entity shape with respect to the frame at the scale desired."))

(defgeneric collide-entity (left-collidable right-collidable)
  (:documentation
   "If the left and right entities collide, then invoke their do-collide-entity
generic functions."))

(defgeneric perform-collide-entity (collider collidee)
  (:documentation
   "Perform whatever effects need to happen now that it is known this entity
collided with something."))

(defgeneric ship-fires (ship)
  (:documentation
   "The ship, be it player or enemy or something else, fires its main gun."))

(defgeneric shield-absorbs (collider shield)
  (:documentation
   "Should return true if the shield absorbs a specific collider"))

;; Perform one physical and temporal step in the simulation
(defmethod step-entity ((ent frame))
  (with-accessors ((x frame-x) (y frame-y) (dx frame-dx) (dy frame-dy)
                   (ttl frame-ttl)) ent
    (incf x dx)
    (incf y dy)
    (unless (null ttl)
      (when (> ttl 0)
        (decf ttl)))))

;; The player objects gets clipped to the edges of the screen
(defmethod step-entity :after ((ent player))
  (with-accessors ((x frame-x) (y frame-y)) ent
    (when (< y .05) (setf y .05))
    (when (> y .95) (setf y .95))
    (when (< x .03) (setf x .03))
    (when (> x .97) (setf x .97))))

(defmethod step-entity :before ((ent enemy))
  (with-accessors ((until-next-action brain-until-next-action)) ent
    (when (> until-next-action 0)
      (decf until-next-action))))

(defmethod step-entity :after ((ent enemy))
  (with-accessors ((until-next-action brain-until-next-action)) ent
    (when (zerop until-next-action)
      (think-entity ent)
      (setf until-next-action (+ 15 (random 105))))))

;; When the ttl for any drawable hits zero, it goes stale and is
;; removed from the game world.
(defmethod step-entity :after ((ent drawable))
  (with-accessors ((ttl frame-ttl) (status entity-status)) ent
    (unless (null ttl)
      (when (zerop ttl)
        (setf status :stale)))))

(defmethod render-entity ((ent drawable) scale)
  (with-accessors ((x frame-x) (y frame-y) (dx frame-dx) (dy frame-dy)) ent
    (destructuring-bind (sx sy) scale
      ;; render a list of possibly differing primitives associated with this
      ;; shape
      (mapc #'(lambda (primitive)
                (gl:with-primitive (car primitive)
                  ;; render each specific primitivve
                  (mapc #'(lambda (vertex/color)
                            (destructuring-bind ((vx vy) (cx cy cz))
                                vertex/color
                              (gl:color cx cy cz)
                              (gl:vertex (+ x (* vx sx)) (+ y (* vy sy)) 0.0)))
                        (cdr primitive))))
            (shape-primitives ent)))))

;; if a ship has a shield, render the ship and then the shield (which is
;; at the same place in the world as the ship).
(defmethod render-entity :after ((s ship) scale)
  (with-accessors ((main-shield ship-main-shield)
                   (ox frame-x) (oy frame-y)) s
    (when main-shield
      (with-accessors ((x frame-x) (y frame-y)) main-shield
        (setf x ox
              y oy)
        (render-entity main-shield scale)))))

;; See if two collidables actually collide.
(defmethod collide-entity ((fist collidable) (face collidable))
  (with-accessors ((lx frame-x) (ly frame-y)
                   (lradius collidable-radius)
                   (lstatus entity-status)) fist
    (with-accessors ((rx frame-x) (ry frame-y)
                     (rradius collidable-radius)
                     (rstatus entity-status)) face
      (when (and (eq lstatus :alive) (eq rstatus :alive))
        (let ((dist (sqrt (+ (expt (- lx rx) 2) (expt (- ly ry) 2)))))
          (when (< dist (max lradius rradius))
            ;; tell both objects what they collided with. In practice this
            ;; means that by default, both will explode.
            (perform-collide-entity fist face)))))))

;; Primary method is both entities die and explode.
(defmethod perform-collide-entity ((collider collidable) (collidee collidable))
  (with-accessors ((lstatus entity-status)
                   (linit entity-initial-sparks)
                   (ladd entity-additional-sparks)) collider
    (with-accessors ((rstatus entity-status)
                     (rinit entity-initial-sparks)
                     (radd entity-additional-sparks)) collidee
      (setf lstatus :dead
            rstatus :dead)
      (make-explosion (entity-game-context collider) collider linit ladd)
      (make-explosion (entity-game-context collidee) collidee rinit radd))))

;; By default, the collider will not be absorbed by the shield and the shield
;; will be considered used up.
(defmethod shield-absorbs (collider (collidee shield))
  (values nil t))

;; A shot shield will only absorb shots
(defmethod shield-absorbs ((collider shot) (collidee shot-shield))
  (with-accessors ((shots-absorbed shield-shots-absorbed)) collidee
    (when (> shots-absorbed 0)
      (decf shots-absorbed))
    (values t (zerop shots-absorbed))))

;; A ship shield will absorb shots
(defmethod shield-absorbs ((collider shot) (collidee ship-shield))
  (with-accessors ((shots-absorbed shield-shots-absorbed)) collidee
    (when (> shots-absorbed 0)
      (decf shots-absorbed))
    (values t (zerop shots-absorbed))))

;; A ship shield will also absorb other ships
(defmethod shield-absorbs ((collider ship) (collidee ship-shield))
  (with-accessors ((shots-absorbed shield-shots-absorbed)) collidee
    (when (> shots-absorbed 0)
      (decf shots-absorbed))
    (values t (zerop shots-absorbed))))

;; Here we handle the processing of a something hitting a ship which might
;; or might not have a shield.
(defmethod perform-collide-entity :around ((collider collidable)
                                           (collidee ship))
  (with-accessors ((lstatus entity-status)
                   (linit entity-initial-sparks)
                   (ladd entity-additional-sparks)) collider
    (with-accessors ((main-shield ship-main-shield)) collidee
      (if main-shield
          (multiple-value-bind (absorbedp shield-is-used-up)
              (shield-absorbs collider main-shield)
            (if absorbedp
                (progn
                  (setf lstatus :dead)
                  (make-explosion (entity-game-context collider)
                                  collider linit ladd)
                  (when shield-is-used-up
                    (setf main-shield nil)))
                (call-next-method)))

          (call-next-method)))))

;; When colliding with an enemy, there is a small chance a power up gets
;; created in the place of an enemy.
(defmethod perform-collide-entity :after ((collider collidable)
                                          (collidee enemy))
  (declare (ignorable collider))
  (when (< (random 1.0) .25)
    (make-powerup (entity-game-context collidee) collidee)))

;; The player gets the new weapon as denoted by the powerup, and the
;; powerup goes immediately stale. NOTE: If I change the collider type
;; to ship here, then ANY ship can get the powerup and its effects (as
;; long as I collide the enemies to the powerups in the main loop of
;; the game. However, I haven't coded the right geometries for the
;; ship shields of a good means to choose between them. So for now,
;; only the player can get powerups.
(defmethod perform-collide-entity ((collider player) (collidee powerup))
  (with-accessors ((main-gun-of-player ship-main-gun)
                   (main-shield-of-player ship-main-shield)) collider
    (with-accessors ((main-gun-of-powerup powerup-main-gun)
                     (main-shield-of-powerup powerup-main-shield)
                     (x frame-x) (y frame-y) (status entity-status)) collidee
      ;; A powerup can only be used ONCE
      (when (not (eq status :stale))
        (setf status :stale)
        (when main-gun-of-powerup
          (setf main-gun-of-player main-gun-of-powerup))
        (when main-shield-of-powerup
          (setf main-shield-of-player
                (make-entity main-shield-of-powerup)))))))

;; Hardnose shots destroy simple shots and keep going!
(defmethod perform-collide-entity ((collider hardnose-shot)
                                   (collidee simple-shot))
  (declare (ignorable collider))
  (with-accessors ((status entity-status) (init entity-initial-sparks)
                   (add entity-additional-sparks)) collidee
    (setf status :dead)
    (make-explosion (entity-game-context collidee) collidee init add)))

;; Super shots destroy everything and keep going!
(defmethod perform-collide-entity ((collider super-shot)
                                   (collidee collidable))
  (declare (ignorable collider))
  (with-accessors ((status entity-status) (init entity-initial-sparks)
                   (add entity-additional-sparks)) collidee
    (setf status :dead)
    (make-explosion (entity-game-context collidee) collidee init add)))

;; The method for when the player ship fires
(defmethod fires-ship ((ship player))
  (with-accessors ((player-shots game-player-shots)) (entity-game-context ship)
    (with-accessors ((x frame-x) (y frame-y) (main-gun ship-main-gun)) ship
      (let ((shot (make-entity main-gun
                               :x x :y (+ y .03) :dx 0 :dy .022)))
        (push shot player-shots))))
  (modify-score (entity-game-context ship) -1))

;; The method for when the enemy ship fires.
(defmethod fires-ship ((ship enemy))
  (with-accessors ((enemy-shots game-enemy-shots)) (entity-game-context ship)
    (with-accessors ((x frame-x) (y frame-y) (main-gun ship-main-gun)
                     (dy frame-dy)) ship
      (let ((shot (make-entity main-gun
                               :x x :y (- y .03)
                               :dx 0 :dy (+ (- (+ .005 (random .005))) dy))))
        (push shot enemy-shots)))))

(defmethod think-entity ((ent enemy))
  ;; Instead of doing anything cool like inspect the world, we'll just fire
  (fires-ship ent))

(defun load-all-entities (filename)
  (let ((entity-hash (make-hash-table :test #'eq))
        (entities
         (with-open-file (strm filename :direction :input
                               :if-does-not-exist nil)
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



