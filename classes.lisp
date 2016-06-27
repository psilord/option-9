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

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; There will be a single one of these objects
(defclass assets ()
  (;; The processed :entities form which is a hash table keyed by
   ;; :instance and whose value is a pile of info about how to
   ;; initialize that :instance into a real CLOS instance.
   (%entities :initarg :entities
              :initform nil
              :accessor entities)
   ;; What are the roles any entity can assume?
   (%defined-roles :initarg :defined-roles
                   :initform nil
                   :accessor defined-roles)
   ;; How do roles collide with each other and in what order?
   (%collision-plan :initarg :collision-plan
                    :initform nil
                    :accessor collision-plan)
   ;; How do I convert generic-instance names to grounded-instance-names?
   (%instance-specialization-map :initarg :instance-specialization-map
                                 :initform nil
                                 :accessor instance-specialization-map)
   ;; The storage of all of the geometries for everything drawable in the game.
   (%geometries :initarg :geometries
                :initform nil
                :accessor geometries)
   ;; The processed :instance-equivalences form which is a hash table
   ;; keyed by an instance equivalencey keyword and whose value is a vector
   ;; of :instances that are all equivalent.
   (%insts/equiv :initarg :insts/equiv
                 :initform nil
                 :accessor insts/equiv)))

;; This hold a keyword which identifies what game instance name this object
;; will have. It is at the top of the game object hierarchy.
(defclass instance ()
  ((%instance-name :initarg :instance-name
                   :initform :unknown-instance-name
                   :reader instance-name))
  (:documentation "All classes that can be described as :instance in
the config files have this type as its base so we can do certain
instance specializations in a data driven manner. In practice, this means
the class must be of type DRAWABLE or more specialized."))

;; This structure defines a pile of constraints and restrictions about
;; a rate. A rate can be things like "units per second", or "radians
;; per second", or "thingies per second". It is a vector so COPY-SEQ works on
;; it when it is used in the assets files...
(defstruct (ratespec (:type vector))
  (initval 0d0 :type double-float)
  (minval most-negative-double-float :type double-float)
  (maxval most-positive-double-float :type double-float)
  (rand-minoff 0d0 :type double-float)
  (rand-maxoff 0d0 :type double-float))

(defclass entity ()
  ((%id :initarg :id
        :initform (new-id)
        :accessor id)
   ;; Entities are grouped into roles that facilitate collision
   ;; detection among other uses.
   (%roles :initarg :roles
           :initform (list :general)
           :accessor roles)
   (%game-context :initarg :game-context
                  :initform nil
                  :accessor game-context)
   (%max-hit-points :initarg :max-hit-points
                    :initform 10
                    :accessor max-hit-points)
   (%hit-points :initarg :hit-points
                :initform 10
                :accessor hit-points)
   (%damage-points :initarg :damage-points
                   :initform 10
                   :accessor damage-points)
   (%points :initarg :points
            :initform 0
            :accessor points)
   (%status :initarg :status
            :initform :alive
            :accessor status)
   (%charge :initarg :charge
            :initform -1
            :accessor charge)
   ;; When some entities blow up, they blow up into shrapnel. These name
   ;; instance names of thigns which do so.
   (%shrapnel :initarg :shrapnel
              :initform nil
              :accessor shrapnel)
   ;; And this is how often it'll blow up into shrapnel.
   (%shrapnel-generation :initarg :shrapnel-generation
                         :initform nil
                         :accessor shrapnel-generation)

   (%initial-sparks :initarg :initial-sparks
                    :initform 10
                    :accessor initial-sparks)
   (%additional-sparks :initarg :additional-sparks
                       :initform 50
                       :accessor additional-sparks)
   ;; Should I draw a hud around the entity on the screen?
   (%hudp :initarg :hudp
          :initform nil
          :accessor hudp)

   ;; These next slots represent information about what this entity
   ;; could do in terms of movement, but not what it is actually
   ;; doing. Information needs to be moved from here to the FRAME
   ;; slots, to actually enact the movements. The main purpose of these
   ;; slots is to remove hard coded constants about entity movement and
   ;; allow specification of them in the asset files.

   ;; These are in units per second speed defaults in every direction
   (%forward-speed-spec
    :initarg :forward-speed-spec
    :initform (make-ratespec)
    :accessor forward-speed-spec)
   ;; Set up in the initialize-instance :after method.
   (%forward-speed :initarg :forward-speed
                   :initform 0d0
                   :accessor forward-speed)

   (%backward-speed-spec
    :initarg :backward-speed-spec
    ;; current-speed init-speed min-speed max-speed rand-off-min rand-off-max
    :initform (make-ratespec)
    :accessor backward-speed-spec)
   ;; Set up in the initialize-instance :after method.
   (%backward-speed :initarg :backward-speed
                    :initform 0d0
                    :accessor backward-speed)

   (%strafe-left-speed-spec
    :initarg :strafe-left-speed-spec
    ;; current-speed init-speed min-speed max-speed rand-off-min rand-of-max
    :initform (make-ratespec)
    :accessor strafe-left-speed-spec)
   ;; Set up in the initialize-instance :after method.
   (%strafe-left-speed :initarg :strafe-left-speed
                       :initform 0d0
                       :accessor strafe-left-speed)

   (%strafe-right-speed-spec
    :initarg :strafe-right-speed-spec
    ;; current-speed init-speed min-speed max-speed rand-off-min rand-off-max
    :initform (make-ratespec)
    :accessor strafe-right-speed-spec)
   ;; Set up in the initialize-instance :after method.
   (%strafe-right-speed :initarg :strafe-right-speed
                        :initform 0d0
                        :accessor strafe-right-speed)

   (%up-speed-spec
    :initarg :up-speed-spec
    ;; current-speed init-speed min-speed max-speed rand-off-min rand-off-max
    :initform (make-ratespec)
    :accessor up-speed-spec)
   ;; Set up in the initialize-instance :after method.
   (%up-speed :initarg :up-speed
              :initform 0d0
              :accessor up-speed)

   (%down-speed-spec
    :initarg :down-speed-spec
    ;; current-speed init-speed min-speed max-speed rand-off-min rand-off-max
    :initform (make-ratespec)
    :accessor down-speed-spec)
   ;; Set up in the initialize-instance :after method.
   (%down-speed :initarg :down-speed
                :initform 0d0
                :accessor down-speed)

   )
  (:documentation "The Entity Class"))

(defclass temporal ()
  ;; temporal simulation variables, all times are in usecs.
  (;; Time To Live until something happens.
   (%ttl :initarg :ttl
         :initform nil
         :accessor ttl)
   (%ttl-min :initarg :ttl-min
             :initform 0
             :accessor ttl-min)
   (%ttl-max :initarg :ttl-max
             :initform nil
             :accessor ttl-max)

   ;; A "charge" is a value that varies between 0.0 and 1.0 and it
   ;; represents the "fullness" of the charge.
   ;;
   ;; Should this object be subject to a charging effect?
   (%chargeablep :initarg :chargeablep
                 :initform nil
                 :accessor chargeablep)
   ;; Is this object currently charging?
   (%chargingp :initarg :chargingp
               :initform nil
               :accessor chargingp)
   ;; Should this object be subject to a decaying effect?
   (%decayablep :initarg :decayablep
                :initform nil
                :accessor decayablep)
   ;; Is this object currently decaying?
   (%decayingp :initarg :decayingp
               :initform nil
               :accessor decayingp)
   ;; Total linear time to charge from a 0.0 charge to 1.0 charge in usecs.
   (%charge-time :initarg :charge-time
                 :initform (in-usecs 2.0) ;; must be correct when chargeablep
                 :accessor charge-time)
   ;; Total linear time to decay from a 1.0 charge to 0.0 charge in usecs.
   (%decay-time :initarg :decay-time
                :initform 0
                :accessor decay-time)
   ;; The current percentage of charge for this object.
   ;; The min charge of anything is 0.0, the max charge is 1.0.
   (%charge-percentage :initarg :charge-percentage
                       :initform 0.0
                       :accessor charge-percentage)

   ;; invulnerability time to live in usecs, when > 0 is considered invulnerable
   (%inttl :initarg :inttl
           :initform 0
           :accessor inttl))
  (:documentation
   "The Temporal Class. Used for things which need to understand time."))

;; Used in the field class to trace vector streams
(defclass location ()
  ((%x :initarg :x
       :initform 0d0
       :accessor x)
   (%y :initarg :y
       :initform 0d0
       :accessor y)
   (%z :initarg :z
       :initform 0d0
       :accessor z)
   (%dx :initarg :dx
        :initform 0d0
        :accessor dx)
   (%dy :initarg :dy
        :initform 0d0
        :accessor dy)
   (%dz :initarg :dz
        :initform 0d0
        :accessor dz))
  (:documentation
   "The Location Class. Used to hold a current position and direction
vector at that position"))

(defclass frame (temporal)
  (
   ;; Incremental fly vector, accumulated into the basis at each step
   ;; (if flying).
   (%dfv :initarg :dfv
         :initform (pvec)
         :accessor dfv)
   ;; Incremental rotation vector, accumulated into the basis at each
   ;; step (if rotating).
   (%drv :initarg :drv
         :initform (pvec)
         :accessor drv)
   ;; Incremental translation vector of this frames basis in relation to the
   ;; parent basis.
   (%dtv :initarg :dtv
         :initform (pvec)
         :accessor dtv)
   ;; A one time applied displacement vector. Applied once to the
   ;; local-basis then zeroed.
   (%dv :initarg :dv
        :initform (pvec)
        :accessor dv)
   ;; A one time applied rotation vector. Applied once to the
   ;; local-basis then zeroed.
   (%dr :initarg :dr
        :initform (pvec)
        :accessor dr)
   ;; Should I apply the incremental rotation into my basis?
   (%rotatingp :initarg :rotatingp
               :initform NIL
               :accessor rotatingp)
   ;; Should I apply the incremental flight vector into my basis?
   (%flyingp :initarg :flyingp
             :initform NIL
             :accessor flyingp)
   ;; Computed anew each time, persistently the flight and rotation.
   ;; This represents the rigid-body motion (aka the relative motion)
   ;; of the object.
   (%local-basis :initarg :local-basis
                 :initform (matrix-identity)
                 :accessor local-basis)
   ;; The basis which will transform object geometry into world space.
   ;; It is totally reinitialized every frame--prolly could be
   ;; optimized later to only be updated if it needs to be, but that's
   ;; for later if need be.
   (%world-basis :initarg :world-basis
                 :initform (matrix-identity)
                 :accessor world-basis)
   ;; Set to NIL if there is no previous basis, set to T when it is defined.
   (%previous-world-basis-defined-p :initarg :previous-world-basis-defined-p
                                    :initform nil
                                    :accessor previous-world-basis-defined-p)
   ;; previous storage of the model matrix for jutter removal
   (%previous-world-basis :initarg :previous-world-basis
                          :initform (matrix-identity)
                          :accessor previous-world-basis)
   ;; Parent frame of this frame. NIL means this is the root.
   (%parent :initarg :parent
            :initform nil
            :accessor parent)
   ;; A hashtable of those that inherit the spatial frame of this frame.
   (%children :initarg :children
              :initform nil
              :accessor children))
  (:documentation "A hierarchical reference frame system for all objects."))

;; All geometry locations are in a single local coordinate system
(defclass geometry ()
  (
   ;; This represents a set of places at which turrets may
   ;; be placed.
   (%ports :initarg :ports
           :initform nil
           :accessor ports)
   ;; Actual lines, and other geometrical information.
   (%primitives :initarg :primitives
                :initform nil
                :accessor primitives))
  (:documentation "The Shape Class"))

(defclass drawable (entity frame instance)
  (;; A drawable HASA geometry.
   (%geometry :initarg :geometry
              :initform nil
              :accessor geometry)
   ;; If my parent gets destroyed, what happens to me?
   (%orphan-policy :initarg :orphan-policy
                   ;; :destroy means this object is also destroyed.
                   ;; :universe means fling object into the :universe.
                   ;; :nearest-ancestor means the first alive ancestor of
                   ;; my destroyed parent inherits me.
                   :initform :destroy
                   :accessor orphan-policy))
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
  ((%time-to-next-action :initarg :time-to-next-action ;; units in seconds.
                         :initform 0d0
                         :accessor time-to-next-action)
   (%idea-rate-spec :initarg :idea-rate-spec
                    :initform (make-ratespec :initval .5d0)
                    :accessor idea-rate-spec)
   (%idea-rate :initarg :idea-rate
               :initform 0d0
               :accessor idea-rate))
  (:documentation "The Brain Class"))

(defclass powerup (brain)
  ((%powerup-turrets
    :initarg :powerup-turrets
    ;; ((:port-name :turret-instance-name :payload-name/nil) ...)
    ;; If payload name is nil, then inherit the payload
    ;; already in the previous turret.
    :initform nil
    :accessor powerup-turrets)

   (%charging-effects
    :initarg :charge-effects
    ;; Tell the payload found in the turret at :port-name that it is charging
    ;; or decaying and how long it is taking to do it. If we pick up more
    ;; powerups we either divide or multiply the charging-time by something.
    ;; ((:port-name :charging/:decaying) ...)
    :initform nil
    :accessor charging-effects)

   (%health-level :initarg :health-level
                  :initform 0
                  :accessor powerup-health-level))
  (:documentation "The Powerup class"))

;; Turrets can be placed into ports.
(defclass turret (brain)
  ;; A keyword that indicates in which port this turret resides.
  ((%port :initarg :port
          :initform nil
          :accessor port)

   ;; This is what the turret contains
   (%payload :initarg :payload
             :initform nil
             :accessor payload)

   ;; Does this turret contain an INSTANCE-NAME that we must generate? Or
   ;; does it contain an actual instance of something (like a shield?).
   (%payload-instance-p :initarg :payload-instance-p
                        :initform nil
                        :accessor payload-instance-p)))

;; specific weapon turrets
(defclass weapon-turret (turret) ())

;; Special behavior for how these fire.
(defclass one-shot-turret (weapon-turret) ())
(defclass two-shot-turret (weapon-turret) ())
(defclass three-shot-turret (weapon-turret) ())

;; specific shield turrets
(defclass shield-turret (turret) ())

(defclass ship (brain)
  (


   ;; used in the assets file to denote which turret instances are placed
   ;; at which ports in the geometry description when this object is created.
   (%port-layout :initarg :port-layout
                 :initform nil
                 :accessor port-layout)

   ;; A hash table of actual turret instances keyed by port location
   ;; name (since there can only be one turret at that location).
   ;; These are created at spawn, by the defaults in the
   ;; turret-layout, or adjusted by powerups and such. These same turrents
   ;; are also inserted into the children slot of this object.
   (%turrets :initarg :turrets
             :initform (make-hash-table)
             :accessor turrets))

  (:default-initargs :hudp t)
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

(defclass weapon (brain)
  ()
  (:documentation "The Weapon Class"))

(defclass muzzle (weapon)
  ;; Used to hold the instance name of the shot which will come out of
  ;; the muzzle when the turret SHOOTs with it. This MAY also have stuff
  ;; to do with charging of shots and things....
  ;; Children types have different effects for how they render charging and
  ;; other effects.
  ((%shot-instance-name :initarg :shot-instance-name
                        :initform nil
                        :accessor shot-instance-name))
  (:documentation "The Muzzle Class"))

;; These are mostly different for rendering differences.
(defclass simple-muzzle (muzzle)
  ()
  (:documentation "Muzzles which hold simple shots only"))

(defclass hardnose-muzzle (muzzle)
  ()
  (:documentation "Muzzles which hold hardnose shots only"))

(defclass super-muzzle (muzzle)
  ()
  (:documentation "Muzzles which hold super shots only"))

(defclass mine-muzzle (muzzle)
  (;; How many mines can this muzzle drop before being empty.
   (%mine-count :initarg :mine-count
                :initform 5
                :accessor mine-count))
  (:documentation "Muzzles which hold super shots only"))


(defclass shot (weapon)
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

(defclass mine (weapon)
  ()
  (:default-initargs :hudp t)
  (:documentation "The base mine class"))

(defclass proximity-mine (mine)
  ()
  (:documentation "The Proximity Mine Class"))

(defclass field-mine (mine)
  ()
  (:documentation "The Field Mine Class"))

(defclass shield (brain)
  ;; how many shots this shield has currentlyabsorbed
  ((%shots-absorbed :initarg :shots-absorbed
                    :initform 0
                    :accessor shots-absorbed)
   ;; how many total shots it can absorb when fully charged.
   (%max-shot-absorption :initarg :max-shot-absorption
                         :accessor max-shot-absorption))
  (:documentation "The Shield Base Class"))

(defclass shot-shield (shield)
  ()
  (:documentation "The Shot Shield Class"))

(defclass ship-shield (shield)
  ()
  (:documentation "The Ship Shield Class"))







(defclass fieldpath ()
  ;; How many steps the path went before it either hit something or
  ;; reached the end of its range. This is in world space.
  ((%steps :initarg :steps
           :initform 0
           :accessor steps)
   ;; The vector containing the location coordinates of each step with
   ;; element 0 being the start of the path.
   (%path :initarg :path
          :initform nil
          :accessor path))
  (:documentation "The Field Path Class"))

(defclass pathcontact ()
  ((%number-of-contacts :initarg :number-of-contacts
                        :initform 0
                        :accessor number-of-contacts)
   (%path-ids :initarg :contacts
              :initform nil
              :accessor path-ids))
  (:documentation "The Path Contact Class. This is stored on a per entity basis and records the field path-ids that touch that particular entity."))

(defclass field (drawable)
  ;; This range is described in the number of steps I should follow
  ;; the field line trace.
  ((%range :initarg :range
           :initform 1
           :accessor range)
   ;; Num paths are how many even distributed paths should be followed
   ;; from around the field generating object.
   (%num-paths :initarg :num-paths
               :initform 1
               :accessor num-paths)
   ;; An array of fieldpath classes where each one is a trace of the field
   ;; line in world space.
   (%paths :initarg :traces
           :initform nil
           :accessor paths)
   ;; A hash table of pathcontact classes keyed by the entity id the trace
   ;; touches, or "no-id" if it doesn't touch.
   (%entity-contacts :initarg :contacts
                     :initform (make-hash-table :test #'equal)
                     :accessor entity-contacts))
  (:documentation "The Field Class"))

(defclass tesla-field (field)
  ;; This is a quantized range of power for the tesla-field
  ((%power-range :initarg :power-range
                 :initform 1
                 :reader power-range)
   (%power-lines :initarg :power-lines
                 :initform 1
                 :reader power-lines))
  (:documentation "The Tesla Field Class"))

(defclass scene-manager ()
  (;; Each drawable gets shoved into the scene manager
   (%root :initarg :scene
          ;; The universal coordinate frame.
          :initform (make-instance 'drawable :id :universe)
          :accessor root)

   (%views :initarg :views
           ;; These are dynamically created. Keyed by the keyword of
           ;; the role name, and the value is a list of entities in
           ;; that role.
           :initform (make-hash-table :test #'eq)
           :accessor views))
  (:documentation "This is the scene manager class. It keeps the scene
 tree of the relation of the frames to each other in a tree. It
 additionally keeps objects separated by their role in the game so we
 can speed up collision detection among other algorithms that we would
 only like to do on slices of objects in the game."))


;; Each thing in the world is kept is its particular list. This makes it
;; easy to perform collision detection only as necessary.
(defclass game ()
  (;; Important dimensions about the display window and game world size
   (%window-width :initarg :window-width
                  :initform +game-width+
                  :accessor window-width)
   (%window-height :initarg :window-height
                   :initform +game-height+
                   :accessor window-height)
   (%game-width :initarg :game-width
                :initform +game-width+
                :accessor game-width)
   (%game-height :initarg :game-height
                 :initform +game-height+
                 :accessor game-height)

   ;; Make the universe object which is the root of the scene tree.
   (%scene-man :initarg :scene-man
               :initform nil
               :accessor scene-man)

   ;; The spawnables that will likely be created at the start of the
   ;; next frame.
   (%spawnables :initarg :spawnables
                :initform nil
                :accessor spawnables)


   (%score-board :initarg :score-board
                 :initform nil
                 :accessor score-board)
   (%highscore-board :initarg :highscore-board
                     :initform nil
                     :accessor highscore-board)
   ;; Set to T when the score changes, in order to redraw the score boards.
   ;; when NIL, don't redraw the score boards. This is because it happens
   ;; to be expensive in the manner I implemented it.
   (%modified-score-p :initarg :modified-score-p
                      :initform t
                      :accessor modified-score-p)


   ;; These are thigns related to the game itself.
   (%score :initarg :score
           :initform 0
           :accessor score)
   (%highscore :initarg :highscore
               :initform 0
               :accessor highscore)
   (%enemy-spawn-timer :initarg :enemy-spawn-timer
                       :initform 60
                       :accessor enemy-spawn-timer)
   (%paused :initarg :paused
            :initform nil
            :accessor paused))
  (:documentation "The Game Class"))
