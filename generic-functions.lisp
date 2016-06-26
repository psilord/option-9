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

(defgeneric sparks (entity)
  (:documentation
   "How many sparks the entity produces when it dies"))

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

(defgeneric die (entity)
  (:documentation
   "Performs the necessary actions when an entity dies"))

(defgeneric distance (left-frame right-frame &key &allow-other-keys)
  (:documentation
   "Computes the distance between the origins of two frames. If the
keyword :sqrt is T (the default) then return two values of the euclidean
distance and the non-normalized distance. If :SQRT is NIL, then return two
values where the first and second are both the non-normalized distance."))


(defgeneric dfvx (frame)
  (:documentation "The x component of the fly vector"))
(defgeneric dfvy (frame)
  (:documentation "The y component of the fly vector"))
(defgeneric dfvz (frame)
  (:documentation "The z component of the fly vector"))
(defgeneric (setf dfvx) (new-val frame)
  (:documentation "Set the x component of the fly vector"))
(defgeneric (setf dfvy) (new-val frame)
  (:documentation "Set the y component of the fly vector"))
(defgeneric (setf dfvz) (new-val frame)
  (:documentation "Set the z component of the fly vector"))

(defgeneric drvx (frame)
  (:documentation "The x component of the rotation vector"))
(defgeneric drvy (frame)
  (:documentation "The y component of the rotation vector"))
(defgeneric drvz (frame)
  (:documentation "The z component of the rotation vector"))
(defgeneric (setf drvx) (new-val frame)
  (:documentation "Set the x component in radians of the rotation vector"))
(defgeneric (setf drvy) (new-val frame)
  (:documentation "Set the y component in radians of the rotation vector"))
(defgeneric (setf drvz) (new-val frame)
  (:documentation "Set the z component in radians of the rotation vector"))

(defgeneric dtvx (frame)
  (:documentation "The x component of the incremental translation vector"))
(defgeneric dtvy (frame)
  (:documentation "The y component of the incremental translation vector"))
(defgeneric dtvz (frame)
  (:documentation "The z component of the incremental translation vector"))
(defgeneric (setf dtvx) (new-val frame)
  (:documentation "Set the x component of the incremental translation vector"))
(defgeneric (setf dtvy) (new-val frame)
  (:documentation "Set the y component of the incremental translation vector"))
(defgeneric (setf dtvz) (new-val frame)
  (:documentation "Set the z component of the incremental translation vector"))

(defgeneric dvx (frame)
  (:documentation "The x component of the one time displacement
translation vector"))
(defgeneric dvy (frame)
  (:documentation "The y component of the one time displacement
translation vector"))
(defgeneric dvz (frame)
  (:documentation "The z component of the one time displacement
translation vector"))
(defgeneric (setf dvx) (new-val frame)
  (:documentation "Set the x component of the one time displacement
translation vector"))
(defgeneric (setf dvy) (new-val frame)
  (:documentation "Set the y component of the one time displacement
translation vector"))
(defgeneric (setf dvz) (new-val frame)
  (:documentation "Set the z component of the one time displacement
translation vector"))

;; TODO, this isn't cognizant of passing time. Probably wrong.
(defgeneric dtv-decay (frame &key &allow-other-keys)
  (:documentation
   "Each time the frame's local axis is computed, set the dtv vector
in the frame to whatever this function returns."))

(defgeneric at-location (destination source)
  (:documentation "Copy the local-basis and the world-basis from the SOURCE
into the DESTINATION."))


(defgeneric walk-frame-hierarchy (root func)
  (:documentation
   "Apply FUNC to each element of the scene tree rooted at ROOT, starting at
the ROOT and working towards the leaves of the tree."))

(defgeneric resolve-world-basis (frame)
  (:documentation "Multiply the parent's world-basis against the
FRAME's local-basis and assign it to be the world-basis of the
frame. When coupled with WALK-FRAME-HIERARCHY, this performs the
associative matrix multiply of (((RA)B)C) where R is the root, A the
child, B the grandchild, C the great grandchild. Then the resultant
matrix is made the world-basis for C."))

(defgeneric update-local-basis (frame)
  (:documentation "Apply any flying, rotating, or one time
displacement translation mechanics to the local-basis."))

(defgeneric add-child (frame child)
  (:documentation "Add the child to the frame. Return the added child."))
(defgeneric remove-child (frame child)
  (:documentation "Recurse down the hierarchies, and when the
specified child is found, remove it from the scene tree. Return the
child if the child was found and removed, and NIL if never found."))

(defgeneric entities-with-role (scene-man role)
  (:documentation "Return a list of entities from the SCENE-MAN that
have the specific ROLE specified. The ROLE is a single keyword."))

(defgeneric all-entities-in-roles (scene-man &rest roles)
  (:documentation "Return a list of all entities from the SCENE-MAN which
are in the roles of one or more of the ROLES list."))



(defgeneric active-step-once (frame)
  (:documentation
   "Performs one step in the simulation of this entity. By default it
will move the x y location of the entity by dx dy in the frame and
will decrease the ttl towards zero if present. It is intended that
before or after methods are used in more specific objects to take
advantage of the simulation step."))

(defgeneric passive-step-once (entity)
  (:documentation
   "Performed after all active-steps, this performs on passive step in the
simulation of the entity. A passive step is something which requires final
knowledge of all entity locations. An example is field generation."))

(defgeneric render (drawable jutter-interpolant)
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

(defgeneric damage (thing other-thing)
  (:documentation
   "Two things _may_ damage each other if they interact"))

(defgeneric explode (thing)
  (:documentation
   "When something dies and needs to explode, this is how it explodes."))

(defgeneric shoot (ship port)
  (:documentation
   "The ship, be it player or enemy or something else, shoots whatever turret
exists at the identified port."))

(defgeneric turret (ship turret-name)
  (:documentation
   "Find the instance of the turret named TURRET-NAME in the SHIP."))

(defgeneric (setf turret) (turret-instance ship turret-name)
  (:documentation
   "Set the TURRET-NAME in the SHIP to be the TURRET-INSTANCE."))

(defgeneric absorbs (collider shield)
  (:documentation
   "Should return true if the shield absorbs a specific collider"))

(defgeneric think (brain)
  (:documentation
   "For entites which need to think, count down until the next idea shows up
and when it does, invoke it"))

(defgeneric idea (brain)
  (:documentation
   "If anything needs to be done about a future or current action to take, this
is where it is done."))

(defgeneric contacts (contacter contactee)
  (:documentation
   "Different than collide, this return a data structure explaining the manner
of the contact between the concacter and the contactee."))

(defgeneric generate (field source participants)
  (:documentation
   "Certain types of effects like fields need a generation phase which
computes their solution in terms of a source of the effect and other entities
(which may also include the source) on the board."))

(defgeneric increase-power (item)
  (:documentation
   "Increases one of multiple aspects of the object that represents its
power. It does so randomly but intelligently such that if one aspect is
already at max, another is chosen."))

(defgeneric increase-density (tesla-field)
  (:documentation
   "Increases the number of lines traced from the source of the field or
as appropriate for whatever class this gets applied to."))

(defgeneric increase-range (item)
  (:documentation
   "Increases the range of the item."))

(defgeneric (setf power-range) (range field)
  (:documentation "Bookkeep internals of the FIELD object when the
power range of the field increases"))

(defgeneric power-density-maxp (item)
  (:documentation
   "Returns true if the item is at the maximum density (of whatever quantity)"))
(defgeneric power-range-maxp (item)
  (:documentation
   "Returns true if the item is at the maximum range (of whatever quantity)"))

(defgeneric (setf power-lines) (lines field)
  (:documentation
   "Bookkeep internals of the FIELD object when the number of paths the field
shoots is changed."))

(defgeneric trace-field-line (field path-num tx ty q1 charges)
  (:documentation
   "Trace the field-line denoted by path-num starting at tx ty with q1 as the
source charge and charges (which may contain q1) as the participants in the
field. The positions of the traced field line is encoded into the field."))

(defgeneric trace-field-lines (field q1 charges)
  (:documentation
   "Trace all field lines associated with the field starting from q1 with the
participating charges. All information is stored into the field object."))
