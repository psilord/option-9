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

(defgeneric make-instance-finish (object)
  (:documentation
   "Sometimes a class constructor needs a little extra class specific
work to finish its construction."))

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

(defgeneric distance (left-frame right-frame)
  (:documentation
   "Computes the distance between the origins of two frames"))

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

(defgeneric damage (thing other-thing)
  (:documentation
   "Two things _may_ damage each other if they interact"))

(defgeneric shoot (ship)
  (:documentation
   "The ship, be it player or enemy or something else, shoots its main gun."))

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

