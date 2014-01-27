(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defclass spawnable ()
  ((%ioi/e :initarg :ioi/e ;; instance-or-insts/equiv
           :initform nil
           :accessor spawnable-ioi/e)
   (%spawn-context :initarg :spawn-context
                   :initform nil
                   :accessor spawnable-spawn-context)
   (%initializer :initarg :initializer
                 :initform nil
                 :accessor spawnable-initializer)
   (%parent :initarg :parent
            :initform :universe
            :accessor spawnable-parent)
   (%mutator :initarg :mutator
             :initform #'identity
             :accessor spawnable-mutator)
   (%game :initarg :game
          :initform nil
          :accessor spawnable-game)))

;; These are known as 'spawn classes'. An entity of a certain class,
;; suppose HARDNOSE-SHOT, may be spawned for the player or the
;; enemy. To know that for the collision sets, we denote the
;; spawn-class of the spawn itself. These classes represent the spawn class
;; of the newly spawned entity. The entity may change who it collides with
;; at a later time.
(defclass sp-player (spawnable) ())
(defclass sp-player-shot (spawnable) ())
(defclass sp-player-powerup (spawnable) ())
(defclass sp-enemy (spawnable) ())
(defclass sp-enemy-shot (spawnable) ())
(defclass sp-enemy-mine (spawnable) ())
(defclass sp-sparks (spawnable) ())
(defclass sp-shrapnel (spawnable) ())
;; How things like HUD and scoreboards are done might need to be rethought.
;; In this feature, each digit is an entity shoved into the game world.
;; Maybe that isn't what I want for strings of text and such. But for now
;; I'll leave it here.
(defclass sp-alphanum (spawnable) ())

;; Hrm, I might need to reify that concept even more and have a spawn
;; queue into which things get pushed, and then after the frame is
;; completed, they get inserted into the scene tree. Very tricky,
;; since maybe a ship spawned a rotating option around it and states
;; it is its parent in the scene tree, then gets destroyed later in
;; that frame, then the option doesn't have a parent when it is
;; inserted.... Very tricky...

;; Also, how come I don't have a spawn on the entity hiearchy? Because so far
;; some things like SHOTs are not delineated between player and enemy, or
;; anything else. So to categorize them correctly, I need to know for what
;; spawn-class they are.

(defun make-spawnable (spawn-type &rest initargs)
  "A factory constructor to create spawnables of CLASS-TYPE for the supplied
INITARGS."
  (apply #'make-instance spawn-type initargs))


;; a CATEGORY is really a developer defined thing just to separate different
;; spawn algorithms for various instances.
(defgeneric spawn (spawn-class ioi/e loc/ent game
                               &key spawn-context parent mutator
                               &allow-other-keys)
  (:documentation "TODO"))

(defgeneric realize-spawn (spawnable)
  (:documentation "If the parent of the spawn promise is still alive
then force the promise into a real entity and insert it into the scene tree
in accordance with the spawn's wishes. Return two values of T/NIL if the
spawn succeeded and a REASON about the success or failure."))

(defgeneric reclaim-failed-spawn (spawnable reason-failed)
  (:documentation "When a spawn failed, do something meaningful with
it, if anything."))

(defgeneric resolve-spawn-location (loc/ent)
  (:documentation "Retrive from either the PVEC or the ENTITY, what the
location of spawning is."))

(defun realize-spawns (game)
  "Iterate the spawnables list and if possible, realize all of the
spawnables into real in game entities. Those that can't be immediately
realized due to loss of parents are funneled to RECLAIM-FAILED-SPAWN."
  (dolist (spawnable (spawnables game))
    (multiple-value-bind (spawnedp reason-failed)
        (realize-spawn spawnable)
      (unless spawnedp
        (reclaim-failed-spawn spawnable reason-failed))))
  (clear-spawnables game))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic means to get the real-world location where something should spawn.
;; TODO: This may need work when dealing with relative to a parent locations.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assume this will be a PVEC, break in an undefined manner otherwise.
(defmethod resolve-spawn-location ((loc/ent simple-array))
  loc/ent)

(defmethod resolve-spawn-location ((loc/ent entity))
  (pm-get-trans (world-basis loc/ent)))

;; While this could be defined for each spawn-class, we just do the
;; same behavior for everything unless we find a reason to do
;; something different.  TODO: At this time, we don't expect anything
;; to fail to be spawned so if it happened it is likely a programmer
;; error.
(defmethod reclaim-failed-spawn ((spawnable spawnable) reason-failed)
  (error "Failed to spawn ~A because of ~A. Sorry!~%"
         spawnable reason-failed)
  nil)


;; In general, most things can be realized without too much trouble.
(defmethod realize-spawn ((spawnable spawnable))
  (let ((entity (funcall (spawnable-mutator spawnable)
                         (apply #'make-entity
                                (spawnable-initializer spawnable)))))

    (insert-into-scene (scene-man (spawnable-game spawnable))
                       entity
                       (spawnable-parent spawnable))
    (values T :spawned)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning Player 1
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-player)) ioi/e loc/ent game
                  &key
                  (spawn-context 1) ;; default to player 1
                  (parent :universe)
                  (mutator #'identity)
                  &allow-other-keys)
  (declare (ignorable loc/ent))

  ;; This initialization list is ultimately given to MAKE-ENTITY
  (let ((initializer `(,(insts/equiv-choice ioi/e)
                        :roles (:player)
                        :flyingp t
                        :dv ,(pvec .5d0 .05d0 0d0))))

    ;; This will be potentially realized later if the conditions are
    ;; still good for realization.
    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning a Player Shot
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-player-shot)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    ;; the shot starts its journey in nearly the same location as the
    ;; player ship.
    (with-pvec-accessors (o loc)
      (let ((initializer `(,ioi/e
                           :roles (:player-shot)
                           :flyingp t
                           ;; This goes into the world location of
                           ;; the player.
                           :dv ,(pvec ox (+ oy .03d0) 0d0)
                           ;; And it flies going up.
                           ;; XXX It should fly in the direction of the
                           ;; main gun (which is up y axis) on the ship.
                           ;; And shots have a "front" which should be
                           ;; rotated to orient in parallel with the
                           ;; "front" direction vector of the ship firing
                           ;; it.
                           :dfv ,(pvec 0d0 .022d0 0d0))))

        (add-spawnable
         (make-spawnable spawn-class
                         :ioi/e ioi/e
                         :spawn-context spawn-context
                         :initializer initializer
                         :parent parent
                         :mutator mutator
                         :game game)
         game)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning an Enemy
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-enemy)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  &allow-other-keys)
  (declare (ignorable loc/ent))

  ;; This initialization list is ultimately given to MAKE-ENTITY
  (let* ((xloc (random 1.0d0))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :roles (:enemy)
                         :flyingp t
                         :dv ,(pvec xloc .95d0 0d0)
                         :dfv ,(pvec (coerce
                                      (* (random .001d0)
                                         (if (> (the double-float xloc) .5d0)
                                             -1d0
                                             1d0)) 'double-float)
                                     (- (random .01d0))
                                     0d0))))

    ;; This will be potentially realized later if the conditions are
    ;; still good for realization.
    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning an Enemy Shot
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-enemy-shot)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    ;; the shot starts its journey in nearly the same location as the
    ;; enemy ship.
    (with-pvec-accessors (o loc)
      (let ((initializer `(,ioi/e
                           :roles (:enemy-shot)
                           :flyingp t
                           ;; This goes into the world location of
                           ;; the enemy.
                           :dv ,(pvec ox (- oy .03d0) 0d0)
                           ;; And it flies going down.  XXX It should
                           ;; fly in the direction of the main gun on
                           ;; the ship.  And shots have a "front"
                           ;; which should be rotated to orient in
                           ;; parallel with the "front" direction
                           ;; vector of the ship firing it.
                           :dfv ,(pvec 0d0
                                       (+ (- (+ .005d0 (random .005d0)))
                                          (dfvy loc/ent))
                                       0d0))))

        (add-spawnable
         (make-spawnable spawn-class
                         :ioi/e ioi/e
                         :spawn-context spawn-context
                         :initializer initializer
                         :parent parent
                         :mutator mutator
                         :game game)
         game)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning a pile of sparks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-sparks)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  (num-sparks 10)
                  (ttl-max 0 ttl-max-supplied-p)
                  (velocity-factor 1d0)
                  &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    (dotimes (p num-sparks)
      (let ((initializer `(,(insts/equiv-choice ioi/e)
                            :roles (:scenery)
                            :flyingp t
                            :dv ,(pv-copy loc) ;; each spark needs own copy!
                            ;; :rotatingp t
                            ;; :drv ,(pvec 0d0 0d0 (/ pi (random 256d0)))
                            :dfv ,(pvec (* (random-delta) velocity-factor)
                                        (* (random-delta) velocity-factor)
                                        0d0))))

        (flet ((spark-mutator (spark)
                 ;; Complete the spark instantiation by updating the ttl.
                 (when ttl-max-supplied-p
                   (setf (ttl-max spark) ttl-max))
                 (make-instance-finish spark)))

          (add-spawnable
           (make-spawnable spawn-class
                           :ioi/e ioi/e
                           :spawn-context spawn-context
                           :initializer initializer
                           :parent parent
                           :mutator #'(lambda (s) ;; compose with supplied one.
                                        (funcall mutator
                                                 (spark-mutator s)))
                           :game game)
           game))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning a Powerup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-player-powerup)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :roles (:player-powerup)
                         :flyingp t
                         :dv ,(pv-copy loc))))
    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning an Enemy Mine
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-enemy-mine)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :roles (:enemy-mine)
                         :flyingp t
                         :dv ,(pv-copy loc))))
    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning a digit for a score board
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-alphanum)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  roles
                  &allow-other-keys)

  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :roles ,roles
                         :dv ,(pv-copy loc))))
    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning some shrapnel that can hurt anyone and can damage shields
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-shrapnel)) ioi/e loc/ent game
                  &key
                  spawn-context
                  (parent :universe)
                  (mutator #'identity)
                  (velocity-factor .5d0)
                  &allow-other-keys)


  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :roles (:shrapnel)
                         :dv ,(pv-copy loc)
                         :rotatingp t
                         :drv ,(pvec 0d0 0d0 (/ pi (+ 64d0 (random 64d0))))
                         :dtv ,(pvec (* (random-delta) velocity-factor)
                                     (* (random-delta) velocity-factor)
                                     0d0))))

    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))

