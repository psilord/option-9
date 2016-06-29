(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defclass spawnable ()
  ((%ioi/e :initarg :ioi/e ;; keyword which is the instance or insts/equiv
           :initform nil
           :accessor spawnable-ioi/e)
   (%spawn-context :initarg :spawn-context
                   :initform nil
                   :accessor spawnable-spawn-context)
   (%initializer :initarg :initializer
                 :initform nil
                 :accessor spawnable-initializer)
   ;; The instance that is my parent in the scene-tree.
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
(defclass sp-ship (spawnable) ())
(defclass sp-player (sp-ship) ())
(defclass sp-player-shot (spawnable) ())
(defclass sp-player-mine (spawnable) ())
(defclass sp-player-powerup (spawnable) ())
(defclass sp-enemy (sp-ship) ())
(defclass sp-enemy-shot (spawnable) ())
(defclass sp-enemy-mine (spawnable) ())
(defclass sp-sparks (spawnable) ())
(defclass sp-shrapnel (spawnable) ())
;; How things like HUD and scoreboards are done might need to be rethought.
;; In this feature, each digit is an entity shoved into the game world.
;; Maybe that isn't what I want for strings of text and such. But for now
;; I'll leave it here.
(defclass sp-alphanum (spawnable) ())

;; Sometimes the initialization parameters of an object are fully realized and
;; I just want to spawn it with no changes.
(defclass sp-realize (spawnable) ())

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
                   &key spawn-context parent orphan-policy mutator extra-init
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
realized due to loss of parents are funneled to RECLAIM-FAILED-SPAWN for now."
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
(defmethod resolve-spawn-location (loc/ent)
  (error "resolve-spawn-location: Can't resolve the location of something I don't understand!"))

;; Assume this will be a PVEC, break in an undefined manner otherwise.
(defmethod resolve-spawn-location ((loc/ent simple-array))
  loc/ent)

(defmethod resolve-spawn-location ((loc/ent entity))
  (matrix-translate-get (world-basis loc/ent)))

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


;; used for sp-ships since we have to create additional entities inside them
(defmethod realize-spawn ((spawnable sp-ship))
  (let ((entity (funcall (spawnable-mutator spawnable)
                         (apply #'make-entity
                                (spawnable-initializer spawnable)))))

    ;; first add the ship into the scene graph.
    (insert-into-scene (scene-man (spawnable-game spawnable))
                       entity
                       (spawnable-parent spawnable))

    ;; Then make each turret specified in the turret-layout and add them as
    ;; children to the ship.
    (dolist (layout (port-layout entity))
      (destructuring-bind (port turret-instance payload) layout
        ;; create a real instance of the turrent
        (let* ((port-frame
                ;; Get the orientation matrix of the turret in relation to the
                ;; origin of the entity.
                ;; XXX This is a ridiculous query. Fix it.
                (cadadr (assoc port (ports (geometry entity)))))

               (a-turret
                (make-entity
                 ;; Get an appropriate instance for this entity and turret.
                 (specialize-generic-instance-name
                  (instance-name entity) turret-instance)
                 ;; TODO: For now, we set the orphan
                 ;; policy to just be destroyed. However,
                 ;; maybe this should be set in option-9.dat
                 ;; or somehow dynamically.
                 :orphan-policy :destroy

                 ;; Get an appropriate payload for this entity and payload name.
                 :payload
                 (let ((payload (specialize-generic-instance-name
                                 (instance-name entity) payload)))
                   (when payload
                     (make-entity payload)))

                 :port port

                 :local-basis (matrix-copy port-frame))))

          ;; Add each turret to the scene tree with the entity as the parent.
          ;; I've forgotten a bit how this works, ah well, I'll remember soon
          ;; enough.
          (insert-into-scene (scene-man (spawnable-game spawnable))
                             a-turret
                             entity)
          (when (payload a-turret)
            (insert-into-scene (scene-man (spawnable-game spawnable))
                               (payload a-turret)
                               a-turret))

          ;; associate the turret instance with the port location in
          ;; the turrets hash table. This allows us later to route
          ;; events to turrets. When the turret is destroyed, we'll have
          ;; to remove it from here too.
          (setf (turret entity port) a-turret)
          )))


    (values T :spawned)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just spawn a fully realized thing without serious modification
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-realize)) ioi/e location game
                  &key spawn-context
                    (parent :universe)
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)

  (let ((initializer `(,(insts/equiv-choice ioi/e)
                        :orphan-policy ,orphan-policy
                        ,@extra-init)))

    ;; This will be potentially realized later if the conditions are
    ;; still good for realization.
    (add-spawnable
     (make-spawnable 'sp-realize
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spawning Player 1
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-player)) ioi/e loc/ent game
                  &key
                    (spawn-context 1) ;; default to player 1
                    (parent :universe)
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (declare (ignorable loc/ent))

  ;; This initialization list is ultimately given to MAKE-ENTITY
  (let ((initializer `(,(insts/equiv-choice ioi/e)
                        :orphan-policy ,orphan-policy
                        :roles (:player)
                        :flyingp t
                        :dv ,(pvec (per-game-width game 50.0)
                                   (per-game-height game 5.0)
                                   0d0)
                        ,@extra-init)))

    ;; This will be potentially realized later if the conditions are
    ;; still good for realization.
    (add-spawnable
     (make-spawnable 'sp-ship
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
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    (with-pvec-accessors (o loc)
      (let ((initializer `(,ioi/e
                           :orphan-policy ,orphan-policy
                           :roles (:player-shot)
                           ;; Put it at the location and rotation of the
                           ;; turret.
                           :local-basis ,(matrix-copy (world-basis loc/ent))
                           ,@extra-init)))

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
;; Spawning a Player Mine
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-player-mine)) ioi/e loc/ent game
                  &key
                    spawn-context
                    (parent :universe)
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
         (the-mine-instance-name
          (specialize-generic-instance-name (instance-name loc/ent) ioi/e))

         (initializer `(,the-mine-instance-name
                        :orphan-policy ,orphan-policy
                        :roles (:player-mine)
                        :dv ,(vcopy loc)
                        ,@extra-init)))
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
;; Spawning an Enemy
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-enemy)) ioi/e loc/ent game
                  &key
                    spawn-context
                    (parent :universe)
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (declare (ignorable loc/ent))

  ;; This initialization list is ultimately given to MAKE-ENTITY
  (let* ((xloc (coerce (random (game-width game)) 'double-float))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :orphan-policy ,orphan-policy
                         :roles (:enemy)

                         ;; We let the THINK system determine if it
                         ;; should be flying and what the dfv will be.
                         :flyingp nil

                         ;; rotate to aim downwards
                         :dr ,(pvec 0d0 0d0 pi)

                         ;; place the enemy in relation to screen coordinates.
                         :dv ,(pvec xloc (per-game-height game 95.0) 0d0)

                         ,@extra-init)))

    ;; This will be potentially realized later if the conditions are
    ;; still good for realization.
    (add-spawnable
     (make-spawnable 'sp-ship
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
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    ;; Start the shot at the location of the loc/ent that created
    ;; it. On our case a turret.
    (with-pvec-accessors (o loc)
      (let ((initializer `(,ioi/e
                           :orphan-policy ,orphan-policy
                           :roles (:enemy-shot)
                           :local-basis ,(matrix-copy (world-basis loc/ent))
                           :dfv ,(pvec 0d0
                                       (+ .5d0 (random .5d0)
                                          (dfvy
                                           ;; FIXME: This is a bit of
                                           ;; a hack...  If loc/ent is
                                           ;; a turret, get our parent
                                           ;; (the ship's) velocity to
                                           ;; add to the shot. There
                                           ;; must be a better way to
                                           ;; do this.
                                           (if (subtypep
                                                (type-of loc/ent) 'turret)
                                               (parent loc/ent)
                                               loc/ent)))
                                       0d0)
                           ,@extra-init)))

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
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    (num-sparks 10)
                    (ttl-max 0 ttl-max-supplied-p)
                    (initial-velocity 0d0)
                    (velocity-factor 2d0)
                    extra-init
                    &allow-other-keys)
  (let ((loc (resolve-spawn-location loc/ent)))
    (dotimes (p num-sparks)
      (let ((initializer `(,(insts/equiv-choice ioi/e)
                            :orphan-policy ,orphan-policy
                            :roles (:scenery)
                            ;; each spark needs own copy!
                            :dv ,(vcopy loc)
                            ;; And it moves in a random direction.
                            :dtv ,(let ((dirvec (vrand :span :xy)))
                                       (vscalei dirvec
                                                dirvec
                                                (+ initial-velocity
                                                   (random velocity-factor))))
                            ,@(when ttl-max-supplied-p (list :ttl-max ttl-max))
                            ,@extra-init)))

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
;; Spawning a Powerup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod spawn ((spawn-class (eql 'sp-player-powerup)) ioi/e loc/ent game
                  &key
                    spawn-context
                    (parent :universe)
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :orphan-policy ,orphan-policy
                         :roles (:player-powerup)
                         :flyingp t
                         :dv ,(vcopy loc)
                         ,@extra-init)))
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
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    extra-init
                    &allow-other-keys)
  (let* ((loc (resolve-spawn-location loc/ent))

         ;; Find a random generic mine, then create the specialized instance
         ;; of it for this particular ioi/e if it is an entity.
         (the-mine-instance-name
          (if (subtypep (type-of loc/ent) 'instance)
              ;; If we happen to be spawning this because of a death
              ;; of an entity, we'll use that entity's specializations
              ;; to pick a random mine.
              (specialize-generic-instance-name
               (instance-name loc/ent)
               (insts/equiv-choice ioi/e))
              ;; Otherwise, it better be a SPECIFIC mine instance we
              ;; want at the location specified.
              ioi/e))

         (initializer `(,the-mine-instance-name
                        :orphan-policy ,orphan-policy
                        :roles (:enemy-mine)
                        :dv ,(vcopy loc)
                        ,@extra-init)))
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
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    roles
                    extra-init
                    &allow-other-keys)

  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :orphan-policy ,orphan-policy
                         :roles ,roles
                         :dv ,(vcopy loc)
                         ,@extra-init)))
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
                    (orphan-policy :destroy)
                    (mutator #'identity)
                    (velocity-factor 50d0)
                    extra-init
                    &allow-other-keys)


  (let* ((loc (resolve-spawn-location loc/ent))
         (initializer `(,(insts/equiv-choice ioi/e)
                         :orphan-policy ,orphan-policy
                         :roles (:shrapnel)
                         :dv ,(vcopy loc)
                         :rotatingp t
                         :drv ,(pvec 0d0 0d0 (/ pi (+ 64d0 (random 64d0))))
                         :dtv ,(pvec (* (random-delta) velocity-factor)
                                     (* (random-delta) velocity-factor)
                                     0d0)
                         ,@extra-init)))

    (add-spawnable
     (make-spawnable spawn-class
                     :ioi/e ioi/e
                     :spawn-context spawn-context
                     :initializer initializer
                     :parent parent
                     :mutator mutator
                     :game game)
     game)))
