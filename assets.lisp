(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; If any random entity sets a ttl-max and nothing more specific changes this
;; method, then assign a random ttl based upon the ttl-max.
(defmethod initialize-instance :after ((s temporal) &key)
  (when (not (null (ttl-max s)))
    (setf (ttl s) (+ (ttl-min s) (random (- (ttl-max s) (ttl-min s))))))
  s)

(defun compute-ratespec (the-spec)
  (clamp (+ (ratespec-initval the-spec)
            (coerce (random-in-range
                     (ratespec-rand-minoff the-spec)
                     (ratespec-rand-maxoff the-spec))
                    'double-float))
         (ratespec-minval the-spec)
         (ratespec-maxval the-spec)))

(defmethod initialize-instance :after ((e entity) &key)
  (setf (hit-points e) (max-hit-points e))

  ;; Initialize the various speeds we need from the ratespecs.
  (setf (forward-speed e) (compute-ratespec (forward-speed-spec e)))
  (setf (backward-speed e) (compute-ratespec (backward-speed-spec e)))
  (setf (strafe-left-speed e) (compute-ratespec (strafe-left-speed-spec e)))
  (setf (strafe-right-speed e) (compute-ratespec (strafe-right-speed-spec e)))
  (setf (up-speed e) (compute-ratespec (up-speed-spec e)))
  (setf (down-speed e) (compute-ratespec (down-speed-spec e)))
  e)

(defmethod initialize-instance :after ((e brain) &key)
  ;; initialize the rate at which this brain can have ideas.
  (setf (idea-rate e) (compute-ratespec (idea-rate-spec e)))
  e)

(defmethod initialize-instance :after ((ent tesla-field) &key)
  (setf (power-range ent) (power-range ent))
  (setf (power-lines ent) (power-lines ent))
  ent)

;; A factory constructor to make me a CLOS instance of any :instance
;; read into *assets*. Each entity also knows the game context
;; in which it will be placed. This allows the generic methods of
;; entities to inspect the game universe or insert effects into the
;; game like sparks.
(defun make-entity (instance &rest override-initargs)
  (multiple-value-bind (info present) (gethash instance (entities *assets*))
    (assert present)
    (assert instance)
    (let ((found-instance (cadr (assoc :instance info)))
          (cls (cadr (assoc :class info)))
          ;; Add in the instance name associated with this object too!
          (initargs (cons :instance-name
                          (cons instance
                                (cdr (assoc :initargs info))))))

      (assert (eq found-instance instance))
      (assert cls)

      ;; The COPY-SEQ is important because there is a potential to
      ;; modify the full-init args to replace certain named things
      ;; with their actual values. Also, it prevents literal objects from
      ;; being referenced by all instances (in addition to the undefined
      ;; writing to them, if they were not copied)
      (let* ((full-args (copy-seq (append override-initargs initargs)))
             (roles (cadr (member :role full-args))))

        ;; Ensure any specified roles are actually valid!
        (assert (apply #'defined-roles-p *assets* roles))

        ;; Concerning the :geometry initarg value, replace named
        ;; geometries with real geometries from the cache. Otherwise,
        ;; make-instance the form found assuming it is an in-place
        ;; geometry specification.
        (let* ((?geometry (member :geometry full-args))
               (?geometry-name (cadr ?geometry)))
          ;; Only mess with the ?geometry initializer if we have one at all.
          (when ?geometry
            (if (and ?geometry-name (symbolp ?geometry-name))
                ;; Ok, we found a geometry-name, replace it with an actual
                ;; geometry object constructed form the initializers
                (multiple-value-bind (geometry presentp)
                    (gethash ?geometry-name (geometries *assets*))
                  (when (not presentp)
                    (error "Cannot find geometry-name ~A in the assets!"
                           ?geometry-name))
                  ;; Replace the copy-seq'ed full-args entry for the
                  ;; :geometry initarg to be the actual geometry instead
                  ;; of its name.
                  (setf (cadr ?geometry)
                        (apply #'make-instance 'geometry geometry)))
                ;; Ok, we have an inplace form, so just convert it.
                (setf (cadr ?geometry)
                      (apply #'make-instance 'geometry (cadr ?geometry))))))

        ;; The values of the override arguments are accepted
        ;; first when in a left to right ordering in the
        ;; argument list in concordance with the ANSI spec.
        (apply #'make-instance cls :game-context *game* full-args)))))

(defun weighted-choice (choices)
  "Given a list of probabilistic CHOICES like:

 ((.25 thing-1) (.25 thing-2) (.50 thing-3))

choose a random number from 0 to 1.0 and if

  0.0 below .25, return thing-1
  .25 below .50, return thing-2
  .50 upto 1.0, return thing-3.

If the ranges add up to less than than 1.0, then the remainder of the
probability range is the NIL result. If they add up to more than 1.0, it is an
ERROR.

If (ATOM CHOICES) => T, then just return CHOICES."

  (when (atom choices)
    (return-from weighted-choice choices))

  ;; Assert that the probabilities sum to 1.0 or less.
  ;; TODO: slow to do each time, but good for consistency.
  (let ((prob-sum (reduce '+ choices :key 'car)))
    (when (> prob-sum 1.0)
      (error "Probability of list ~A is ~A and violates the constraint of being less than or equal to 1.0" choices prob-sum)))

  (let ((stone (random 1.0)))
    (loop for (prob choice) in choices
       summing prob into prob-accum do
         (when (< stone prob-accum)
           (return-from weighted-choice choice))))

  ;; Otherwise, nothing was chosen (the sum of the probability must have
  ;; been less than 1.0, so we ascribe the rest of the probability to NIL).

  nil)



(defun insts/equiv-choice (ioi/e)
  "Given an instance equivalence class, select a random :instance from it.
Given an :instance name, just return it."
  (multiple-value-bind (instances presentp)
      (gethash ioi/e (insts/equiv *assets*))
    (if presentp
        (svref instances (random (length instances)))
        ioi/e)))


(defun defined-roles-p (assets &rest roles)
  (dolist (role roles)
    (multiple-value-bind (value presentp)
        (gethash role (defined-roles assets))
      (declare (ignore value))
      (when (not presentp)
        (return-from defined-roles-p nil))))
  t)

(defun specialize-generic-instance-name (context-instance-name
                                         generic-instance-name/choice)
  "Given a CONTEXT instance-name keyword (like :player-1), lookup the
GENERIC-INSTANCE-NAME/CHOICE, which is a generic instance name keyword
such as :generic-hardnose-shot or a weighted choice list such
as ((.50 :generic-hardnose-shot) (.25 :generic-super-shot)). Return
return a KEYWORD which is either the specialized name (such
as :player-1-hardnose-shot), or the original symbol or chosen element
from the GENERIC-INSTANCE-NAME/CHOICE if no specialization was found."
  (let ((chosen-instance-name (weighted-choice generic-instance-name/choice)))
    (multiple-value-bind (cin-hash presentp)
        (gethash context-instance-name (instance-specialization-map *assets*))

      (unless presentp
        (return-from specialize-generic-instance-name chosen-instance-name))

      (multiple-value-bind (spec-name-list presentp)
          (gethash chosen-instance-name cin-hash)

        (unless presentp
          (return-from specialize-generic-instance-name chosen-instance-name))

        ;; Until I possibly extend the spec-name-lists, just return the
        ;; first one.
        (car spec-name-list)))))

;; This takes a relative filename based at the installation location
;; of the package.
(defun load-dat-file (filename)
  (let ((entity-hash (make-hash-table :test #'eq))
        (geometry-hash (make-hash-table :test #'eq))
        (instance-equivalences (make-hash-table :test #'eq))
        (instance-specializations (make-hash-table :test #'eq))
        (defined-roles (make-hash-table :test #'eq))
        (collision-plan nil)
        (entities
         (with-open-file (strm
                          ;; We'll look for the data file either in
                          ;; the current working directory, or at the
                          ;; ASDF install location.
                          (or (probe-file filename)
                              (asdf:system-relative-pathname
                               :option-9 filename))
                          :direction :input
                          :if-does-not-exist :error)
           ;; Read the symbols from the point of view of this package
           ;; so later when we make-instance it'll work even if the
           ;; user only "used" our package.
           (let ((*package* (find-package 'option-9)))
             (eval (read strm))))))

    ;; Ensure the thing we expect to be there actually are.
    (assert (member :defined-roles entities))
    (assert (member :collision-plan entities))
    (assert (member :instance-equivalence entities))
    (assert (member :instance-specialization-map entities))
    (assert (member :geometries entities))
    (assert (member :entities entities))

    ;; TODO typechecking and loading are intermixed. I probably can do
    ;; type checking after loading if I'm careful. Need to implement
    ;; that....

    ;; Consume the defined roles
    (loop for i in (cadr (member :defined-roles entities)) do
         (setf (gethash i defined-roles) t))

    ;; Consume and validate the collision-plan
    (setf collision-plan (cadr (member :collision-plan entities)))
    (dolist (plan collision-plan)
      ;; Check that all roles are currently defined.
      (destructuring-bind (fists faces) plan
        (loop for fist in fists do
             (assert (member fist (cadr (member :defined-roles entities)))))
        (loop for face in faces do
             (assert (member face (cadr (member :defined-roles entities)))))))

    ;; Consume the equivalence classes
    (loop for i in (cadr (member :instance-equivalence entities)) do
         (setf (gethash (car i) instance-equivalences) (cadr i)))

    ;; Consume the instance specialization map and convert it into a hash of
    ;; hashes.
    (loop for inst in (cadr (member :instance-specialization-map entities)) do
         (destructuring-bind (context-name mappings) inst
           (let ((spec-hash (make-hash-table :test #'eq)))
             (loop for entry in mappings do
                  (destructuring-bind (generic-name spec-name-list) entry
                    (setf (gethash generic-name spec-hash) spec-name-list)))
             (setf (gethash context-name instance-specializations) spec-hash))))

    ;; Consume the geometry information by processing each file in the
    ;; :geometries list, rip all geometries out and insert it into the
    ;; geometry-hash named by the supplied name associated with the
    ;; geometry.
    (let ((geometry-files (cadr (member :geometries entities))))
      (when geometry-files
        (dolist (geometry-file geometry-files)
          (let ((geometries
                 (with-open-file (gstrm
                                  (or (probe-file geometry-file)
                                      (asdf:system-relative-pathname
                                       :option-9 geometry-file))
                                  :direction :input
                                  :if-does-not-exist :error)
                   (let ((*package* (find-package 'option-9)))
                     (eval (read gstrm))))))
            ;; Insert all geometry forms into the geometry hash keyed
            ;; by the name of the form and whose value is the geometry
            ;; form.
            (loop for (geometry-name geometry) in geometries do
               ;; check to make sure they are all uniquely named
                 (multiple-value-bind (entry presentp)
                     (gethash geometry-name geometry-hash)
                   (declare (ignore entry))
                   (when presentp
                     (error "Geometry name ~A is not unique!" geometry-name)))
               ;; Poke it into the hash table if it is good.
                 (setf (gethash geometry-name geometry-hash) geometry))))))

    ;; Consume the entities into a hash indexed by the :instance in the form.
    (loop for i in (cadr (member :entities entities)) do
       ;; If :geometry exists in the initiargs and it is a symbol,
       ;; ensure it is defined in the geometry-hash
         (let* ((initargs (assoc :initargs i))
                (geometry-name (cadr (member :geometry initargs))))
           (when (and geometry-name (symbolp geometry-name))
             (multiple-value-bind (entry presentp)
                 (gethash geometry-name geometry-hash)
               (declare (ignore entry))
               (unless presentp
                 (error "Entity instance ~A uses a :geometry symbol ~(~S~) which does not exist in the geometry hash table!"
                        i geometry-name)))))
       ;; If the typecheck passed, store it!
         (setf (gethash (cadr (assoc :instance i)) entity-hash) i))


    ;; Create the master object which holds all the assets.
    (make-instance 'assets
                   :defined-roles defined-roles
                   :collision-plan collision-plan
                   :entities entity-hash
                   :geometries geometry-hash
                   :insts/equiv instance-equivalences
                   :instance-specialization-map instance-specializations)))
