(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

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
          (initargs (cdr (assoc :initargs info))))

      (assert (eq found-instance instance))
      (assert cls)

      ;; The COPY-SEQ is important because there is a potential to
      ;; modify the full-init args to replace certain named things
      ;; with their actual values.
      (let* ((full-args (copy-seq (append override-initargs initargs)))
             (roles (cadr (member :role full-args))))

        ;; Ensure any specified roles are actually valid!
        (assert (apply #'defined-roles-p *assets* roles))

        ;; Concerning the :primitives initarg value, replace named
        ;; geometries with real geometries. Otherwise, leave it alone
        ;; since I assume it specifies a geometry in place.
        (let* ((?geometry (member :primitives full-args))
               (?geometry-name (cadr ?geometry)))
          (when (and ?geometry ?geometry-name (symbolp ?geometry-name))
            ;; Ok, we found a geometry-name, replace it with the actual
            ;; geometry.
            (multiple-value-bind (geometry presentp)
                (gethash ?geometry-name (geometries *assets*))
              (when (not presentp)
                (error "Cannot find geometry-name ~A in the assets!"
                       ?geometry-name))
              ;; Replace the copy-seq'ed full-args entry for the
              ;; :primitives initarg to be the actual geometry instead
              ;; of its name.
              (setf (cadr ?geometry) geometry))))

        (make-instance-finish
         ;; The values of the override arguments are accepted
         ;; first when in a left to right ordering in the
         ;; argument list in concordance with the ANSI spec.
         (apply #'make-instance cls :game-context *game* full-args))))))

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

;; This takes a relative filename based at the installation location
;; of the package.
(defun load-dat-file (filename)
  (let ((entity-hash (make-hash-table :test #'eq))
        (geometry-hash (make-hash-table :test #'eq))
        (instance-equivalences (make-hash-table :test #'eq))
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
             (read strm)))))

    ;; Ensure the thing we expect to be there actually are.
    (assert (member :defined-roles entities))
    (assert (member :collision-plan entities))
    (assert (member :instance-equivalence entities))
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
                     (read gstrm)))))
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
         (setf (gethash (cadr (assoc :instance i)) entity-hash) i))

    ;; Create the master object which holds all the assets.
    (make-instance 'assets
                   :defined-roles defined-roles
                   :collision-plan collision-plan
                   :entities entity-hash
                   :geometries geometry-hash
                   :insts/equiv instance-equivalences)))
