(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun euclidean-distance (fx fy tx ty)
  (let ((factor-1 (- tx fx))
        (factor-2 (- ty fy)))
    (let ((ndist (+ (* factor-1 factor-1)
                    (* factor-2 factor-2))))
      (values (sqrt ndist) ndist))))

(defun unit-vector (from tx ty)
  (multiple-value-bind (dist ndist)
      (euclidean-distance (x from) (y from) tx ty)
    (values (/ (- tx (x from)) dist)
            (/ (- ty (y from)) dist)
            ndist)))

(defun e-field-one-point (from tx ty)
  (multiple-value-bind (ux uy ndist)
      (unit-vector from tx ty)
    (let ((c (/ (charge from) ndist)))
      (values (* c ux)
              (* c uy)))))

(defun e-field (charges tx ty)
  (let* ((const (/ 1.0 (* 4.0 pi 8.854187817e-12)))
         (nx 0) (ny 0))
    (dolist (q charges)
      (if (< (euclidean-distance (x q) (y q) tx ty)
             (radius q))
          (return-from e-field (values :collision 0 0 q))
          (multiple-value-bind (qx qy)
              (e-field-one-point q tx ty)
            (incf nx qx)
            (incf ny qy))))
    (values :tracing (* const nx) (* const ny) nil)))

(defun normalize-vector (dx dy)
  (let ((dist (euclidean-distance 0 0 dx dy)))
    (if (= dist 0)
        (values 0 0)
        (values (/ dx dist) (/ dy dist)))))

(defun e-field-direction (dx dy)
  (normalize-vector dx dy))

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

(defun gen-path (steps)
  (make-array (list steps)
              :initial-contents
              (loop repeat steps collecting (make-instance 'location))))

;; When we construct a fieldpath class, we initially set up all of the
;; memory we'll need to deal with it. Since we'll compute a fair
;; amount of information per frame, we don't want to generate too much
;; garbage.
(defun make-fieldpath (max-steps)
  (make-instance 'fieldpath :path (gen-path max-steps)))

(defclass pathcontact ()
  ((%number-of-contacts :initarg :number-of-contacts
                        :initform 0
                        :accessor number-of-contacts)
   (%path-ids :initarg :contacts
              :initform nil
              :accessor path-ids))
  (:documentation "The Path Contact Class"))

(defun make-pathcontact ()
  (make-instance 'pathcontact))

(defclass field ()
  ((%range :initarg :range
           :initform 1
           :accessor range)
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

(defclass tesla-field (field weapon)
  ((%power-range :initarg :power-range
                 :initform 1
                 :reader power-range)
   (%power-lines :initarg :power-lines
                 :initform 1
                 :reader power-lines))
  (:documentation "The Tesla-Field Class"))

(defun gen-paths (num-paths steps)
  (make-array (list num-paths)
              :initial-contents
              (loop repeat num-paths collecting
                   (make-fieldpath steps))))

(defmethod (setf power-range) (range (tf tesla-field))
  (assert (> range 0))
  ;; The power range of the tesla-field can range from 1 to 7
  (setf (slot-value tf '%power-range) (min range 7))
  ;; Now we regenerate all of the internals.
  ;; First we figure out the real range of the traces, capped at 2^7 for
  ;; efficiency (otherwise we could waste 25% of our time in oscillations
  ;; or other integration foibles when computing the trace).
  (setf (range tf) (min (expt 2 (power-range tf))
                        (expt 2 7)))

  ;; Remake the paths array
  (setf (paths tf) (gen-paths (num-paths tf) (range tf)))

  ;; Clear out the contacts hash since it is now invalid
  (setf (entity-contacts tf) (make-hash-table :test #'equal))

  range)

(defmethod (setf power-lines) (lines (tf tesla-field))
  (assert (> lines 0))

  (setf (slot-value tf '%power-lines) (min lines 4))
  (setf (num-paths tf) (expt 2 (power-lines tf)))
  (setf (paths tf) (gen-paths (num-paths tf) (range tf)))
  (setf (entity-contacts tf) (make-hash-table :test #'equal))
  lines)

(defmethod power-range-maxp ((tf tesla-field))
  (if (= (power-range tf) 7)
      t
      nil))

(defmethod power-lines-maxp ((tf tesla-field))
  (if (= (power-lines tf) 4)
      t
      nil))

(defmethod increase-power-randomly ((tf tesla-field))
  (if (zerop (random 2))
      (if (power-lines-maxp tf)
          (incf (power-range tf))
          (incf (power-lines tf)))
      (if (power-range-maxp tf)
          (incf (power-lines tf))
          (incf (power-range tf)))))

;; Return true if c1 and c2 are both positive, or both negative, charges
(defun same-polarityp (c1 c2)
  (or (and (> (charge c1) 0) (> (charge c2) 0))
      (and (< (charge c1) 0) (< (charge c2) 0))))

;; Starting at x,y trace a field line until we get close to a charge
;; which is not q1 or we go out of bounds.
(defmethod trace-field-line ((f field) path-num tx ty q1 charges)
  (labels ((determine-field-line-direction (vx vy dx dy q)
             (multiple-value-bind (dx dy)
                 (e-field-direction dx dy)
               ;; than vx vy is by itself, return -1, otherwise 1
               (let ((nx (+ vx (* .001 dx)))
                     (ny (+ vy (* .001 dy))))
                 (if (< (euclidean-distance (x q) (y q) nx ny)
                        (euclidean-distance (x q) (y q) vx vy))
                     -1.0
                     1.0))))

           (store-contact (f path-id entity-id xl yl)
             (multiple-value-bind (path-contact presentp)
                 (gethash entity-id (entity-contacts f))

               ;; Is there a better idiom for this?
               (unless presentp
                 (let ((pc (make-pathcontact)))
                   (setf (gethash entity-id (entity-contacts f)) pc)
                   (setf path-contact pc)))

               (incf (number-of-contacts path-contact))
               (push path-id (path-ids path-contact)))))

    (with-accessors ((fp-steps steps) (fp-path path))
        (svref (paths f) path-num)
      (setf fp-steps 0)

      (let ((vx tx) (vy ty) (sum 0))
        ;; First, we compute the field direction at the initial point, if
        ;; by following that vector, we get closer to q1, we'll reverse
        (when (or (< vx 0.0) (> vx 1.0) (< vy 0.0) (> vy 1.0))
          (return-from trace-field-line))

        ;; direction and follow the stream backwards.
        (multiple-value-bind (classification ex ey cent)
            (e-field charges vx vy)

          ;; XXX For traced paths which START in a colliding
          ;; situation, we ignore them. This might be bad and needs
          ;; revisiting.
          (when (eq classification :tracing)
            (multiple-value-bind (dx dy) (e-field-direction ex ey)
              (let ((dir (determine-field-line-direction vx vy dx dy q1)))
                ;; Now, we begin storing the line strip moving the
                ;; test charge from the start point to wherever it ends up
                ;; ensuring it goes in the right direction.

                ;; Keep walking the field line until we are done. We
                ;; bound it to (range f) to keep the oscillations from
                ;; the gross integration from overwhelming the
                ;; computation.
                (when
                    (dotimes (index (range f) t)

                      (when (or (< vx 0) (> vx 1) (< vy 0) (> vy 1))
                        ;; we went off the screen, so no contact.
                        (return t))

                      (multiple-value-bind (classification ex ey cent)
                          (e-field charges vx vy)

                        ;; If we collide and the entity we collided with
                        ;; is of an opposite charge, we're done.
                        (when (and (eq classification :collision)
                                   (not (same-polarityp q1 cent)))
                          (setf (x (svref fp-path index)) vx
                                (y (svref fp-path index)) vy)
                          (incf fp-steps)

                          ;; Associate the contacting path-num with
                          ;; the id of to whom it collided and where.
                          (store-contact f path-num (id cent) vx vy)
                          (return nil))

                        ;; otherwise we keep tracing the path
                        (multiple-value-bind (dx dy)
                            (e-field-direction ex ey)

                          (setf (x (svref fp-path index)) vx
                                (y (svref fp-path index)) vy
                                (dx (svref fp-path index)) dx
                                (dy (svref fp-path index)) dy)
                          (incf fp-steps)

                          ;; No direction guarantees that we loop
                          ;; until done with no path movement, so we
                          ;; bail.
                          (when (and (= dx 0) (= dy 0))
                            ;; we died in a saddle point, no contact.
                            (return t))

                          ;; Stepping to the next point is a little
                          ;; interesting. We use a quadratic function
                          ;; near the field generator to increase the
                          ;; resolution of the field lines. As we go
                          ;; farther away from the generator, we then
                          ;; fixate the resolution to a ad hoc number
                          ;; which looked good and was cheap to
                          ;; compute for game play. This allows for us
                          ;; to decrease the ugly oscillations right
                          ;; nearby the ship, but not have to pay for
                          ;; that oscillation reduction farther away
                          ;; where oscillations are less likely to
                          ;; happen. This equation to compute the
                          ;; incremental was pulled out of my butt.
                          (let* ((delta (/ (+ index 25) 450))
                                 (incremental (if (< sum .15)
                                                  (* delta delta)
                                                  .015)))
                            (incf sum incremental)
                            (setf vx (+ vx (* dx incremental dir))
                                  vy (+ vy (* dy incremental dir)))))))

                  ;; If the dotimes returned true, it meant that the
                  ;; path did not collide with any game entity but
                  ;; instead ran out of range, got clipped, or
                  ;; whatever. So it gets a special designation in the
                  ;; entity-contacts table.
                  (store-contact f path-num :no-collision vx vy))))))))))


;; Starting at slightly more than radius from the object in world
;; space, render each field-line whose number depends upon the charge.
(defmethod trace-field-lines ((f field) q1 charges)
  ;; we're going to circle around the charge in even increments according
  ;; to num-paths
  (let* ((num-paths (num-paths f))
         (delta (/ (* 2.0 pi) num-paths)))
    (flet ((start-point (x y path-num)
             (values (+ x (* (+ (radius q1) .001) (sin (* path-num delta))))
                     (+ y (* (+ (radius q1) .001) (cos (* path-num delta)))))))
      (dotimes (path-num num-paths)
        (multiple-value-bind (nx ny)
            (start-point (x q1) (y q1) path-num)
          (trace-field-line f path-num nx ny q1 charges))))))

;; Actually generate the field and compute path contact information.
(defmethod generate ((f field) source-charge charges)
  ;; Clear the contact hash because we're doing a new trace of the field.
  (clrhash (entity-contacts f))
  (trace-field-lines f source-charge charges))

;; return the path-contact instance if the entity was contacted by any
;; field paths, otherwise nil.
(defmethod contacts ((f tesla-field) (e entity))
  (multiple-value-bind (path-contact presentp)
      (gethash (id e) (entity-contacts f))
    (when presentp
      path-contact)))

;; Ships can have passive-guns, so here we compute if the passive gun
;; hit the other collidable. The face will hit back in this
;; context. :)
(defmethod collide :before ((fist collidable) (face ship))
  (when (ship-passive-gun face)
    (collide (ship-passive-gun face) fist)))

;; If any field lines hit the face, perform the collision with it.
(defmethod collide ((f tesla-field) (face collidable))
  (when (contacts f face)
    (perform-collision f face)))

(defmethod damage ((f tesla-field) (ent collidable))
  (let ((path-contact (contacts f ent)))
    (decf (hit-points ent) (ceiling (number-of-contacts path-contact) 10))
    (when (<= (hit-points ent) 0)
      (die ent))))

;; In general, the field damages the thing it touches.
(defmethod perform-collision ((f tesla-field) (ent collidable))
  (damage f ent))

;; field mines follow the field back to the generating ship and are
;; NOT damaged by the field, therefore they are a new primary method.
(defmethod perform-collision ((f tesla-field) (ent field-mine))
  (let ((pc (contacts f ent))
        (mx 0)
        (my 0))
    ;; add up the inverse direction vectors from the participating
    ;; field paths. We're going to be following the field lines
    ;; back...
    (dolist (path-index (path-ids pc))
      (with-accessors ((fp-steps steps) (fp-path path))
          (svref (paths f) path-index)
        ;; Sum the inverted direction vectors found at the last field
        ;; path step. XXX It turns out when the last step was computed, it
        ;; is often past the object (since it was in the collision radius).
        ;; This messes up the computation of this vector so I use the second
        ;; to the last step, which should be ok in accordance to the algorithm
        ;; which produced the path.
        (when (> fp-steps 2)
          (let ((loc (svref fp-path (1- (1- fp-steps)))))
            (incf mx (* (dx loc) -1))
            (incf my (* (dy loc) -1))))))

    ;; Normalize the vector, set some fraction of it to be the
    ;; direction and speed the mine should move towards the ship.
    (multiple-value-bind (nvx nvy) (normalize-vector mx my)
      (setf (dx ent) (* nvx .004)
            (dy ent) (* nvy .004)))))

;; Draw the field lines before any actual geometry of the weapon. We
;; do this by walking the entity-contacts hash table which lets us
;; know how to render the various paths. We render this before the
;; regular render so any :primitives for the passive-gun get rendered
;; on top of the passive effect.
(defmethod render :before ((f tesla-field) scale)
  (declare (ignorable scale))

  (maphash
   #'(lambda (eid path-contact)
       (with-accessors ((pc-number-of-contacts number-of-contacts)
                        (pc-path-ids path-ids)) path-contact
         (dolist (path-index pc-path-ids)
           ;; get the path which corresponds to the path-id at the
           ;; index
           (with-accessors ((fp-steps steps) (fp-path path))
               (svref (paths f) path-index)
             ;; walk the path vector while drawing it
             (cond
               ((equal eid :no-collision)
                (gl:line-width 1.0)
                (gl:color .1 .1 .3))
               (t
                (gl:line-width 2.0)
                (let ((r (random .3)))
                  (gl:color (+ .1 r) (+ .1 r) (+ .2 (random .8))))))

             (gl:with-primitive :line-strip
               (dotimes (vertex-index fp-steps)
                 (let ((loc (svref fp-path vertex-index)))
                   (gl:vertex (x loc) (y loc) 0))))

             (gl:line-width 1.0)

             ;; If the contact point exists, draw it and emit some sparks
             ;; from it.
             (unless (or (equal eid :no-collision) (zerop fp-steps))
               (let ((loc (svref fp-path (1- fp-steps))))
                 (when (zerop (random 10))
                   (emit-sparks 1 (x loc) (y loc)
                                :ttl-max 5 :velocity-factor 1/20))))))))
   (entity-contacts f)))


