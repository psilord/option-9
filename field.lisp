(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun euclidean-distance (fx fy tx ty)
  (let ((factor-1 (- tx fx))
        (factor-2 (- ty fy)))
    (let ((ndist (+ (* factor-1 factor-1)
                    (* factor-2 factor-2))))
      (values (sqrt ndist) ndist))))

(defun unit-vector (from tx ty)
  (with-pvec-accessors (o (matrix-translate-get (world-basis from)))
    (multiple-value-bind (dist ndist)
        (euclidean-distance ox oy tx ty)
      (values (/ (- tx ox) dist)
              (/ (- ty oy) dist)
              ndist))))

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
      ;; Since this is so expensive, ensure to only do it for things that
      ;; are actually alive at the computation of this field.
      (when (alivep q)
        (with-pvec-accessors (o (matrix-translate-get (world-basis q)))
          (if (< (euclidean-distance ox oy tx ty)
                 (radius q))
              (return-from e-field (values :collision 0 0 q))
              (multiple-value-bind (qx qy)
                  (e-field-one-point q tx ty)
                (incf nx qx)
                (incf ny qy))))))
    (values :tracing (* const nx) (* const ny) nil)))

(defun normalize-vector (dx dy)
  (let ((dist (euclidean-distance 0 0 dx dy)))
    (if (= dist 0)
        (values 0 0)
        (values (/ dx dist) (/ dy dist)))))

(defun e-field-direction (dx dy)
  (normalize-vector dx dy))

;; This is a random dumping ground for the field and tesla-field class until
;; I figure out exactly how these verbs work.

;; This will help us keep up to date numerous portions of the
;; telsa-field when the range for the tesla field changes.
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

;; Allocate the whole array which could hold up to 'steps' locations.
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

(defun make-pathcontact ()
  (make-instance 'pathcontact))

(defun gen-paths (num-paths steps)
  (make-array (list num-paths)
              :initial-contents
              (loop repeat num-paths collecting
                   (make-fieldpath steps))))

;; Return true if c1 and c2 are both positive, or both negative, charges
(defun same-polarityp (c1 c2)
  (or (and (> (charge c1) 0) (> (charge c2) 0))
      (and (< (charge c1) 0) (< (charge c2) 0))))



;; This will help us keep up to date numerous portions of the
;; telsa-field when the density (or number of path lines in the field
;; we will be tracing) for the tesla field changes.
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

(defmethod power-density-maxp ((tf tesla-field))
  (if (= (power-lines tf) 4)
      t
      nil))

(defmethod increase-range ((tf tesla-field))
  (unless (power-range-maxp tf)
    (incf (power-range tf))))

(defmethod increase-density ((tf tesla-field))
  (unless (power-density-maxp tf)
    (incf (power-lines tf))))

;; Starting at x,y trace a field line until we get close to a charge
;; which is not q1 or we go out of bounds.
(defmethod trace-field-line ((f field) path-num tx ty q1 charges)
  (labels ((determine-field-line-direction (vx vy dx dy q)
             (with-pvec-accessors (qo (matrix-translate-get (world-basis q)))
               (multiple-value-bind (dx dy)
                   (e-field-direction dx dy)
                 ;; than vx vy is by itself, return -1, otherwise 1
                 (let ((nx (+ vx (* .1 dx)))
                       (ny (+ vy (* .1 dy))))
                   (if (< (euclidean-distance qox qoy nx ny)
                          (euclidean-distance qox qoy vx vy))
                       -1.0
                       1.0)))))

           (store-contact (f path-id entity-id xl yl)
             (declare (ignorable xl yl))
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
        (when (or (< vx 0.0)
                  (> vx (game-width *game*))
                  (< vy 0.0)
                  (> vy (game-height *game*)))
          (return-from trace-field-line))

        ;; direction and follow the stream backwards.
        (multiple-value-bind (classification ex ey cent)
            (e-field charges vx vy)
          (declare (ignorable cent))

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

                      (when (or (< vx 0.0)
                                (> vx (game-width *game*))
                                (< vy 0.0)
                                (> vy (game-height *game*)))
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
                          (let* ((delta (/ (+ index 25.0) 450.0))
                                 (incremental (if (< sum 2.0)
                                                  ;; the 100.0 is
                                                  ;; there because
                                                  ;; this value is
                                                  ;; resized according
                                                  ;; to the raw size
                                                  ;; of the world
                                                  ;; which is (0,0) to
                                                  ;; (100,100).
                                                  (* delta delta 100.0)
                                                  1.5)))
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
;; space,  trace each field-line whose number depends upon the charge.
(defmethod trace-field-lines ((f field) q1 charges)
  ;; we're going to circle around the charge in even increments according
  ;; to num-paths
  (let* ((num-paths (num-paths f))
         (delta (/ (* 2.0 pi) num-paths)))
    (flet ((start-point (x y path-num)
             (values (+ x (* (+ (radius q1) .1) (sin (* path-num delta))))
                     (+ y (* (+ (radius q1) .1) (cos (* path-num delta)))))))
      (dotimes (path-num num-paths)
        (with-pvec-accessors (o (matrix-translate-get (world-basis q1)))
          (multiple-value-bind (nx ny)
              (start-point ox oy path-num)
            (trace-field-line f path-num nx ny q1 charges)))))))

;; return the path-contact instance if the entity was contacted by any
;; field paths, otherwise nil.
(defmethod contacts ((f tesla-field) (e entity))
  (multiple-value-bind (path-contact presentp)
      (gethash (id e) (entity-contacts f))
    (when presentp
      path-contact)))
