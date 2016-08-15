(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defmethod render (ent jutter-interpolant)
  nil)

(defparameter *hackmax* (* 60 4)) ;; in seconds.
(defparameter *hack* (make-hash-table :test 'equal))
(defparameter *hackiter* 0)
(defstruct hackit
  (replayingp NIL)
  (write-index 0)
  (read-index 0)
  (transforms (make-array *hackmax* :initial-element NIL)))

(defmethod render ((ent drawable) jutter-interpolant)
  (declare (ignorable jutter-interpolant))
  (let ((geometry (geometry ent))
        ;; And compute the rendering interpolation to remove juttering.
        (model-interpolated
         (interpolate-transform-matricies (previous-world-basis ent)
                                          (world-basis ent)
                                          jutter-interpolant)))

    (when NIL
      ;; ONE BIG HACK to cycle N frames of data.
      (multiple-value-bind (hit presentp)
          (gethash ent *hack*)
        ;; If I haven't recorded any for this entity, do so and keep going
        (unless presentp
          (let ((nhit (make-hackit)))
            (setf (gethash ent *hack*) nhit)
            (setf hit nhit)))

        ;; If I do have one, see if I've writetn the max I could into it.
        (cond
          ((hackit-replayingp hit)
           (setf model-interpolated
                 (aref (hackit-transforms hit) (hackit-read-index hit)))
           (incf (hackit-read-index hit))
           ;; reset to zero if gone too fr.
           (when (= (hackit-read-index hit) *hackmax*)
             (setf (hackit-read-index hit) 0)))
          (t
           ;; not replaying, so storing.
           (setf (aref (hackit-transforms hit) (hackit-write-index hit))
                 model-interpolated)
           (incf (hackit-write-index hit))
           (when (= (hackit-write-index hit) *hackmax*)
             (setf (hackit-replayingp hit) T))))))



    ;; only render if we actually have a geometry to render.
    (when geometry

      ;; Give opengl my world-basis so all verticies are computed by ogl.
      (gl:matrix-mode :modelview)
      ;; Push matrix because I have the inverted camera view matrix
      ;; there already.
      (gl:push-matrix)

      ;; Crap. Implement quaternions to jutter-interpolate between the
      ;; previous and current model matrix before putting it into
      ;; opengl. Otherwise, I'd have to manually compute each vertex
      ;; interpolation.

      ;; Then put the object into the camera's coordinate system.
      (gl:mult-matrix (matrix-convert-to-opengl model-interpolated))

      ;; render a list of possibly differing primitives associated with
      ;; this shape. NOTE: this is really simple, nothing like texturing
      ;; is supported.
      (dolist (primitive (primitives geometry))
        (gl:with-primitive (car primitive)
          ;; render each specific primitive
          (dolist (vertex/color (cdr primitive))
            (destructuring-bind ((vx vy vz) (cx cy cz))
                vertex/color
              (gl:color cx cy cz)
              (gl:vertex vx vy vz)))))

      (gl:pop-matrix)


      (when (hudp ent)
        ;; If the hit-points is not equal to the maximum hit points, then
        ;; render the health bar
        (when (/= (hit-points ent) (max-hit-points ent))

          ;; Push matrix because I have the inverted camera view matrix
          ;; there already.
          (gl:push-matrix)

          ;; NOTE: I want the hud to always be aligned with the world
          ;; axis (as opposed to in the local coordinate space of the
          ;; entity) and at the world location of the entity. So
          ;; create a new matrix with an identity rotation operator
          ;; and the translation to get the HUD to the right place. If
          ;; I were writing a 3D game, I'd align the hud's xy plane to
          ;; be perpendicular to the camera.
          (gl:mult-matrix
           (matrix-convert-to-opengl
            (mtr (matrix-translate-get model-interpolated))))

          ;; draw with respect to local coordinate system.
          (with-pvec-accessors (o (pvec 0d0 0d0 0d0))
            (gl:line-width 4.0)
            (gl:with-primitive :lines
              (let* ((per (/ (hit-points ent) (max-hit-points ent)))
                     (invper (- 1.0 per)))
                (gl:color 1 1 1)
                ;; start life bar
                (gl:vertex (+ ox -4)
                           (+ oy 5)
                           0d0)
                ;; End life bar
                (gl:vertex (+ ox (- 4 (* 8 invper)))
                           (+ oy 5)
                           0d0)

                ;; start filler
                (gl:color .2 .2 .2)
                (gl:vertex (+ ox (- 4 (* 8 invper)))
                           (+ oy 5)
                           0d0)

                (gl:vertex (+ ox 4)
                           (+ oy 5)
                           0d0)))
            (gl:line-width 1.0))

          (gl:pop-matrix))))))

;; Draw the field lines before any actual geometry of the weapon. We
;; do this by walking the entity-contacts hash table which lets us
;; know how to render the various paths. We render this before the
;; regular render so any :primitives for the passive-gun get rendered
;; on top of the passive effect.
(defmethod render :before ((f tesla-field) jutter-interpolant)

  ;; The tesla field is rendered in the world coordiante system.
  (gl:matrix-mode :modelview)
  (gl:push-matrix)

  ;; Ok, the fields line positions were already computed to be in exactly
  ;; world space at the correct location of the ship and aligned with the
  ;; world axes. So, we do not transform them again and instead draw them
  ;; at the same place we computed them.
  ;; NOTE: I may change this in the future to compute the lines in a different
  ;; coordinate system.
  (gl:mult-matrix (matrix-convert-to-opengl (matrix-identity)))

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
                   (gl:vertex (x loc) (y loc) (z loc)))))

             (gl:line-width 1.0)))))
   (entity-contacts f))

  (gl:pop-matrix))
