(in-package :option-9)

(defmethod render ((ent drawable) scale)
  (destructuring-bind (sx sy) scale
    ;; render a list of possibly differing primitives associated with
    ;; this shape. NOTE: this is realy simple, nothign like texturing
    ;; is supported.
    (dolist (primitive (primitives ent))
      (gl:with-primitive (car primitive)
        ;; render each specific primitive
        (let ((vertex (pvec)))
          (dolist (vertex/color (cdr primitive))
            ;; Apply the point to the world basis of the
            ;; drawable.
            (destructuring-bind ((vx vy vz) (cx cy cz))
                vertex/color
              (gl:color cx cy cz)
              (pv-set-into vertex
                           ;; TODO Remove scaling hack.
                           (coerce (* sx vx) 'double-float)
                           (coerce (* sy vy) 'double-float)
                           vz)
              (pm-apply-into vertex (world-basis ent) vertex)
              (with-pvec-accessors (w vertex)
                (gl:vertex wx wy wz)))))))))


;; Ships have various other things which need to be renderede as well. So
;; do them...
(defmethod render :after ((s ship) scale)

  ;; If there is a passive-gun (which is field-like) render that.
  (when (ship-passive-gun s)
    (render (ship-passive-gun s) scale))

  ;; If there is a shield, render that too.
  (when (ship-main-shield s)
    (render (ship-main-shield s) scale))

  ;; If the hit-points is not equal to the maximum hit points, then
  ;; render the health bar
  (when (/= (hit-points s) (max-hit-points s))
    (with-pvec-accessors (o (pm-get-trans (world-basis s)))
      (destructuring-bind (xscale yscale) scale
        (gl:line-width 4.0)
        (gl:with-primitive :lines
          (let* ((per (/ (hit-points s) (max-hit-points s)))
                 (invper (- 1.0 per)))
            (gl:color 1 1 1)
            ;; start life bar
            (gl:vertex (+ ox (* -4 xscale))
                       (+ oy (* 5 yscale))
                       0d0)
            ;; End life bar
            (gl:vertex (+ ox (* (- 4 (* 8 invper)) xscale))
                       (+ oy (* 5 yscale))
                       0d0)

            ;; start filler
            (gl:color .2 .2 .2)
            (gl:vertex (+ ox (* (- 4 (* 8 invper)) xscale))
                       (+ oy (* 5 yscale))
                       0d0)

            (gl:vertex (+ ox (* 4 xscale))
                       (+ oy (* 5 yscale))
                       0d0)))
        (gl:line-width 1.0)))))

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
                   (gl:vertex (x loc) (y loc) (z loc)))))

             (gl:line-width 1.0)))))
   (entity-contacts f)))

