(in-package :option-9)

#+(or (not option-9-optimize-pvec) option-9-debug)
(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defparameter *pvec-tol* (as-double-float 10d-10))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype pvec () `(simple-array double-float (3)))
  (defstruct (pvec
               (:type (vector double-float))
               (:constructor make-pvec)
               (:constructor pvec (&optional x y z)))

    (x 0.0d0 :type double-float)
    (y 0.0d0 :type double-float)
    (z 0.0d0 :type double-float))

  (defmacro with-pvec-accessors ((prefix-symbol pvec) &body body)
    `(with-accessors
           ((,(make-accessor-symbol prefix-symbol "X") pvec-x)
            (,(make-accessor-symbol prefix-symbol "Y") pvec-y)
            (,(make-accessor-symbol prefix-symbol "Z") pvec-z))
         ,pvec
       ,@body))


  (defmacro with-multiple-pvec-accessors (sbinds &body body)
    (if (null sbinds)
        `(progn ,@body)
        `(with-pvec-accessors ,(car sbinds)
           (with-multiple-pvec-accessors ,(cdr sbinds) ,@body))))

  ;; NOTE: I must use the pprint-dispatch table to emit nicely formatted
  ;; pvecs because they aren't a CLASS due to the defstruct definition
  ;; I am using. So PRINT-OBJECT doesn't work on PVEC types.
  (set-pprint-dispatch
   'pvec #'(lambda (str pobj)
             (with-pvec-accessors (p pobj)
               (print-unreadable-object (pobj str)
                 (format str "[~A ~A ~A]" px py pz))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-copy-into))
(declaim (inline vect-copy-into))
(defun vect-copy-into (dst src)
  "Copy the SRC into the DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d dst) (s src))
    (psetf dx sx
           dy sy
           dz sz))
  dst)

(declaim (ftype (function (pvec pvec) pvec) vcopyi))
(declaim (inline vcopyi))
(defun vcopyi (dst src)
  "Shortname for VECT-COPY-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-copy-into dst src))

 ;;; ;;;;;;;;

(declaim (ftype (function (pvec) pvec) vect-copy))
(declaim (inline vect-copy))
(defun vect-copy (pv)
  "Return a newly allocated copy of PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vcopyi (pvec) pv))

(declaim (ftype (function (pvec) pvec) vcopy))
(declaim (inline vcopy))
(defun vcopy (pv)
  "Sortname for VECT-COPY."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-copy pv))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec) pvec) vect-zero-into))
(declaim (inline vect-zero-into))
(defun vect-zero-into (pv)
  "Zero each element of PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (psetf px 0.0d0
           py 0.0d0
           pz 0.0d0))
  pv)

(declaim (ftype (function (pvec) pvec) vzeroi))
(declaim (inline vzeroi))
(defun vzeroi (pv)
  "Shortname for VECT-ZERO-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-zero-into pv))

;;; ;;;;;;;;

(declaim (ftype (function () pvec) vect-zero))
(declaim (inline vect-zero))
(defun vect-zero ()
  "Allocate a vector with all elements being 0d0 and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pvec))

(declaim (ftype (function () pvec) vzero))
(declaim (inline vzero))
(defun vzero ()
  "Shortname for VECT-ZERO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-zero))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec double-float double-float double-float) pvec)
                vect-set-into))
(declaim (inline vect-set-into))
(defun vect-set-into (pv x y z)
  "Assign X Y Z into PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (psetf px (as-double-float x)
           py (as-double-float y)
           pz (as-double-float z)))
  pv)

(declaim (ftype (function (pvec double-float double-float double-float) pvec)
                vseti))
(declaim (inline vseti))
(defun vseti (pv x y z)
  "Shortname for VECT-SET-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-set-into pv x y z))

;;; ;;;;;;;;

(declaim (ftype (function (double-float double-float double-float) pvec)
                vect-set))
(declaim (inline vect-set))
(defun vect-set (x y z)
  "Allocate and return a new pvec and assign its :x :y and :z elements
with X Y and Z."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pvec x y z))

(declaim (ftype (function (double-float double-float double-float) pvec)
                vset))
(declaim (inline vset))
(defun vset (x y z)
  "Shortname for VECT-SET."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-set x y z))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vect-get (pv &key (multiple-value t))
  "Return either VALUES of the x, y, and z elements, or as a list,
depending on the value of the keyword argument :MULTIPLE-VALUE, which
defaults to T."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (if multiple-value
        (values
         (as-double-float px)
         (as-double-float py)
         (as-double-float pz))
        (list
         (as-double-float px)
         (as-double-float py)
         (as-double-float pz)))))

(defun vget (pv &key (multiple-value t))
  "Shortname for VECT-GET."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-get pv :multiple-value multiple-value))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pvec) vect-clamp-into))
(declaim (inline vect-clamp-into))
(defun vect-clamp-into (dst src &key (min-val least-negative-double-float)
                                  (max-val most-positive-double-float))
  "Clamp all elements from SRC between (inclusive) MIN-VAL and MAX-VAL
values into DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d dst) (s src))
    (macrolet ((clamp (read-place)
                 `(cond
                    ((< (as-double-float ,read-place)
                        (as-double-float min-val))
                     (as-double-float min-val))

                    ((> (as-double-float ,read-place)
                        (as-double-float max-val))
                     (as-double-float max-val))
                    (t
                     ,read-place
                     ))))
      (psetf dx (clamp sx)
             dy (clamp sy)
             dz (clamp sz))
      dst)))

(declaim (ftype (function (pvec pvec &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pvec) vclampi))
(declaim (inline vclampi))
(defun vclampi (dst src &key (min-val least-negative-double-float)
                          (max-val most-positive-double-float))
  "Shortname for VECT-CLAMP-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-clamp-into dst src :min-val min-val :max-val max-val))

;;; ;;;;;;;;

(declaim (ftype (function (pvec &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pvec) vect-clamp))
(declaim (inline vect-clamp))
(defun vect-clamp (src &key (min-val least-negative-double-float)
                         (max-val most-positive-double-float))
  "Allocate a new pvec and return the clamped SRC in it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vclampi (pvec) src :min-val min-val :max-val max-val))


(declaim (ftype (function (pvec &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pvec) vclamp))
(declaim (inline vclamp))
(defun vclamp (src &key (min-val least-negative-double-float)
                     (max-val most-positive-double-float))
  "Shortname for VECT-CLAMP."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-clamp src :min-val min-val :max-val max-val))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-negate-into))
(declaim (inline vect-negate-into))
(defun vect-negate-into (dst src)
  "Negate the vector in SRC and store into DST. Return DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d dst) (s src))
    (psetf dx (as-double-float (- sx))
           dy (as-double-float (- sy))
           dz (as-double-float (- sz))))
  dst)

(declaim (ftype (function (pvec pvec) pvec) vnegi))
(declaim (inline vnegi))
(defun vnegi (dst src)
  "Shortname for VECT-NEGATE-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-negate-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pvec) vect-negate))
(declaim (inline vect-negate))
(defun vect-negate (src)
  "Store the negation of SRC into a newly created pvec and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vnegi (pvec) src))

(declaim (ftype (function (pvec) pvec) vneg))
(declaim (inline vneg))
(defun vneg (src)
  "Shortname for VECT-NEGATE."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-negate src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-stabilize-into))
(declaim (inline vect-stabilize-into))
(defun vect-stabilize-into (dst src)
  "If any element in SRC is < than *pvec-tol*, force it to 0.0d0 when
storing it into into DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d dst) (s src))
    (macrolet ((stabilize (slot)
                 `(if (< (as-double-float (abs ,slot))
                         (as-double-float *pvec-tol*))
                      0d0
                      ,slot)))
      (psetf dx (stabilize sx)
             dy (stabilize sy)
             dz (stabilize sz))))
  dst)

(declaim (ftype (function (pvec pvec) pvec) vstabi))
(declaim (inline vstabi))
(defun vstabi (dst src)
  "Shortname for VECT-STABILIZE-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-stabilize-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pvec) vect-stabilize))
(declaim (inline vect-stabilize))
(defun vect-stabilize (src)
  "Allocate a new pvec and return the stabilized PV in it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vstabi (pvec) src))

(declaim (ftype (function (pvec) pvec) vstab))
(declaim (inline vstab))
(defun vstab (src)
  "Shortname for VECT-STABILIZE."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-stabilize src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec) double-float) vect-norm))
(declaim (inline vect-norm))
(defun vect-norm (src)
  "Compute the double-float Euclidean magnitude of SRC and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  ;; And remove the compiler note about the boxed double-float I'm trying
  ;; to return.
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-pvec-accessors (s src)
    (the double-float
         (sqrt (as-double-float
                (+ (* sx sx)
                   (* sy sy)
                   (* sz sz)))))))

(declaim (ftype (function (pvec) double-float) vnorm))
(declaim (inline vnorm))
(defun vnorm (src)
  "Shortname for VECT-NORM."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  ;; And remove the compiler note about the boxed double-float I'm trying
  ;; to return.
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (vect-norm src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-normalize-into))
(declaim (inline vect-normalize-into))
(defun vect-normalize-into (dst src)
  "Normalize SRC and put result into DST. Return DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (let ((inv-mag (the double-float (/ (vnorm src)))))
    (with-multiple-pvec-accessors ((d dst) (s src))
      (psetf dx (as-double-float (* sx inv-mag))
             dy (as-double-float (* sy inv-mag))
             dz (as-double-float (* sz inv-mag))))
    dst))

(declaim (ftype (function (pvec pvec) pvec) vnormalizei))
(declaim (inline vnormalizei))
(defun vnormalizei (dst src)
  "Shortname for VECT-NORMALIZE-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-normalize-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pvec) vect-normalize))
(declaim (inline vect-normalize))
(defun vect-normalize (src)
  "Allocate a new pvec that contains the normalization of SRC and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vnormalizei (pvec) src))

(declaim (ftype (function (pvec) pvec) vnormalize))
(declaim (inline vnormalize))
(defun vnormalize (src)
  "Shortname for VECT-NORMALIZE."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-normalize src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec) t) vect-zero-p))
(defun vect-zero-p (src)
  "If all elements are < *pvec-tol* return NIL, otherwise PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (s src)
    (let* ((is-zero-p (and (< (the double-float (abs sx))
                              (the double-float *pvec-tol*))
                           (< (the double-float (abs sy))
                              (the double-float *pvec-tol*))
                           (< (the double-float (abs sz))
                              (the double-float *pvec-tol*)))))
      (if is-zero-p
          src
          nil))))

(declaim (ftype (function (pvec) t) vzerop))
(defun vzerop (src)
  "Shortname for VECT-ZERO-P."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-zero-p src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec pvec) pvec) vect-cross-into))
(declaim (inline vect-cross-into))
(defun vect-cross-into (pvn pvu pvv)
  "Perform the cross product of PVU x PVV and store into PVN. Return PVN."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((n pvn) (u pvu) (v pvv))
    (psetf nx (as-double-float (- (* uy vz) (* uz vy)))
           ny (as-double-float (- (* uz vx) (* ux vz)))
           nz (as-double-float (- (* ux vy) (* uy vx)))))
  pvn)

(declaim (ftype (function (pvec pvec pvec) pvec) vcrossi))
(declaim (inline vcrossi))
(defun vcrossi (pvn pvu pvv)
  "Shortname for VECT-CROSS-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-cross-into pvn pvu pvv))

;;; ;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-cross))
(declaim (inline vect-cross))
(defun vect-cross (pvu pvv)
  "Do a cross product between PVU and PVB and return a new pvec of it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vcrossi (pvec) pvu pvv))

(declaim (ftype (function (pvec pvec) pvec) vcross))
(declaim (inline vcross))
(defun vcross (pvu pvv)
  "Shortname of VECT-CROSS."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-cross pvu pvv))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec pvec) pvec) vect-add-into))
(declaim (inline vect-add-into))
(defun vect-add-into (pvd pva pvb)
  "Vector add PVA and PVB and store into PVD."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((r pvd) (a pva) (b pvb))
    (psetf rx (as-double-float (+ ax bx))
           ry (as-double-float (+ ay by))
           rz (as-double-float (+ az bz))))
  pvd)

(declaim (ftype (function (pvec pvec pvec) pvec) vaddi))
(declaim (inline vaddi))
(defun vaddi (pvd pva pvb)
  "Shortname for VECT-ADD-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-add-into pvd pva pvb))

;;; ;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-add))
(declaim (inline vect-add))
(defun vect-add (pva pvb)
  "Add two vectors PVA and PVB and return a new pvec with the result."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vaddi (pvec) pva pvb))

(declaim (ftype (function (pvec pvec) pvec) vadd))
(declaim (inline vadd))
(defun vadd (pva pvb)
  "Shortname for VECT-ADD."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-add pva pvb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec pvec) pvec) vect-sub-into))
(declaim (inline vect-sub-into))
(defun vect-sub-into (pvd pva pvb)
  "Subtract PVB from PVA and store into PVD. Return PVD."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((r pvd) (a pva) (b pvb))
    (psetf rx (as-double-float (- ax bx))
           ry (as-double-float (- ay by))
           rz (as-double-float (- az bz))))
  pvd)

(declaim (ftype (function (pvec pvec pvec) pvec) vsubi))
(declaim (inline vsubi))
(defun vsubi (pvd pva pvb)
  "Shortname for VECT-SUB-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-sub-into pvd pva pvb))

;;; ;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-sub))
(declaim (inline vect-sub))
(defun vect-sub (pva pvb)
  "Subtract PVB from PVA and return result in a new pvec."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vsubi (pvec) pva pvb))

(declaim (ftype (function (pvec pvec) pvec) vsub))
(declaim (inline vsub))
(defun vsub (pva pvb)
  "Shortname for VECT-SUB."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-sub pva pvb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec pvec) pvec) vect-vect-into))
(declaim (inline vect-vect-into))
(defun vect-vect-into (pvd pva pvb)
  "Create a vector from point PVA to point PVB and stored into PVD. This
is just a conveniently named vector subtraction in a well known context."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vsubi pvd pvb pva))

(declaim (ftype (function (pvec pvec pvec) pvec) vvecti))
(declaim (inline vvecti))
(defun vvecti (pvd pva pvb)
  "Shortname for VECT-VECT-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-vect-into pvd pva pvb))

;;; ;;;;;;;;

(declaim (ftype (function (pvec pvec) pvec) vect-vect))
(declaim (inline vect-vect))
(defun vect-vect (pva pvb)
  "Compute a vector from the point represented in PVA to the point
represented in PVB and return the new pvec vector. This is just a
conveniently named vector subtraction in a well known context."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vvecti (pvec) pva pvb))

(declaim (ftype (function (pvec pvec) pvec) vvect))
(declaim (inline vvect))
(defun vvect (pva pvb)
  "Shortname for VECT-VECT."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-vect pva pvb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec double-float) pvec) vect-scale-into))
(declaim (inline vect-scale-into))
(defun vect-scale-into (dst src scale-factor)
  "Multiply SRC by the DOUBLE-FLOAT SCALE-FACTOR and store into DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d dst) (s src))
    (psetf dx (as-double-float (* sx scale-factor))
           dy (as-double-float (* sy scale-factor))
           dz (as-double-float (* sz scale-factor)))
    dst))

(declaim (ftype (function (pvec pvec double-float) pvec) vscalei))
(declaim (inline vscalei))
(defun vscalei (dst src scale-factor)
  "Shortname for VECT-SCALE-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-scale-into dst src scale-factor))

;;; ;;;;;;;;

(declaim (ftype (function (pvec double-float) pvec) vect-scale))
(declaim (inline vect-scale))
(defun vect-scale (src scale-factor)
  "Allocate and return a new pvec that contains SRC scaled by the
DOUBLE-FLOAT SCALE-FACTOR."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vscalei (pvec) src scale-factor))

(declaim (ftype (function (pvec double-float) pvec) vscale))
(declaim (inline vscale))
(defun vscale (src scale-factor)
  "Shortname for VECT-SCALE."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-scale src scale-factor))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec) double-float) vect-dot))
(declaim (inline vect-dot))
(defun vect-dot (pva pvb)
  "Compute the dot product between the two vectors and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-multiple-pvec-accessors ((a pva) (b pvb))
    (as-double-float
     (+ (* ax bx)
        (* ay by)
        (* az bz)))))

(declaim (ftype (function (pvec pvec) double-float) vdot))
(declaim (inline vdot))
(defun vdot (pva pvb)
  "Shortname for VEC-DOT."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-dot pva pvb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec) double-float) vect-angle))
(defun vect-angle (pva pvb)
  "Compute and return the angle in radians between the two vectors."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((dot (vdot pva pvb))
        (denom (* (vnorm pva) (vnorm pvb))))

    (if (zerop denom)
        0d0
        (as-double-float (acos (/ dot denom))))))

(declaim (ftype (function (pvec pvec) double-float) vangle))
(declaim (inline vangle))
(defun vangle (pva pvb)
  "Shortname for VECT-ANGLE."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-angle pva pvb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec &key (:sqrt t))
                          (values double-float double-float))
                vect-dist))
(defun vect-dist (pva pvb &key (sqrt t))
  "Assume PVA and PVB are points in 3 dimensions and compute the distance
between them. If :SQRT is T, the default, then return the Euclidean distance
as the first value and the non-normalized distance as the second. If :SQRT is
NIL, then do not compute the Euclidean distance and instead return the
non-normalized distance for both values."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((a pva) (b pvb))
    (let* ((factor-1 (as-double-float (- bx ax)))
           (factor-2 (as-double-float (- by ay)))
           (factor-3 (as-double-float (- bz az)))
           (non-normalized (as-double-float (+ (* factor-1 factor-1)
                                               (* factor-2 factor-2)
                                               (* factor-3 factor-3)))))
      (values
       (the double-float
            (if sqrt
                (sqrt (as-double-float non-normalized))
                (as-double-float non-normalized)))
       (as-double-float non-normalized)))))

(declaim (ftype (function (pvec pvec &key (:sqrt t))
                          (values double-float double-float))
                vdist))
(declaim (inline vdist))
(defun vdist (pva pvb &key (sqrt t))
  "Shortname for VECT-DIST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-dist pva pvb :sqrt sqrt))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec &key (:span keyword)) pvec)
                vect-rand-into))
(defun vect-rand-into (dst &key (span :xy))
  "Into DST place a normalized vector which is randomly oriented in
the vector space defined by the :SPAN keyword. :SPAN may be one of:
:X, :Y, :Z, :XZ, :XY, :YZ, :XYZ."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (flet ((rand-one-dim ()
           (* (random-sign) 1d0))

         (rand-two-dim ()
           ;; parametric description of a circle centered at zero radius one.
           (let ((t0 (random (* 2d0 pi))))
             (values (sin t0) (cos t0))))

         (rand-three-dim ()
           (let ((longitude (random (* 2d0 pi)))
                 (colatitude (random (* 1d0 pi))))
             (values (* 1d0 (cos longitude) (sin colatitude))
                     (* 1d0 (sin longitude) (sin colatitude))
                     (* 1d0 (cos colatitude))))))

    (ecase span
      ((:x)
       (vseti dst (rand-one-dim) 0d0 0d0))
      ((:y)
       (vseti dst 0d0 (rand-one-dim) 0d0))
      ((:z)
       (vseti dst 0d0 0d0 (rand-one-dim)))
      ((:xy)
       (multiple-value-bind (x y) (rand-two-dim)
         (vseti dst x y 0d0)))
      ((:xz)
       (multiple-value-bind (x z) (rand-two-dim)
         (vseti dst x 0d0 z)))
      ((:yz)
       (multiple-value-bind (y z) (rand-two-dim)
         (vseti dst 0d0 y z)))
      ((:xyz)
       (multiple-value-bind (x y z) (rand-three-dim)
         (vseti dst x y z))))

    ;; already normalized by virtue of construction.
    dst))

(declaim (ftype (function (pvec &key (:span keyword)) pvec)
                vrandi))
(declaim (inline vrandi))
(defun vrandi (dst &key (span :xy))
  "Shortname for VECT-RAND-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-rand-into dst :span span))

;;; ;;;;;;;;

(declaim (ftype (function (&key (:span keyword)) pvec)
                vect-rand))
(declaim (inline vect-rand))
(defun vect-rand (&key (span :xy))
  "Return a newly allocated normalized vector which is randomly
oriented in the vector space defined by the :SPAN keyword. :SPAN may
be one of:
:X, :Y, :Z, :XZ, :XY, :YZ, :XYZ."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vrandi (pvec) :span span))

(declaim (ftype (function (&key (:span keyword)) pvec)
                vrand))
(declaim (inline vrand))
(defun vrand (&key (span :xy))
  "Shortname for VECT-RAND-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-rand :span span))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (double-float double-float double-float) double-float)
                dlerp))
(declaim (inline dlerp))
(defun dlerp (a b interp)
  "Perform a linear interpolation from double-float A to double-float B
with double-float INTERP being the interpolant value. Return the interpolated
value as a double-float."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (as-double-float
   (+ (* (- 1d0 interp) a) (* interp b))))

(defun vect-interpolate-into (dst from to interp &key (interp-func #'dlerp))

  "Interpolate from pvec FROM to pvec TO by INTERP. Put result into pvec DST
and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d dst) (f from) (e to))
    (psetf dx (funcall interp-func fx ex interp)
           dy (funcall interp-func fy ey interp)
           dz (funcall interp-func fz ez interp)))
  dst)

(defun vinterpi (dst from to interp &key (interp-func #'dlerp))
  "Shortname for VECT-INTERPOLATE-INTO."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-interpolate-into dst from to interp :interp-func interp-func))

;;; ;;;;;;;;

(defun vect-interpolate (from to interp &key (interp-func #'dlerp))
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-interpolate-into (pvec) from to interp :interp-func interp-func))

(defun vinterp (from to interp &key (interp-func #'dlerp))
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (vect-interpolate from to interp :interp-func interp-func))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
