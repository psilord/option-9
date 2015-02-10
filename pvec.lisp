(in-package :option-9)

#-option-9-optimize-pvec
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
           ((,(alexandria:symbolicate prefix-symbol "X") pvec-x)
            (,(alexandria:symbolicate prefix-symbol "Y") pvec-y)
            (,(alexandria:symbolicate prefix-symbol "Z") pvec-z))
         ,pvec
       ,@body))


  (defmacro with-multiple-pvec-accessors (sbinds &body body)
    (if (null sbinds)
        `(progn ,@body)
        `(with-pvec-accessors ,(car sbinds)
           (with-multiple-pvec-accessors ,(cdr sbinds) ,@body))))

  ;; NOTE: I must use the pprint-dispatch table to emit nicely formated
  ;; pvecs because they aren't a CLASS due to the defstruct definition
  ;; I am using. So PRINT-OBJECT doesn't work on PVEC types.
  (set-pprint-dispatch
   'pvec #'(lambda (str pobj)
             (with-pvec-accessors (p pobj)
               (print-unreadable-object (pobj str)
                 (format str "[~A ~A ~A]" px py pz))))))

(declaim (ftype (function (pvec pvec) pvec) pv-copy-into))
(declaim (inline pv-copy-into))
(defun pv-copy-into (pvdst pvsrc)
  "Copy the PVSRC into the PVDST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((d pvdst) (s pvsrc))
    (psetf dx sx
           dy sy
           dz sz))
  pvdst)

(declaim (ftype (function (pvec) pvec) pv-copy))
(declaim (inline pv-copy))
(defun pv-copy (pv)
  "Return a newly allocated copy of PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-copy-into (pvec) pv))

(declaim (ftype (function (pvec) pvec) pv-clear-into))
(declaim (inline pv-clear-into))
(defun pv-clear-into (pv)
  "Zero each element of PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (psetf px 0.0d0
           py 0.0d0
           pz 0.0d0))
  pv)

(declaim (ftype (function (pvec) pvec) pv-clear))
(defun pv-clear (pv)
  "Allocate a vector with all elements being 0d0 and return it. Ignore PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (declare (ignorable pv))
  (pvec))

(declaim (ftype (function (pvec double-float double-float double-float) pvec)
                pv-set-into))
(declaim (inline pv-set-into))
(defun pv-set-into (pv x y z)
  "Assign X Y Z into PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (psetf px (as-double-float x)
           py (as-double-float y)
           pz (as-double-float z)))
  pv)

(declaim (ftype (function (double-float double-float double-float) pvec)
                pv-set))
(defun pv-set (x y z)
  "Allocate and return a new pvec and assign its :x :y and :z elements
with X Y and Z."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pvec x y z))

(defun pv-get (pv &key (multiple-value t))
  "Return either VALUES of the x, y, and z elements, or as a list,
depending on the value of the keyword argument :multiple-value, which
defaults to T."
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

(declaim (ftype (function (pvec) pvec) pv-stabilize-into))
(declaim (inline pv-stabilize-into))
(defun pv-stabilize-into (pv)
  "If any element is < than *pvec-tol*, force it to 0.0d0."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (macrolet ((stabilize (slot)
                 `(when (< (as-double-float (abs ,slot))
                           (as-double-float *pvec-tol*))
                    (setf ,slot 0.0d0))))
      (stabilize px)
      (stabilize py)
      (stabilize pz)))
  pv)

(declaim (ftype (function (pvec) pvec) pv-stabilize))
(defun pv-stabilize (pv)
  "Allocate a new pvec and return the stabilized PV in it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-stabilize-into (pv-copy pv)))

(declaim (ftype (function (pvec) pvec) pv-negate-into))
(defun pv-negate-into (pv)
  "Negate the vetor and store back into PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (psetf px (as-double-float (- px))
           py (as-double-float (- py))
           pz (as-double-float (- pz))))
  pv)

(declaim (ftype (function (pvec) pvec) pv-negate))
(defun pv-negate (pv)
  "Store the negation of PV into a newly created pvec and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-negate-into (pv-copy pv)))

(declaim (ftype (function (pvec) double-float) pv-mag))
(declaim (inline pv-mag))
(defun pv-mag (pv)
  "Compute the double-float euclidean magnitude of PV and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  ;; And remove the compiler note about the boxed double-float I'm trying
  ;; to return.
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-pvec-accessors (p pv)
    (the double-float
         (sqrt (as-double-float
                (+ (* px px)
                   (* py py)
                   (* pz pz)))))))

(declaim (ftype (function (pvec) pvec) pv-normalize-into))
(defun pv-normalize-into (pv)
  "Normalize PV in a side effecting manner and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (let ((mag (pv-mag pv)))
    (with-pvec-accessors (p pv)
      (psetf px (as-double-float (/ px mag))
             py (as-double-float (/ py mag))
             pz (as-double-float (/ pz mag))))
    pv))

(declaim (ftype (function (pvec) pvec) pv-normalize))
(defun pv-normalize (pv)
  "Allocate a new pvec that contains the normalization of PV and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-normalize-into (pv-copy pv)))

(declaim (ftype (function (pvec) t) pv-zero-p))
(defun pv-zero-p (pv)
  "If all elements are < *pvec-tol* return NIL, otherwise PV."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  ;; Don't modify what was passed in!
  (let* ((is-zero-p (and (< (the double-float
                                 (abs (pvec-x pv)))
                            (the double-float *pvec-tol*))
                         (< (the double-float
                                 (abs (pvec-y pv)))
                            (the double-float *pvec-tol*))
                         (< (the double-float
                                 (abs (pvec-z pv)))
                            (the double-float *pvec-tol*)))))
    (if is-zero-p
        pv
        nil)))

(declaim (inline pv-cross-into))
(defun pv-cross-into (pvn pvu pvv)
  "Low level right-handed cross product of PVU and PVV and store into PVN"
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((n pvn) (u pvu) (v pvv))
    (psetf nx (as-double-float (- (* uy vz) (* uz vy)))
           ny (as-double-float (- (* uz vx) (* ux vz)))
           nz (as-double-float (- (* ux vy) (* uy vx)))))
  pvn)

(declaim (ftype (function (pvec pvec) pvec) pv-cross))
(defun pv-cross (pvu pvv)
  "Do a right handed cross product between PVU and PVB and return a
new pvec of it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-cross-into (pvec) pvu pvv))

(declaim (ftype (function (pvec pvec pvec) pvec) pv-vector-into))
(declaim (inline pv-vector-into))
(defun pv-vector-into (pvd pva pvb)
  "Low level vector operation from point PVA to point PVB and stored into PVD."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((r pvd) (a pva) (b pvb))
    (psetf rx (as-double-float (- bx ax))
           ry (as-double-float (- by ay))
           rz (as-double-float (- bz az))))
  pvd)

(declaim (ftype (function (pvec pvec) pvec) pv-vector))
(defun pv-vector (pva pvb)
  "Compute a vector from the point represented in PVA to the point
represented in PVB and return the new pvec vector."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (let ((result (pvec)))
    (pv-vector-into result pva pvb)
    (pv-stabilize-into result)))

(declaim (ftype (function (pvec pvec pvec) pvec) pc-add-into))
(declaim (inline pv-add-into))
(defun pv-add-into (pvd pva pvb)
  "Low level add of PVA and PVB and store into PVD."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((r pvd) (a pva) (b pvb))
    (psetf rx (as-double-float (+ ax bx))
           ry (as-double-float (+ ay by))
           rz (as-double-float (+ az bz))))
  pvd)

(declaim (ftype (function (pvec pvec) pvec) pv-add))
(defun pv-add (pva pvb)
  "Add two vectors and return a new pvec with the result."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-add-into (pvec) pva pvb))

(declaim (ftype (function (pvec pvec pvec) pvec) pv-sub-into))
(declaim (inline pv-sub-into))
(defun pv-sub-into (pvd pva pvb)
  "Low level subtraction of PVB from PVA and store into PVD."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((r pvd) (a pva) (b pvb))
    (psetf rx (as-double-float (- ax bx))
           ry (as-double-float (- ay by))
           rz (as-double-float (- az bz))))
  pvd)

(declaim (ftype (function (pvec pvec) pvec) pv-sub))
(defun pv-sub (pva pvb)
  "Subtract PVB from PVA and return result in a new pvec."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-sub-into (pvec) pva pvb))

(declaim (ftype (function (pvec double-float) pvec) pv-scale-into))
(declaim (inline pv-scale-into))
(defun pv-scale-into (pv scale-factor)
  "In place scale of PV by the DOUBLE-FLOAT SCALE-FACTOR"
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p pv)
    (psetf px (as-double-float (* px scale-factor))
           py (as-double-float (* py scale-factor))
           pz (as-double-float (* pz scale-factor)))
    pv))

(declaim (ftype (function (pvec double-float) pvec) pv-scale))
(defun pv-scale (pv scale-factor)
  "Allocate and return a new pvec that contains PV scaled by SCALE-FACTOR."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (pv-scale-into (pv-copy pv) scale-factor))

(declaim (ftype (function (pvec pvec) double-float) pv-dot))
(declaim (inline pv-dot))
(defun pv-dot (pva pvb)
  "Compute the dot product between the two vectors and return it."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-multiple-pvec-accessors ((a pva) (b pvb))
    (as-double-float
     (+ (* ax bx)
        (* ay by)
        (* az bz)))))

(declaim (ftype (function (pvec pvec) double-float) pv-angle))
(defun pv-angle (pva pvb)
  "Compute and return the angle in radians between the two vectors."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (when (or (pv-zero-p pva) (pv-zero-p pvb))
    ;; Avoid a divide by zero, assume they are coincident.
    (return-from pv-angle 0.0d0))

  (let ((dot (pv-dot pva pvb))
        (a-mag (pv-mag pva))
        (b-mag (pv-mag pvb)))
    (as-double-float (acos (/ dot (* a-mag b-mag))))))


(declaim (ftype (function (pvec pvec &key (:sqrt t))
                          (values double-float double-float))
                pv-dist))
(defun pv-dist (pva pvb &key (sqrt t))
  "Assume PVA and PVB are points in 3 dimensions and compute the distance
between them. If :SQRT is T, the default, then return the euclidean distance
as the first value and the non-normalized distance as the second. If :SQRT is
NIL, then do not compute the euclidean distance and instead return the
non-normalized distance for both values."
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

(declaim (ftype (function (pvec &key (:span keyword)) pvec)
                pv-rand-dir-into))
(defun pv-rand-dir-into (pv &key (span :xy))
  "Into PV place a normalized vector which is randomly oriented in
the vector space defined by the :SPAN keyword. :SPAN may be one of:
:X, :Y, :Z, :XZ, :XY, :YZ, :XYZ."
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
       (pv-set-into pv (rand-one-dim) 0d0 0d0))
      ((:y)
       (pv-set-into pv 0d0 (rand-one-dim) 0d0))
      ((:z)
       (pv-set-into pv 0d0 0d0 (rand-one-dim)))
      ((:xy)
       (multiple-value-bind (x y) (rand-two-dim)
         (pv-set-into pv x y 0d0)))
      ((:xz)
       (multiple-value-bind (x z) (rand-two-dim)
         (pv-set-into pv x 0d0 z)))
      ((:yz)
       (multiple-value-bind (y z) (rand-two-dim)
         (pv-set-into pv 0d0 y z)))
      ((:xyz)
       (multiple-value-bind (x y z) (rand-three-dim)
         (pv-set-into pv x y z))))

    ;; already normalized by virtue of construction.
    pv))

(declaim (ftype (function (&key (:span keyword)) pvec)
                pv-rand-dir))
(defun pv-rand-dir (&key (span :xy))
  "Compute and return a new pvec with a randomized oriented and
  normalized vector in the specified SPAN."
  (pv-rand-dir-into (pvec) :span span))
