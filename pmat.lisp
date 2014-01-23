(in-package :option-9)

#-option-9-optimize-pmat
(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype pmat () `(simple-array double-float (16)))
  (defstruct (pmat
               (:type (vector double-float))
               (:constructor make-pmat)
               (:constructor pmat (&optional m00 m01 m02 m03
                                             m10 m11 m12 m13
                                             m20 m21 m22 m23
                                             m30 m31 m32 m33)))
    (m00 0.0d0 :type double-float)
    (m01 0.0d0 :type double-float)
    (m02 0.0d0 :type double-float)
    (m03 0.0d0 :type double-float)
    (m10 0.0d0 :type double-float)
    (m11 0.0d0 :type double-float)
    (m12 0.0d0 :type double-float)
    (m13 0.0d0 :type double-float)
    (m20 0.0d0 :type double-float)
    (m21 0.0d0 :type double-float)
    (m22 0.0d0 :type double-float)
    (m23 0.0d0 :type double-float)
    (m30 0.0d0 :type double-float)
    (m31 0.0d0 :type double-float)
    (m32 0.0d0 :type double-float)
    (m33 0.0d0 :type double-float))

  (defmacro with-pmat-accessors ((prefix-symbol pmat) &body body)
    `(with-accessors ((,(alexandria:symbolicate prefix-symbol "00") pmat-m00)
                      (,(alexandria:symbolicate prefix-symbol "01") pmat-m01)
                      (,(alexandria:symbolicate prefix-symbol "02") pmat-m02)
                      (,(alexandria:symbolicate prefix-symbol "03") pmat-m03)
                      (,(alexandria:symbolicate prefix-symbol "10") pmat-m10)
                      (,(alexandria:symbolicate prefix-symbol "11") pmat-m11)
                      (,(alexandria:symbolicate prefix-symbol "12") pmat-m12)
                      (,(alexandria:symbolicate prefix-symbol "13") pmat-m13)
                      (,(alexandria:symbolicate prefix-symbol "20") pmat-m20)
                      (,(alexandria:symbolicate prefix-symbol "21") pmat-m21)
                      (,(alexandria:symbolicate prefix-symbol "22") pmat-m22)
                      (,(alexandria:symbolicate prefix-symbol "23") pmat-m23)
                      (,(alexandria:symbolicate prefix-symbol "30") pmat-m30)
                      (,(alexandria:symbolicate prefix-symbol "31") pmat-m31)
                      (,(alexandria:symbolicate prefix-symbol "32") pmat-m32)
                      (,(alexandria:symbolicate prefix-symbol "33") pmat-m33))
         ,pmat
       ,@body))

  (defmacro with-multiple-pmat-accessors (sbinds &body body)
    (if (null sbinds)
        `(progn ,@body)
        `(with-pmat-accessors ,(car sbinds)
           (with-multiple-pmat-accessors ,(cdr sbinds) ,@body))))

  ;; NOTE: I must use the pprint-dispatch table to emit nicely
  ;; formated pmats because they aren't a CLASS due to the defstruct
  ;; definition I am using. So PRINT-OBJECT doesn't work on PMAT
  ;; types.
  (set-pprint-dispatch
   'pmat
   #'(lambda (str pobj)
       (with-pmat-accessors (m pobj)
         (print-unreadable-object (pobj str)
           (format
            str
            "[~A ~A ~A ~A]~%  [~A ~A ~A ~A]~%  [~A ~A ~A ~A]~%  [~A ~A ~A ~A]"
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33))))))

(defun pm-test ()
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((result (pmat)))
    (with-multiple-pmat-accessors ((r result))
      (psetf r00 1d0 r01 2d0 r02 3d0 r03 4d0
             r10 5d0 r11 6d0 r12 7d0 r13 8d0
             r20 9d0 r21 10d0 r22 11d0 r23 12d0
             r30 13d0 r31 14d0 r32 15d0 r33 16d0))
    result))

(declaim (ftype (function (pmat) pmat) pm-eye-into))
(defun pm-eye-into (mat)
  "Fill the matrix MAT with the identity matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (psetf m00 1.0d0 m01 0.0d0 m02 0.0d0 m03 0.0d0
           m10 0.0d0 m11 1.0d0 m12 0.0d0 m13 0.0d0
           m20 0.0d0 m21 0.0d0 m22 1.0d0 m23 0.0d0
           m30 0.0d0 m31 0.0d0 m32 0.0d0 m33 1.0d0))
  mat)

(declaim (ftype (function () pmat) pm-eye))
(defun pm-eye ()
  "Return a newly allocated identity matrix."
  (pm-eye-into (pmat)))

(defun junk ()
  (declare (optimize (safety 0)))
  (let ((f (make-array 4 :element-type 'double-float
                       :initial-contents '(0d0 0d0 0d0 0d0))))
    (declare ((simple-array double-float (4)) f))
    (psetf (aref f 0) 1d0 (aref f 1) 1d0 (aref f 2) 1d0 (aref f 3) 1d0)))


;; NOTE: This looks scary in that I might be modifying RESULT while
;; still reading from it. Not so. The semantics of PSETF are that all
;; subforms are evaluated and then the assignments happen in any
;; order. So, no assignments can happen before the subforms are
;; finished being evaluated. Hence, I don't have to worry if result EQ
;; mat0 or mat1. It is the same for other uses of PSETF in this file.
(declaim (ftype (function (pmat pmat pmat) pmat) pm-mul-into))
(defun pm-mul-into (result mat0 mat1)
  "Perform matrix multiply of MAT0 * MAT1 and store into RESULT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((r result) (a mat0) (b mat1))
    (psetf r00 (+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30))
           r10 (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30))
           r20 (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30))
           r30 (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30))

           r01 (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31))
           r11 (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31))
           r21 (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31))
           r31 (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31))

           r02 (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32))
           r12 (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32))
           r22 (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32))
           r32 (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32))

           r03 (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33))
           r13 (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33))
           r23 (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33))
           r33 (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33))))
  result)

(declaim (ftype (function (pmat pmat) pmat) pm-mul))
(defun pm-mul (mat0 mat1)
  "Perform matrix multiply of MAT0 * MAT1 and return new pmat of result."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-mul-into (pmat) mat0 mat1))

(declaim (ftype (function (pmat pmat) pmat) pm-copy-into))
(defun pm-copy-into (result src)
  "Copy SRC into RESULT and return RESULT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((r result) (s src))
    (psetf r00 s00 r01 s01 r02 s02 r03 s03
           r10 s10 r11 s11 r12 s12 r13 s13
           r20 s20 r21 s21 r22 s22 r23 s23
           r30 s30 r31 s31 r32 s32 r33 s33))
  result)

(declaim (ftype (function (pmat) pmat) pm-copy))
(defun pm-copy (mat)
  "Return a newly allocated pmat into which MAT was copied."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-copy-into (pmat) mat))

(declaim (ftype (function (pmat) pmat) pm-transpose))
(declaim (inline pm-transpose))
(defun pm-transpose (mat)
  "Transpose the MAT in place and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (rotatef m10 m01)
    (rotatef m20 m02)
    (rotatef m30 m03)
    (rotatef m21 m12)
    (rotatef m31 m13)
    (rotatef m32 m23))
  mat)

(declaim (ftype (function (pmat) pmat) pm-invert-rot-into))
(declaim (inline om-invert-rot-into))
(defun pm-invert-rot-into (rot)
  "Invert the rotation matrix ROT and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (r rot)
    ;; Transpose the upper left square 3x3 portion of the rotation matrix
    (rotatef r10 r01)
    (rotatef r20 r02)
    (rotatef r21 r12)
    ;; Negate the translation
    (psetf r03 (as-double-float (* r03 -1d0))
           r13 (as-double-float (* r13 -1d0))
           r23 (as-double-float (* r23 -1d0))))
  rot)

(declaim (ftype (function (pmat) pmat) pm-invert-rot))
(defun pm-invert-rot (rot)
  "Allocate a new matrix that will be the inversion of the ROT matrix and
return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-invert-rot-into (pm-copy rot)))

(declaim (inline pm-convert-to-opengl-into))
(defun pm-convert-to-opengl-into (ogl mat)
  "Convert the MAT matrix into OGL, which is a column-major OpenGL
matrix represented as a (simple-array souble-float (16)), and then
return OGL."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare ((simple-array double-float (16)) ogl))
  (with-pmat-accessors (m mat)
    (psetf (aref ogl 0) m00
           (aref ogl 1) m10
           (aref ogl 2) m20
           (aref ogl 3) m30

           (aref ogl 4) m01
           (aref ogl 5) m11
           (aref ogl 6) m21
           (aref ogl 7) m31

           (aref ogl 8) m02
           (aref ogl 9) m12
           (aref ogl 10) m22
           (aref ogl 11) m32

           (aref ogl 12) m03
           (aref ogl 13) m13
           (aref ogl 14) m23
           (aref ogl 15) m33))
  ogl)

(defun pm-convert-to-opengl (mat)
  "Convert the MAT matrix into newly allocated column-major ordered
simple-array double-float (16) suitable for OpenGL and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((ogl (make-array 16 :element-type 'double-float
                         :initial-element 0d0)))
    (declare ((simple-array double-float (16)) ogl))
    (pm-convert-to-opengl-into ogl mat)
    ogl))

(defun pm-convert-from-opengl-into (mat ogl)
  "Convert the OGL OpenGL matrix, which is a (simple-arry
double-float (16)), into the MAT format and return MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare ((simple-array double-float (16)) ogl))
  (with-pmat-accessors (m mat)
    (psetf m00 (aref ogl 0)
           m10 (aref ogl 1)
           m20 (aref ogl 2)
           m30 (aref ogl 3)

           m01 (aref ogl 4)
           m11 (aref ogl 5)
           m21 (aref ogl 6)
           m31 (aref ogl 7)

           m02 (aref ogl 8)
           m12 (aref ogl 9)
           m22 (aref ogl 10)
           m32 (aref ogl 11)

           m03 (aref ogl 12)
           m13 (aref ogl 13)
           m23 (aref ogl 14)
           m33 (aref ogl 15)))
  mat)

(defun pm-convert-from-opengl (ogl)
  "Convert the OGL OpenGL matrix into a newly allocated MAT format and
return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare ((simple-array double-float (16)) ogl))
  (pm-convert-from-opengl-into (pmat) ogl))

(declaim (ftype (function (pmat pvec) pmat) pm-set-trans-into))
(declaim (inline pm-set-trans-into))
(defun pm-set-trans-into (mat trans)
  "Set the translation vector in the pvec TRANS into MAT and return MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (v trans)
      (psetf m03 vx
             m13 vy
             m23 vz)))
  mat)

(declaim (ftype (function (pvec) pmat) pm-set-trans))
(defun pm-set-trans (trans)
  "Allocate and return a new identity matrix with TRANS as its translation."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-set-trans-into (pm-eye) trans))


(declaim (ftype (function (pvec pmat) pvec) pm-get-trans-into))
(defun pm-get-trans-into (trans mat)
  "Get the translation column from MAT and put into the pvec TRANS. Return
TRANS."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (v trans)
      (psetf vx m03
             vy m13
             vz m23)))
  trans)

(declaim (ftype (function (pmat) pvec) pm-get-trans))
(defun pm-get-trans (mat)
  "Return the translation column of the MAT as a newly allocated PVEC and
return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-get-trans-into (pvec) mat))

(defun pm-get-raw-axes-into (xdir ydir zdir mat &key (multiple-value t))
  "Retrieve the raw direction vectors into XDIR, YDIR, and ZDIR, from MAT.
If the keyword :MULTIPLE-VALUE is T (the default), then return values
of XDIR, YDIR, and ZDIR. Otherwise return a list of them in the same
ordering."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-multiple-pvec-accessors ((x xdir) (y ydir) (z zdir))
      (psetf xx m00 xy m10 xz m20
             yx m01 yy m11 yz m21
             zx m02 zy m12 zz m22)))
  (if multiple-value
      (values xdir ydir zdir)
      (list xdir ydir zdir)))

(defun pm-get-raw-axes (mat &key (multiple-value t))
  "Allocate new direction vectors for the X, Y and Z directions from MAT
and return them as VALUES when :MULTIPLE-VALUE is T (the default). Otherwise
return them as a list in the same order."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-get-raw-axes-into (pvec) (pvec) (pvec)
                        mat :multiple-value multiple-value))

(declaim (ftype (function (pmat pvec pvec pvec) pmat) pm-set-raw-axes-into))
(defun pm-set-raw-axes-into (mat xdir ydir zdir)
  "Assign the supplied direction vectors XDIR YDIR ZDIR into MAT and
return MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-multiple-pvec-accessors ((x xdir) (y ydir) (z zdir))
      (psetf m00 xx
             m10 xy
             m20 xz

             m01 yx
             m11 yy
             m21 yz

             m02 zx
             m12 zy
             m22 zz)))
  mat)

(declaim (ftype (function (pvec pvec pvec) pmat) pm-set-raw-axes))
(defun pm-set-raw-axes (xdir ydir zdir)
  "Allocate an identity matrix and then set the raw axes supplied as the
rotation components in the matrix. Return the matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-set-raw-axes-into (pmat) xdir ydir zdir))

(declaim (ftype (function (pmat pvec) pmat) pm-fly-into))
(defun pm-fly-into (mat fly-vec)
  "Add each component of the FLY-VEC to the translation portion of the
basis MAT in the direction of the respective basis direction and write
back into MAT. The effect is to translate the basis in the direction
of the fly-vec. Return MAT."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (f fly-vec)
    (with-pmat-accessors (m mat)
      (when (> (as-double-float (abs fx))
               (as-double-float *pvec-tol*))
        (psetf m03 (as-double-float (+ m03 (* m00 fx)))
               m13 (as-double-float (+ m13 (* m10 fx)))
               m23 (as-double-float (+ m23 (* m20 fx)))))
      (when (> (as-double-float (abs fy))
               (as-double-float *pvec-tol*))
        (psetf m03 (as-double-float (+ m03 (* m01 fy)))
               m13 (as-double-float (+ m13 (* m11 fy)))
               m23 (as-double-float (+ m23 (* m21 fy)))))
      (when (> (as-double-float (abs fz))
               (as-double-float *pvec-tol*))
        (psetf m03 (as-double-float (+ m03 (* m02 fz)))
               m13 (as-double-float (+ m13 (* m12 fz)))
               m23 (as-double-float (+ m23 (* m22 fz)))))))
  mat)

(declaim (ftype (function (pmat pvec) pmat) pm-fly))
(defun pm-fly (mat fly-vec)
  "Return a newly allocated matrix in which FLY-VEC has been applied to MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-fly-into (pm-copy mat) fly-vec))

(declaim (ftype (function (pmat pvec) pmat) pm-translate-into))
(defun pm-translate-into (pmat pvec)
  "Add PVEC to the translation column in PMAT. Return PMAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (p pmat)
    (with-pvec-accessors (v pvec)
      (psetf p03 (as-double-float (+ p03 vx))
             p13 (as-double-float (+ p13 vy))
             p23 (as-double-float (+ p23 vz)))))
  pmat)

(declaim (ftype (function (pmat pvec) pmat) pm-translate))
(defun pm-translate (pmat pvec)
  "Return a newly allocated matrix that has PVEC added to the translation
column in PMAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-translate-into (pm-copy pmat) pvec))

(declaim (ftype (function (pvec pmat keyword) pvec) pm-get-dir-into))
(defun pm-get-dir-into (pv mat dir)
  "Select the DIR (one of :X, :Y, or :Z) direction vector from the
rotation matrix MAT and store into PV. Return PV."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (p pv)
      (psetf px (ecase dir ((:x) m00) ((:y) m01) ((:z) m02))
             py (ecase dir ((:x) m10) ((:y) m11) ((:z) m12))
             pz (ecase dir ((:x) m20) ((:y) m21) ((:z) m22)))))
  pv)

(declaim (ftype (function (pmat keyword) pvec) pm-get-dir))
(defun pm-get-dir (mat dir)
  "Allocate and return a new pvec into which is stored the DIR (one
of :X, :Y, or :Z) direction vector from the rotation matrix MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-get-dir-into (pvec) mat dir))

(declaim (ftype (function (pvec pmat pvec) pvec) pm-apply-into))
(defun pm-apply-into (transformed-point basis point)
  "Multiply the BASIS against the POINT and store the result into
TRANSFORMED-POINT which is then returned."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((tp transformed-point) (p point))
    (with-pmat-accessors (b basis)
      (psetf tpx (+ (* b00 px) (* b01 py) (* b02 pz) (* b03 1d0))
             tpy (+ (* b10 px) (* b11 py) (* b12 pz) (* b13 1d0))
             tpz (+ (* b20 px) (* b21 py) (* b22 pz) (* b23 1d0)))))
  transformed-point)

(declaim (ftype (function (pmat pvec) pvec) pm-apply))
(defun pm-apply (basis point)
  "Multiply the BASIS against the POINT and return the transformed
point in a newly allocated pvec."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-apply-into (pvec) basis point))

(declaim (ftype (function (pmat pmat) pmat) pm-copy-rotation-into))
(defun pm-copy-rotation-into (dst src)
  "Copy just the 3x3 rotation vectors of the SRC pmat into the DST
pmat then return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (s src))
    (psetf d00 s00 d01 s01 d02 s02
           d10 s10 d11 s11 d12 s12
           d20 s20 d21 s21 d22 s22))
  dst)

(declaim (ftype (function (pmat) pmat) pm-copy-rotation))
(defun pm-copy-rotation (src)
  "Allocate and return an identity matrix into which just the 3X3 rotation
components of the SRC matrix have been stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-copy-rotation-into (pm-eye) src))

(declaim (ftype (function (pmat) pmat) pm-stabilize-into))
(defun pm-stabilize-into (mat)
  "Check each element in the MAT and if it is less than *pvec-tol* force it
in place to zero. Return the MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    ;; This macro isn't entirely lexically/once-only safe, so don't abuse it.
    (macrolet ((stabilize (place)
                 `(when (< (as-double-float (abs ,place))
                           (as-double-float *pvec-tol*))
                    (setf ,place 0d0))))
      (stabilize m00)
      (stabilize m01)
      (stabilize m02)
      (stabilize m03)
      (stabilize m10)
      (stabilize m11)
      (stabilize m12)
      (stabilize m13)
      (stabilize m20)
      (stabilize m21)
      (stabilize m22)
      (stabilize m23)
      (stabilize m30)
      (stabilize m31)
      (stabilize m32)
      (stabilize m33)))
  mat)

(declaim (ftype (function (pmat) pmat) pm-stabilize))
(defun pm-stabilize (mat)
  "Return a newly allocated matrix that contains the stabilized values
from MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-stabilize-into (pm-copy mat)))

(declaim (ftype (function (pmat pvec &key (:stabilize t)) pmat)
                pm-rotate-basis-into))
(defun pm-rotate-basis-into (basis rotation-vec &key (stabilize t))
  "In place rotate the basis matrix as a relative rotation (AKA the
local axis rotation) as specified by the ROTATION-VEC. The
ROTATION-VEC defines the relative rotation for each axis. The rotation
is stabilized if keyword argument :STABILIZED is T (the default) but
not re-orthogonalized. Return BASIS as the result."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((result (pmat))
        (rot (pmat)))
    (with-pmat-accessors (rt rot)
      (with-pvec-accessors (rv rotation-vec)

        ;; We overwrite these values for each rotation in such a
        ;; manner as to leave everything consistent with mostly
        ;; minimal stores no matter what rotations may happen.
        (pm-eye-into rot)

        ;; This macro is not hygienic and deeply intertwined lexically
        ;; in this code. But it makes things clearer in the flow. I've
        ;; unrolled this loop, which is why it is a macro expansion
        ;; and not a labels.
        (macrolet ((do-a-rotation (axis sin-sym cos-sym &body body)
                     `(when (> (as-double-float (abs ,axis))
                               (as-double-float *pvec-tol*))
                        (let ((,sin-sym (sin ,axis))
                              (,cos-sym (cos ,axis)))
                          ,@body)
                        (pm-mul-into result basis rot)
                        (pm-copy-rotation-into basis result))))

          ;; These rotations must be done in this order.  Also, this
          ;; is a right handed system.
          (do-a-rotation rvz sval cval
                         (psetf rt00 (as-double-float cval)
                                rt10 (as-double-float sval)
                                rt01 (as-double-float (- sval))
                                rt11 (as-double-float cval)))

          (do-a-rotation rvx sval cval
                         (psetf rt00 1d0
                                rt10 0d0
                                rt20 0d0
                                rt01 0d0
                                rt11 (as-double-float cval)
                                rt21 (as-double-float sval)
                                rt02 0d0
                                rt12 (as-double-float (- sval))
                                rt22 (as-double-float cval)))

          (do-a-rotation rvy sval cval
                         (psetf rt00 (as-double-float cval)
                                rt10 0d0
                                rt20 (as-double-float (- sval))
                                rt01 0d0
                                rt11 1d0
                                rt21 0d0
                                rt02 (as-double-float sval)
                                rt12 0d0
                                rt22 (as-double-float cval)))))))
  (if stabilize
      (pm-stabilize-into basis)
      basis))

(declaim (ftype (function (pmat pvec &key (:stabilize t)) pmat)
                pm-rotate-basis))
(defun pm-rotate-basis (basis rotation-vec &key (stabilize t))
  "Return a newly allocated matrix which is the basis with the
relative rotations in ROTATION-VEC applied to it. If the keyword
argument :STABILIZE is T (the default), the returned matrix is
stabilized, but not re-orthogonalized."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-rotate-basis-into (pm-copy basis) rotation-vec :stabilize stabilize))

(declaim (ftype (function (pmat pmat keyword keyword) pmat)
                pm-create-view-into))
(defun pm-create-view-into (camera basis at-dir up-dir)
  "Camera is the basis into which the computed view matrix will be
stored. AT-DIR represents the :x, :y, or :z axis in the basis that
will be used at the 'look at' direction. UP-DIR represents the :x, :y,
or :z axis that will be used as the 'up' direction in the basis."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (xv yv zv) (pm-get-raw-axes basis)
    (let* ((nn (ecase at-dir ((:x) xv) ((:y) yv) ((:z) zv)))
           (vv (ecase up-dir ((:x) xv) ((:y) yv) ((:z) zv)))
           ;; Create the u v n of the new view matrix
           (n (pv-normalize nn))
           (u (pv-normalize-into (pv-cross vv nn)))
           (v (pv-cross n u))
           ;; create an inverted camera rotation
           (tmp-rot (pm-invert-rot-into (pm-set-raw-axes u v n)))
           ;; create an inverted translation matrix
           (tmp-trans (pm-set-trans (pv-negate-into (pm-get-trans basis)))))
      ;; Create the actual inverted view matrix.
      (pm-mul-into camera tmp-rot tmp-trans))))

(declaim (ftype (function (pmat keyword keyword) pmat) pm-create-view))
(defun pm-create-view (basis at-dir up-dir)
  "Allocate and return a camera basis into which the inverted view of
the BASIS is stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-create-view-into (pm-eye) basis at-dir up-dir))

;; XXX This might be wrong, check very carefully.
(defun pm-plane-eqn (basis plane &key (multiple-value :t))
  "Compute the double-float A,B,C,D coeffs for the plane specified in
PLANE, one of :XY, :XZ, or :YZ, and which passes through the
translation point in the basis and return them in that order as
multiple values if the keyword :MULTIPLE-VALUE is T (the default) or
as a list."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-pmat-accessors (b basis)
    (let* ((p0 (pm-get-trans basis))
           ;; All axes are chosen such that their cross leads to a
           ;; vector pointing in the positive direction of the third
           ;; axis.
           (choice-a (ecase plane ((:xy) :x) ((:xz) :z) ((:yz) :y)))
           (choice-b (ecase plane ((:xy) :y) ((:xz) :x) ((:yz) :z)))
           (axis-a (pm-get-dir basis choice-a))
           (axis-b (pm-get-dir basis choice-b))
           ;; These are actual points.
           (p1 (pv-add p0 axis-a))
           (p2 (pv-add p0 axis-b))
           ;; Compute the vector we'll be crossing.
           (vec-a (pv-vector p0 p1))
           (vec-b (pv-vector p0 p2))
           (norm (pv-stabilize-into (pv-cross vec-a vec-b)))
           (d (pv-dot norm p0)))
      (with-pvec-accessors (n norm)
        (if multiple-value
            (values nx ny nz d)
            (list nx ny nz d))))))

;; TODO, spruce this up to emit quadrants too, eventually.
(declaim (ftype (function (pmat pvec keyword) cons) pm-classify-point))
(defun pm-classify-point (basis point plane)
  "Classify the POINT, represented as a pvec, with respect to the specified
PLANE as defined by the BASIS and its translation. Valid PLANEs are :XY,
:XZ, and :YZ. The returned result is a list of keywords indicating the
type of classification.

The return values with respect to the basis location and orientation:
 (:front) means the point is in the direction of the positive Z axis when :XY.
 (:back) means the point is in the direction of the negative Z axis when :XY.
 (:left) means the point is in the direction of the positive X axis when :YZ.
 (:right) means the point is in the direction of the negative X axis when :YZ.
 (:above) means the point is in the direction of the positive Y axis when :XZ.
 (:below) means the point is in the direction of the negative Y axis when :XZ.
 (:coincident :front-back) means the point is on the XY plane when :XY.
 (:coincident :above-below) means the point is on the XZ plane when :XZ.
 (:coincident :left-right) means the point is on the YZ plane when :YZ."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p point)
    (multiple-value-bind (a b c d) (pm-plane-eqn basis plane)
      (let ((side (as-double-float (- (+ (* a px) (* b py) (* c pz)) d))))
        ;; "FRONT" is considered down the positive Z axis when :XY
        ;; "LEFT" is considered down the positive X axis when :YZ
        ;; "ABOVE" is considered down the positive Y axis when :XZ
        (cond
          ((< (the double-float (abs side))
              (as-double-float *pvec-tol*))
           (ecase plane
             ((:xy)
              (list :coincident :front-back))
             ((:xz)
              (list :coincident :above-below))
             ((:yz)
              (list :coincident :left-right))))
          ((< side 0d0)
           (ecase plane
             ((:xy)
              (list :back))
             ((:xz)
              (list :below))
             ((:yz)
              (list :right))))
          (t
           (ecase plane
             ((:xy)
              (list :front))
             ((:xz)
              (list :above))
             ((:yz)
              (list :left)))))))))










