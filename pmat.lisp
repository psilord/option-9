(in-package :option-9)

;; This matrix library contains function to operate on both generic 4x4
;; matricies and also "transformation matricies" which are 3x3
;; rotation matricies embedded into the upper left hand corner of a 4x
;; matrix incuding a 4x1 matrix representing the translation in the
;; 4th column of the 4x4 matrix.

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pm-test-into (mat)
  "Construct a test pattern into the 4x4 matrix MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((m mat))
    (psetf m00 1d0 m01 2d0 m02 3d0 m03 4d0
           m10 5d0 m11 6d0 m12 7d0 m13 8d0
           m20 9d0 m21 10d0 m22 11d0 m23 12d0
           m30 13d0 m31 14d0 m32 15d0 m33 16d0))
  mat)

(defun pm-test ()
  "Return a newly allocated 4x4 matrix that contains a test pattern."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((result (pmat)))
    (pm-test-into result)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat) pmat) pm-transpose-into))
(declaim (inline pm-transpose-into))
(defun pm-transpose-into (mat)
  "Transpose the 4x4 MAT in place and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (rotatef m10 m01)
    (rotatef m20 m02)
    (rotatef m30 m03)
    (rotatef m21 m12)
    (rotatef m31 m13)
    (rotatef m32 m23))
  mat)

(declaim (ftype (function (pmat) pmat) pm-transpose))
(declaim (inline pm-transpose))
(defun pm-transpose (mat)
  "Return a newly allocated 4x4 matrix which is the transpose of MAT"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-transpose-into (pm-copy mat)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (psetf r00 (as-double-float
                (+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30)))
           r10 (as-double-float
                (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30)))
           r20 (as-double-float
                (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30)))
           r30 (as-double-float
                (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30)))

           r01 (as-double-float
                (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31)))
           r11 (as-double-float
                (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31)))
           r21 (as-double-float
                (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31)))
           r31 (as-double-float
                (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31)))

           r02 (as-double-float
                (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32)))
           r12 (as-double-float
                (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32)))
           r22 (as-double-float
                (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32)))
           r32 (as-double-float
                (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32)))

           r03 (as-double-float
                (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33)))
           r13 (as-double-float
                (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33)))
           r23 (as-double-float
                (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33)))
           r33 (as-double-float
                (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33)))))
  result)

(declaim (ftype (function (pmat pmat) pmat) pm-mul))
(defun pm-mul (mat0 mat1)
  "Perform matrix multiply of MAT0 * MAT1 and return new pmat of result."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-mul-into (pmat) mat0 mat1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat double-float pvec &key (:stabilize t)) pmat)
                pm-trfm-rotate-around-into))
(defun pm-trfm-rotate-around-into (rotation angle axis &key (stabilize t))
  "Store a computed rotation matrix with a (0 0 0) translation vector
into the ROTATION transformation matrix that will rotate around the
vector AXIS by the specified ANGLE. This assumes a right handed
coordinate system. Similar to glRotate()."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((norm-axis (pv-normalize axis))
        (c (as-double-float (cos angle)))
        (s (as-double-float (sin angle))))
    #+option-9-optimize-pmat
    (declare (type double-float c s))
    (with-pvec-accessors (a norm-axis)
      (let* ((1-c (as-double-float (- 1d0 c)))
             (xs (as-double-float (* ax s)))
             (ys (as-double-float (* ay s)))
             (zs (as-double-float (* az s)))

             (xx (as-double-float (* ax ax)))
             (yy (as-double-float (* ay ay)))
             (zz (as-double-float (* az az)))

             (xy (as-double-float (* ax ay)))
             (xz (as-double-float (* ax az)))
             (yz (as-double-float (* ay az)))

             (xx*[1-c] (as-double-float (* xx 1-c)))
             (yy*[1-c] (as-double-float (* yy 1-c)))
             (zz*[1-c] (as-double-float (* zz 1-c)))
             (xy*[1-c] (as-double-float (* xy 1-c)))
             (xz*[1-c] (as-double-float (* xz 1-c)))
             (yz*[1-c] (as-double-float (* yz 1-c))))
        #+option-9-optimize-pmat
        (declare (type double-float 1-c xs ys zs xx yy zz xy xz yz
                       xx*[1-c] yy*[1-c] zz*[1-c] xy*[1-c] xz*[1-c] yz*[1-c]))
        (with-pmat-accessors (r rotation)
          (psetf r00 (as-double-float (+ xx*[1-c] c))
                 r10 (as-double-float (+ xy*[1-c] zs))
                 r20 (as-double-float (- xz*[1-c] ys))
                 r30 0d0

                 r01 (as-double-float (- xy*[1-c] zs))
                 r11 (as-double-float (+ yy*[1-c] c))
                 r21 (as-double-float (+ yz*[1-c] xs))
                 r31 0d0

                 r02 (as-double-float (+ xz*[1-c] ys))
                 r12 (as-double-float (- yz*[1-c] xs))
                 r22 (as-double-float (+ zz*[1-c] c))
                 r32 0d0

                 r03 0d0
                 r13 0d0
                 r23 0d0
                 r33 1d0)))))

  (if stabilize
      (pm-stabilize-into rotation)
      rotation))

(declaim (ftype (function (double-float pvec &key (:stabilize t)) pmat)
                pm-trfm-rotate-around))
(defun pm-trfm-rotate-around (angle axis &key (stabilize t))
  "Allocate and return a transformation matrix with a (0 0 0)
translation vector that will rotate around the vector AXIS by the
specified ANGLE. This assumes a right handed coordinate system."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-rotate-around-into (pm-eye) angle axis :stabilize stabilize))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat) pmat) pm-trfm-invert-into))
(declaim (inline pm-trfm-invert-into))
(defun pm-trfm-invert-into (mat)
  "Invert a transformation matrix. This means 1) transpose the 3x3
rotation matrix contained in the upper left hand of the transformation
matrix, 2) store the application of the inverted rotation to the
negation of the 4x1 translation column back into the 4th column of the
transformation matrix MAT. Return the modified MAT."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    ;; Transpose the upper left square 3x3 portion of the rotation matrix
    (rotatef m10 m01)
    (rotatef m20 m02)
    (rotatef m21 m12)

    ;; Invert the translation by applying the inverted rotation to the
    ;; negated translation

    (psetf m03 (as-double-float
                (+ (* m00 (- m03)) (* m01 (- m13)) (* m02 (- m23))))
           m13 (as-double-float
                (+ (* m10 (- m03)) (* m11 (- m13)) (* m12 (- m23))))
           m23 (as-double-float
                (+ (* m20 (- m03)) (* m21 (- m13)) (* m22 (- m23))))))

  (pm-stabilize mat))

(declaim (ftype (function (pmat) pmat) pm-trfm-invert))
(defun pm-trfm-invert (trfm)
  "Invert the transformation matrix TRFM."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-invert-into (pm-copy trfm)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function ((simple-array double-float (16)) pmat)
                          (simple-array double-float (16)))
                pm-convert-to-opengl-into))
(declaim (inline pm-convert-to-opengl-into))
(defun pm-convert-to-opengl-into (ogl mat)
  "Convert the MAT matrix into OGL, which is a column-major OpenGL
matrix represented as a (simple-array double-float (16)), and then
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

(declaim (ftype (function (pmat) (simple-array double-float (16)))
                pm-convert-to-opengl))
(defun pm-convert-to-opengl (mat)
  "Convert the MAT matrix into newly allocated column-major ordered
simple-array double-float (16) suitable for OpenGL and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((ogl (make-array 16 :element-type 'double-float
                         :initial-element 0d0)))
    (declare ((simple-array double-float (16)) ogl))
    (pm-convert-to-opengl-into ogl mat)
    ogl))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat (simple-array double-float (16))) pmat)
                pm-convert-from-opengl-into))
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

(declaim (ftype (function ((simple-array double-float (16))) pmat)
                pm-convert-from-opengl))
(defun pm-convert-from-opengl (ogl)
  "Convert the OGL OpenGL matrix into a newly allocated MAT format and
return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare ((simple-array double-float (16)) ogl))
  (pm-convert-from-opengl-into (pmat) ogl))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) pm-trfm-set-trans-into))
(declaim (inline pm-trfm-set-trans-into))
(defun pm-trfm-set-trans-into (trfm trans)
  "Set the translation vector in the pvec TRANS into the translation
column in the transformation matrix MAT and return MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m trfm)
    (with-pvec-accessors (v trans)
      (psetf m03 vx
             m13 vy
             m23 vz)))
  trfm)

(declaim (ftype (function (pvec) pmat) pm-trfm-set-trans))
(defun pm-trfm-set-trans (trans)
  "Allocate and return a new identity transformation matrix with TRANS
as its translation."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-trans-into (pm-eye) trans))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pmat) pvec) pm-trfm-get-trans-into))
(defun pm-trfm-get-trans-into (trans mat)
  "Get the translation column from the transformation matrix MAT and
put into the pvec TRANS. Return TRANS."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (v trans)
      (psetf vx m03
             vy m13
             vz m23)))
  trans)

(declaim (ftype (function (pmat) pvec) pm-trfm-get-trans))
(defun pm-trfm-get-trans (mat)
  "Return the translation column of the transformation matrix MAT as a
newly allocated PVEC and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-trans-into (pvec) mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun pm-trfm-get-raw-axes-into (xdir ydir zdir mat &key (multiple-value t))
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

(defun pm-trfm-get-raw-axes (mat &key (multiple-value t))
  "Allocate new direction vectors for the X, Y and Z directions from MAT
and return them as VALUES when :MULTIPLE-VALUE is T (the default). Otherwise
return them as a list in the same order."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-raw-axes-into (pvec) (pvec) (pvec)
                             mat :multiple-value multiple-value))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec pvec pvec) pmat)
                pm-trfm-set-raw-axes-into))
(defun pm-trfm-set-raw-axes-into (mat xdir ydir zdir)
  "Assign the supplied direction vectors XDIR YDIR ZDIR into the transformation MAT and return MAT."
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

(declaim (ftype (function (pvec pvec pvec) pmat) pm-trfm-set-raw-axes))
(defun pm-trfm-set-raw-axes (xdir ydir zdir)
  "Allocate an identity transformation matrix and then set the raw
axes supplied as XDIR YDIR ZDIR into the rotation components of the
matrix. Return the transformation matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-raw-axes-into (pmat) xdir ydir zdir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) pm-trfm-fly-into))
(defun pm-trfm-fly-into (mat fly-vec)
  "Add each component of the FLY-VEC to the translation portion of the
transformation matrix MAT in the direction of the respective basis
direction in the rotation submatrix and write back into MAT.  Return
MAT."
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

(declaim (ftype (function (pmat pvec) pmat) pm-trfm-fly))
(defun pm-trfm-fly (mat fly-vec)
  "Copy the transformation matrix MAT and add each component of the
FLY-VEC to the translation portion of the transformation matrix MAT in
the direction of the respective basis direction in the rotation
submatrix and write back into MAT. Return the copy."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-fly-into (pm-copy mat) fly-vec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) pm-trfm-displace-into))
(defun pm-trfm-displace-into (pmat pvec)
  "Add PVEC to the translation column in the transformation matrix PMAT.
Return PMAT. This is a translation in the intertial frame of the transformation
matrix PMAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (p pmat)
    (with-pvec-accessors (v pvec)
      (psetf p03 (as-double-float (+ p03 vx))
             p13 (as-double-float (+ p13 vy))
             p23 (as-double-float (+ p23 vz)))))
  pmat)

(declaim (ftype (function (pmat pvec) pmat) pm-trfm-displace))
(defun pm-trfm-displace (pmat pvec)
  "Return a copy of the transformation matrix PMAT with the translation stored
in PVEC added to the translation column. This is a translation in the intertial
frame of the transformation matrix PMAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-displace-into (pm-copy pmat) pvec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (ftype (function (pvec pmat keyword) pvec) pm-trfm-get-dir-into))
(defun pm-trfm-get-dir-into (pv mat dir)
  "Select the DIR (one of :X, :Y, or :Z) direction vector from the
rotation submatrix encoded into the transformation matrix MAT and
store into PV. Return PV."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (p pv)
      (psetf px (ecase dir ((:x) m00) ((:y) m01) ((:z) m02))
             py (ecase dir ((:x) m10) ((:y) m11) ((:z) m12))
             pz (ecase dir ((:x) m20) ((:y) m21) ((:z) m22)))))
  pv)

(declaim (ftype (function (pmat keyword) pvec) pm-trfm-get-dir))
(defun pm-trfm-get-dir (mat dir)
  "Allocate and return a new pvec into which is stored the DIR (one
of :X, :Y, or :Z) direction vector from the rotation matrix MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-dir-into (pvec) mat dir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pmat pvec) pvec) pm-trfm-apply-into))
(declaim (inline pm-trfm-apply-into))
(defun pm-trfm-apply-into (transformed-point mat point)
  "Multiply the transformation matrix MAT against the POINT and store
the result into TRANSFORMED-POINT which is then returned."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pvec-accessors ((tp transformed-point) (p point))
    (with-pmat-accessors (m mat)
      (psetf tpx (as-double-float
                  (+ (* m00 px) (* m01 py) (* m02 pz) (* m03 1d0)))
             tpy (as-double-float
                  (+ (* m10 px) (* m11 py) (* m12 pz) (* m13 1d0)))
             tpz (as-double-float
                  (+ (* m20 px) (* m21 py) (* m22 pz) (* m23 1d0))))))
  transformed-point)

(declaim (ftype (function (pmat pvec) pvec) pm-trfm-apply))
(defun pm-trfm-apply (basis point)
  "Multiply the transformation matrix MAT against the POINT and return
the transformed point in a newly allocated pvec."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-apply-into (pvec) basis point))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat) pmat) pm-copy-rotation-into))
(defun pm-copy-rotation-into (dst src)
  "Copy just the 3x3 rotation submatrix from the SRC transformation matrix
into the DST transformation matrix then return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (s src))
    (psetf d00 s00 d01 s01 d02 s02
           d10 s10 d11 s11 d12 s12
           d20 s20 d21 s21 d22 s22))
  dst)

(declaim (ftype (function (pmat) pmat) pm-copy-rotation))
(defun pm-copy-rotation (src)
  "Allocate and return an identity matrix into which just the 3X3 rotation
submatrix of the SRC transformation matrix has been stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-copy-rotation-into (pm-eye) src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec &key (:stabilize t)) pmat)
                pm-trfm-local-axis-rotate-into))
(defun pm-trfm-local-axis-rotate-into (trfm rotation-vec &key (stabilize t))
  "In place rotate the transformation matrix TRFM as a relative
rotation (AKA the local axis rotation) as specified by the
ROTATION-VEC. The ROTATION-VEC defines the relative rotation around
each axis in the rotation submatrix. The rotation is stabilized if
keyword argument :STABILIZED is T (the default) but not
re-orthogonalized. Return TRFM as the result."

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
                        (pm-mul-into result trfm rot)
                        (pm-copy-rotation-into trfm result))))

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
      (pm-stabilize-into trfm)
      trfm))

(declaim (ftype (function (pmat pvec &key (:stabilize t)) pmat)
                pm-trfm-local-axis-rotate))
(defun pm-trfm-local-axis-rotate (trfm rotation-vec &key (stabilize t))
  "Return a newly allocated transformation matrix which was the
relative rotation (AKA the local axis rotation) as specified by the
ROTATION-VEC as applied to TRFM. The ROTATION-VEC defines the relative
rotation around each axis in the rotation submatrix (note, it does not
specify a vector around which a rotation happens). The rotation is
stabilized if keyword argument :STABILIZED is T (the default) but not
re-orthogonalized."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-local-axis-rotate-into (pm-copy trfm) rotation-vec
                                  :stabilize stabilize))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pm-trfm-scale-into (mat pvec)
  "Store a transformation matrix into MAT that scales the axes by the
respective PVEC amounts. Return MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-eye-into mat)
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (p pvec)
      (psetf m00 px
             m11 py
             m22 pz)))
  mat)

(defun pm-trfm-scale (pvec)
  "Return a newly allocated transformation matrix that represents a scaling
in each axis denoted by PVEC."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-scale-into (pm-eye) pvec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat keyword keyword) pmat)
                pm-create-view-into))
(defun pm-create-view-into (view camera at-dir up-dir)
  "VIEW is a transformation matrix into which the computed view
transformation matrix derived from the transformation matrix CAMERA
will be stored. AT-DIR represents the :x, :y, or :z axis in the CAMERA
rotation submatrix that will be used at the 'look at'
direction. UP-DIR represents the :x, :y, or :z axis that will be used
as the 'up' direction in the rotation submatrix."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (xv yv zv) (pm-trfm-get-raw-axes camera)
    (let* ((nn (ecase at-dir ((:x) xv) ((:y) yv) ((:z) zv)))
           (vv (ecase up-dir ((:x) xv) ((:y) yv) ((:z) zv)))
           ;; Create the u v n of the new view matrix
           (n (pv-normalize nn))
           (u (pv-normalize-into (pv-cross vv nn)))
           (v (pv-cross n u))
           ;; create an inverted camera rotation
           (tmp-rot (pm-trfm-invert-into (pm-trfm-set-raw-axes u v n)))
           ;; create an inverted translation matrix
           (tmp-trans (pm-trfm-set-trans
                       (pv-negate-into (pm-trfm-get-trans camera)))))
      ;; Create the actual inverted view matrix.
      (pm-mul-into view tmp-rot tmp-trans))))

(declaim (ftype (function (pmat keyword keyword) pmat) pm-create-view))
(defun pm-create-view (camera at-dir up-dir)
  "Allocate and return a transformation matrix into which the inverted view of
the camera is stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-create-view-into (pm-eye) camera at-dir up-dir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (let* ((p0 (pm-trfm-get-trans basis))
           ;; All axes are chosen such that their cross leads to a
           ;; vector pointing in the positive direction of the third
           ;; axis.
           (choice-a (ecase plane ((:xy) :x) ((:xz) :z) ((:yz) :y)))
           (choice-b (ecase plane ((:xy) :y) ((:xz) :x) ((:yz) :z)))
           (axis-a (pm-trfm-get-dir basis choice-a))
           (axis-b (pm-trfm-get-dir basis choice-b))
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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
