(in-package :option-9)

;;; NOTE: TODO: Change all long function names (and docstrings) to
;;; match the commented names by the short names. Also, make the last
;;; few functions starting with pm-create-view-into conform to the new
;;; naming method. Change all references to this API to use the
;;; correct (once they are written) long or short function names as
;;; appropriate.  Do the same type of nomenclature change to the
;;; pvec.lisp library too.


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

(declaim (ftype (function (pmat pmat) pmat) matrix-copy-into))
(defun matrix-copy-into (dst src)
  "Copy SRC into DST and return DST. DST and SRC may be EQ."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (s src))
    (psetf d00 s00 d01 s01 d02 s02 d03 s03
           d10 s10 d11 s11 d12 s12 d13 s13
           d20 s20 d21 s21 d22 s22 d23 s23
           d30 s30 d31 s31 d32 s32 d33 s33))
  dst)

(declaim (ftype (function (pmat pmat) pmat) mcpi))
(declaim (inline mcpi))
(defun mcpi (result src) ;; matrix-copy-into
  "Shortname for MATRIX-COPY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-into result src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-copy))
(defun matrix-copy (mat)
  "Return a newly allocated pmat into which MAT was copied."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-into (mi) mat))

(declaim (ftype (function (pmat) pmat) mcp))
(declaim (inline mcp))
(defun mcp (mat) ;; matrix-copy
  "Shortname for MATRIX-COPY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat) pmat) matrix-stabilize-into))
(defun matrix-stabilize-into (dst src)
  "Read all values from the SRC matrix and if they are less than
*PVEC-TOL*, collapse them to 0d0, or copy the original value, into the
corresponding location in DST.  SRC and DST may be EQ. Return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (s src))
    ;; This macro isn't entirely lexically/once-only safe, so don't abuse it.
    (macrolet ((stabilize (read-place)
                 `(if (< (as-double-float (abs ,read-place))
                         (as-double-float *pvec-tol*))
                      0d0
                      ,read-place)))
      (psetf d00 (stabilize s00)
             d01 (stabilize s01)
             d02 (stabilize s02)
             d03 (stabilize s03)
             d10 (stabilize s10)
             d11 (stabilize s11)
             d12 (stabilize s12)
             d13 (stabilize s13)
             d20 (stabilize s20)
             d21 (stabilize s21)
             d22 (stabilize s22)
             d23 (stabilize s23)
             d30 (stabilize s30)
             d31 (stabilize s31)
             d32 (stabilize s32)
             d33 (stabilize s33)))
    dst))

(declaim (ftype (function (pmat pmat) pmat) msti))
(declaim (inline msti))
(defun msti (dst src) ;; matrix-stabilize-into
  "Shortname for MATRIX-STABILIZE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-stabilize-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-stabilize))
(defun matrix-stabilize (mat)
  "Return a newly allocated matrix that contains the stabilized values
from MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((m (matrix-copy mat)))
    (matrix-stabilize-into m m)))

(declaim (ftype (function (pmat) pmat) mst))
(declaim (inline mst))
(defun mst (mat) ;; matrix-stabilize
  "Shortname for MATRIX-STABILIZE."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-stabilize mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matrix-test-into (mat)
  "Construct a test pattern into the 4x4 matrix MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((m mat))
    (psetf m00 1d0 m01 2d0 m02 3d0 m03 4d0
           m10 5d0 m11 6d0 m12 7d0 m13 8d0
           m20 9d0 m21 10d0 m22 11d0 m23 12d0
           m30 13d0 m31 14d0 m32 15d0 m33 16d0))
  mat)

(defun mtsti (mat) ;; matrix-test-into
  "Shortname for MATRIX-TEST-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-test-into mat))

;;; ;;;;;;;;

(defun matrix-test ()
  "Return a newly allocated 4x4 matrix that contains a test pattern."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((result (pmat)))
    (matrix-test-into result)))

(defun mtst () ;; matrix-test
  "Shortname for MATRIX-TEST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-test))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-identity-into))
(defun matrix-identity-into (mat)
  "Fill the matrix MAT with an identity matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (psetf m00 1.0d0 m01 0.0d0 m02 0.0d0 m03 0.0d0
           m10 0.0d0 m11 1.0d0 m12 0.0d0 m13 0.0d0
           m20 0.0d0 m21 0.0d0 m22 1.0d0 m23 0.0d0
           m30 0.0d0 m31 0.0d0 m32 0.0d0 m33 1.0d0))
  mat)

(declaim (ftype (function (pmat) pmat) mii))
(declaim (inline mii))
(defun mii (mat) ;; matrix-identity-into
  "Shortname for MATRIX-IDENTITY-INTO."
  (matrix-identity-into mat))

;;; ;;;;;;;;

(declaim (ftype (function () pmat) matrix-identity))
(defun matrix-identity ()
  "Return a newly allocated identity matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-identity-into (pmat)))

(declaim (ftype (function () pmat) mi))
(declaim (inline mi))
(defun mi () ;; matrix-identity
  "Shortname for MATRIX-IDENTITY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-identity))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat) pmat) matrix-transpose-into))
(declaim (inline matrix-transpose-into))
(defun matrix-transpose-into (dst src)
  "Transpose the 4x4 SRC and store it into DST. SRC and DST may be EQ"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (unless (eq dst src)
    (matrix-copy-into dst src))

  (with-pmat-accessors (d dst)
    (rotatef d10 d01)
    (rotatef d20 d02)
    (rotatef d30 d03)
    (rotatef d21 d12)
    (rotatef d31 d13)
    (rotatef d32 d23))
  dst)

(declaim (ftype (function (pmat pmat) pmat) mtpi))
(declaim (inline mtpi))
(defun mtpi (dst src) ;; matrix-transpose-into
  "Shortname for MATRIX-TRANSPOSE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-transpose-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-transpose))
(declaim (inline matrix-transpose))
(defun matrix-transpose (src)
  "Return a newly allocated 4x4 matrix which is the transpose of SRC"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-transpose-into (mi) src))

(declaim (ftype (function (pmat) pmat) mtp))
(declaim (inline mtp))
(defun mtp (src) ;; matrix-transpose
  "Shortname for MATRIX-TRANSPOSE."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-transpose src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: This looks scary in that I might be modifying RESULT while
;; still reading from it. Not so. The semantics of PSETF are that all
;; subforms are evaluated and then the assignments happen in any
;; order. So, no assignments can happen before the subforms are
;; finished being evaluated. Hence, I don't have to worry if result EQ
;; mat0 or mat1. It is the same for other uses of PSETF in this file.
(declaim (ftype (function (pmat pmat pmat) pmat) matrix-multiply-into))
(defun matrix-multiply-into (result mat0 mat1)
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

(declaim (ftype (function (pmat pmat pmat) pmat) mmi))
(declaim (inline mmi))
(defun mmi (result mat0 mat1) ;; matrix-multiply-into
  "Shortname for MATRIX-MULTIPLY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-multiply-into result mat0 mat1))

;;; ;;;;;;;;

(declaim (ftype (function (pmat pmat) pmat) matrix-multiply))
(defun matrix-multiply (mat0 mat1)
  "Perform matrix multiply of MAT0 * MAT1 and return new pmat of result."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-multiply-into (pmat) mat0 mat1))

(declaim (ftype (function (pmat pmat) pmat) mm))
(declaim (inline mm))
(defun mm (mat0 mat1) ;; matrix-multiply
  "Shortname for MATRIX-MULTIPLY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-multiply mat0 mat1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat double-float pvec &key (:stabilize t)) pmat)
                matrix-rotate-around-into))
(defun matrix-rotate-around-into (rotation angle axis &key (stabilize t))
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
      (matrix-stabilize-into rotation rotation)
      rotation))

(declaim (ftype (function (pmat double-float pvec &key (:stabilize t)) pmat)
                mrai))
(declaim (inline mrai))
(defun mrai (rotation angle axis &key (stabilize t));; matrix-rotate-around-into
  "Shortname for PM-TRFM-ROTATE-AROUND."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotate-around-into rotation angle axis :stabilize stabilize))

;;; ;;;;;;;;

(declaim (ftype (function (double-float pvec &key (:stabilize t)) pmat)
                matrix-rotate-around))
(defun matrix-rotate-around (angle axis &key (stabilize t))
  "Allocate and return a transformation matrix with a (0 0 0)
translation vector that will rotate around the vector AXIS by the
specified ANGLE. This assumes a right handed coordinate system."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotate-around-into (matrix-identity) angle axis
                             :stabilize stabilize))

(declaim (ftype (function (double-float pvec &key (:stabilize t)) pmat)
                mra))
(declaim (inline mra))
(defun mra (angle axis &key (stabilize t)) ;; matrix-rotate-around
  "Shortname for PM-TRFM-ROTATE-AROUND."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotate-around angle axis :stabilize stabilize))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) matrix-scale-into))
(declaim (inline matrix-scale-into))
(defun matrix-scale-into (dst pvec)
  "Store a transformation matrix into DST that scales the axes by the
respective PVEC amounts. Return DST. Similar to glScale()."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-identity-into dst)
  (with-pmat-accessors (d dst)
    (with-pvec-accessors (p pvec)
      (psetf d00 px
             d11 py
             d22 pz)))
  dst)

(declaim (ftype (function (pmat pvec) pmat) msci))
(declaim (inline msci))
(defun msci (mat pvec) ;; matrix-scale-into
  "Shortname for MATRIX-SCALE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale-into mat pvec))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pmat) matrix-scale))
(defun matrix-scale (pvec)
  "Return a newly allocated transformation matrix that represents a scaling
in each axis denoted by PVEC. Similar to glScale()."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale-into (matrix-identity) pvec))

(declaim (ftype (function (pvec) pmat) msc))
(declaim (inline msc))
(defun msc (pvec) ;; matrix-scale
  "Shortname for MATRIX-SCALE."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale pvec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) matrix-translate-into))
(declaim (inline matrix-translate-into))
(defun matrix-translate-into (mat pvec)
  "Store a transformation matrix into MAT that translates the
coordinate system by the respective PVEC amounts. Return MAT. Similar
to glTranslate()"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-identity-into mat)
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (p pvec)
      (psetf m03 px
             m13 py
             m23 pz)))
  mat)

(declaim (ftype (function (pmat pvec) pmat) mtri))
(declaim (inline mtri))
(defun mtri (mat pvec) ;; matrix-translate-into
  "Shortname for MATRIX-TRANSLATE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-into mat pvec))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pmat) matrix-translate))
(defun matrix-translate (pvec)
  "Return a newly allocated transformation matrix that translates the
coordinate system by the respective PVEC amounts. Similar
to glTranslate()"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-into (matrix-identity) pvec))

(declaim (ftype (function (pvec) pmat) mtr))
(declaim (inline mtr))
(defun mtr (pvec) ;; matrix-translate
  "Shortname for MATRIX-TRANSLATE."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate pvec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: This is special to orthonormal basis transformation matricies.
(declaim (ftype (function (pmat pmat) pmat) matrix-invert-trfm-into))
(declaim (inline matrix-invert-trfm-into))
(defun matrix-invert-trfm-into (result mat)
  "Invert (specifically an) orthonormal transformation matrix (with a
rotation operator in the upper left 3x3 matrix and a translation
vector in the 4x1 column on the right) MAT and store into
RESULT. RESULT can be EQ with MAT. This means 1) store the transpose
the 3x3 rotation matrix contained in the upper left hand of the
transformation matrix, 2) store the application of the inverted
rotation to the negation of the 4x1 translation column.  Return the
stabilized RESULT. This function will not invert arbitrary 4x4
matricies."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (unless (eq result mat)
    (matrix-copy-into result mat))

  (with-pmat-accessors (r result)
    ;; Transpose the upper left square 3x3 portion of the rotation matrix
    (rotatef r10 r01)
    (rotatef r20 r02)
    (rotatef r21 r12)

    ;; Invert the translation by applying the inverted rotation to the
    ;; negated translation

    (psetf r03 (as-double-float
                (+ (* r00 (- r03)) (* r01 (- r13)) (* r02 (- r23))))
           r13 (as-double-float
                (+ (* r10 (- r03)) (* r11 (- r13)) (* r12 (- r23))))
           r23 (as-double-float
                (+ (* r20 (- r03)) (* r21 (- r13)) (* r22 (- r23))))))

  (matrix-stabilize result))

(declaim (ftype (function (pmat pmat) pmat) minvti))
(declaim (inline minvti))
(defun minvti (result mat) ;; matrix-invert-transform-into
  "Shortname for MATRIX-INVERT-TRFM-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-trfm-into result mat))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-invert-trfm))
(defun matrix-invert-trfm (mat)
  "Invert the transformation matrix MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-trfm-into (mi) mat))

(declaim (ftype (function (pmat) pmat) minvt))
(declaim (inline minvt))
(defun minvt (mat) ;; matrix-invert-transform
  "Shortname for MATRIX-INVERT-TRFM."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-trfm mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat) (values pmat t)) matrix-invert-into))
(defun matrix-invert-into (dst src)
  "Invert an arbitrary 4x4 matrix in SRC and put the result into DST.
Return the values of DST and T if the inversion happened, or an identity
matrix and NIL if it couldn't happen."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))

  ;; Hrm, I hope the compiler optimizes this nicely.
  ;; Gotten from:
  ;; http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html
  (with-pmat-accessors (s src)
    (let ((det-s
           (as-double-float
            (- (+ (* s00 s11 s22 s33) (* s00 s12 s23 s31) (* s00 s13 s21 s32)
                  (* s01 s10 s23 s32) (* s01 s12 s20 s33) (* s01 s13 s22 s30)
                  (* s02 s10 s21 s33) (* s02 s11 s23 s30) (* s02 s13 s20 s31)
                  (* s03 s10 s22 s31) (* s03 s11 s20 s32) (* s03 s12 s21 s30))

               (* s00 s11 s23 s32) (* s00 s12 s21 s33) (* s00 s13 s22 s31)
               (* s01 s10 s22 s33) (* s01 s12 s23 s30) (* s01 s13 s20 s32)
               (* s02 s10 s23 s31) (* s02 s11 s20 s33) (* s02 s13 s21 s30)
               (* s03 s10 s21 s32) (* s03 s11 s22 s30) (* s03 s12 s20 s31)))))

      #+option-9-optimize-pmat (declare (type double-float det-s))

      ;; Bail of the inversion doesn't exist.
      (when (< det-s *pvec-tol*)
        (matrix-identity-into dst)
        (return-from matrix-invert-into (values dst NIL)))

      ;; The determinent exists, so compute the inverse into dst.
      (with-pmat-accessors (d dst)
        (psetf d00 (as-double-float
                    (/ (- (+ (* s11 s22 s33) (* s12 s23 s31) (* s13 s21 s32))
                          (* s11 s23 s32) (* s12 s21 s33) (* s13 s22 s31))
                       det-s))

               d01 (as-double-float
                    (/ (- (+ (* s01 s23 s32) (* s02 s21 s33) (* s03 s22 s31))
                          (* s01 s22 s33) (* s02 s23 s31) (* s03 s21 s32))
                       det-s))

               d02 (as-double-float
                    (/ (- (+ (* s01 s12 s33) (* s02 s13 s31) (* s03 s11 s32))
                          (* s01 s13 s32) (* s02 s11 s33) (* s03 s12 s31))
                       det-s))

               d03 (as-double-float
                    (/ (- (+ (* s01 s13 s22) (* s02 s11 s23) (* s03 s12 s21))
                          (* s01 s12 s23) (* s02 s13 s21) (* s03 s11 s22))
                       det-s))

               d10 (as-double-float
                    (/ (- (+ (* s10 s23 s32) (* s12 s20 s33) (* s13 s22 s30))
                          (* s10 s22 s33) (* s12 s23 s30) (* s13 s20 s32))
                       det-s))

               d11 (as-double-float
                    (/ (- (+ (* s00 s22 s33) (* s02 s23 s30) (* s03 s20 s32))
                          (* s00 s23 s32) (* s02 s20 s33) (* s03 s22 s30))
                       det-s))

               d12 (as-double-float
                    (/ (- (+ (* s00 s13 s32) (* s02 s10 s33) (* s03 s12 s30))
                          (* s00 s12 s33) (* s02 s13 s30) (* s03 s10 s32))
                       det-s))

               d13 (as-double-float
                    (/ (- (+ (* s00 s12 s23) (* s02 s13 s20) (* s03 s10 s22))
                          (* s00 s13 s22) (* s02 s10 s23) (* s03 s12 s20))
                       det-s))

               d20 (as-double-float
                    (/ (- (+ (* s10 s21 s33) (* s11 s23 s30) (* s13 s20 s31))
                          (* s10 s23 s31) (* s11 s20 s33) (* s13 s21 s30))
                       det-s))

               d21 (as-double-float
                    (/ (- (+ (* s00 s23 s31) (* s01 s20 s33) (* s03 s21 s30))
                          (* s00 s21 s33) (* s01 s23 s30) (* s03 s20 s31))
                       det-s))

               d22 (as-double-float
                    (/ (- (+ (* s00 s11 s33) (* s01 s13 s30) (* s03 s10 s31))
                          (* s00 s13 s31) (* s01 s10 s33) (* s03 s11 s30))
                       det-s))

               d23 (as-double-float
                    (/ (- (+ (* s00 s13 s21) (* s01 s10 s23) (* s03 s11 s20))
                          (* s00 s11 s23) (* s01 s13 s20) (* s03 s10 s21))
                       det-s))

               d30 (as-double-float
                    (/ (- (+ (* s10 s22 s31) (* s11 s20 s32) (* s12 s21 s30))
                          (* s10 s21 s32) (* s11 s22 s30) (* s12 s20 s31))
                       det-s))

               d31 (as-double-float
                    (/ (- (+ (* s00 s21 s32) (* s01 s22 s30) (* s02 s20 s31))
                          (* s00 s22 s31) (* s01 s20 s32) (* s02 s21 s30))
                       det-s))

               d32 (as-double-float
                    (/ (- (+ (* s00 s12 s31) (* s01 s10 s32) (* s02 s11 s30))
                          (* s00 s11 s32) (* s01 s12 s30) (* s02 s10 s31))
                       det-s))

               d33 (as-double-float
                    (/ (- (+ (* s00 s11 s22) (* s01 s12 s20) (* s02 s10 s21))
                          (* s00 s12 s21) (* s01 s10 s22) (* s02 s11 s20))
                       det-s))))

      (values (matrix-stabilize-into dst dst) t))))

(declaim (ftype (function (pmat pmat) (values pmat t)) minvi))
(declaim (inline minvi))
(defun minvi (dst src) ;; matrix-invert-into
  "Shortname for MATRIX-INVERT-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) (values pmat t)) matrix-invert))
(defun matrix-invert (mat)
  "Allocate a new matrix and place into it the inverse of the arbitrary
4x4 matrix MAT. Return the values of a stabilized inverted matrix and T if
the inversion was possible, or an identity matrix and NIL if it wasn't
 possible."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-into (mi) mat))

(declaim (ftype (function (pmat) (values pmat t)) minv))
(declaim (inline minv))
(defun minv (mat) ;; matrix-invert
  "Shortname for MATRIX-INVERT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert mat))

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

(declaim (ftype (function ((simple-array double-float (16)) pmat)
                          (simple-array double-float (16)))
                mctoi))
(declaim (inline mctoi))
(defun mctoi (ogl mat) ;; matrix-convert-to-opengl-into
  "Shortname for PM-CONVERT-TO-OPENGL-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-convert-to-opengl-into ogl mat))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) (simple-array double-float (16)))
                pm-convert-to-opengl))
(declaim (inline pm-convert-to-opengl))
(defun pm-convert-to-opengl (mat)
  "Convert the MAT matrix into newly allocated column-major ordered
simple-array double-float (16) suitable for OpenGL and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((ogl (make-array 16 :element-type 'double-float
                         :initial-element 0d0)))
    (declare ((simple-array double-float (16)) ogl))
    (pm-convert-to-opengl-into ogl mat)
    ogl))

(declaim (ftype (function (pmat) (simple-array double-float (16))) mcto))
(declaim (inline mcto))
(defun mcto (mat) ;; matrix-convert-to-opengl
  "Shortname for PM-CONVERT-TO-OPENGL."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-convert-to-opengl mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat (simple-array double-float (16))) pmat)
                pm-convert-from-opengl-into))
(declaim (inline pm-convert-from-opengl-into))
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

(declaim (ftype (function (pmat (simple-array double-float (16))) pmat) mcfoi))
(declaim (inline mcfoi))
(defun mcfoi (mat ogl) ;; matrix-convert-from-opengl-into
  "Shortname for PM-CONVERT-FROM-OPENGL-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-convert-from-opengl-into mat ogl))

;;; ;;;;;;;;

(declaim (ftype (function ((simple-array double-float (16))) pmat)
                pm-convert-from-opengl))
(declaim (inline pm-convert-from-opengl))
(defun pm-convert-from-opengl (ogl)
  "Convert the OGL OpenGL matrix into a newly allocated MAT format and
return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare ((simple-array double-float (16)) ogl))
  (pm-convert-from-opengl-into (pmat) ogl))

(defun mcfo (ogl) ;; matrix-convert-from-opengl
  "Shortname for PM-CONVERT-FROM-OPENGL."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-convert-from-opengl ogl))

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

(declaim (ftype (function (pmat pvec) pmat) mtrsi))
(declaim (inline mtrsi))
(defun mtrsi (mat trans) ;; matrix-translate-set-into
  "Shortname for PM-TRFM-SET-TRANS-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-trans-into mat trans))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pmat) pm-trfm-set-trans))
(declaim (inline pm-trfm-set-trans))
(defun pm-trfm-set-trans (trans)
  "Allocate and return a new identity transformation matrix with TRANS
as its translation."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-trans-into (matrix-identity) trans))

(declaim (ftype (function (pvec) pmat) pm-trfm-set-trans))
(declaim (inline mtrs))
(defun mtrs (trans) ;; matrix-translate-set
  "Shortname for PM-TRFM-SET-TRANS."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-trans trans))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pmat) pvec) pm-trfm-get-trans-into))
(declaim (inline pm-trfm-get-trans-into))
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

(declaim (ftype (function (pvec pmat) pvec) mtrgi))
(declaim (inline mtrgi))
(defun mtrgi (trans mat) ;; matrix-translate-get-into
  "Shortname for PM-TRFM-GET-TRANS-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-trans-into trans mat))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pvec) pm-trfm-get-trans))
(declaim (inline pm-trfm-get-trans))
(defun pm-trfm-get-trans (mat)
  "Return the translation column of the transformation matrix MAT as a
newly allocated PVEC and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-trans-into (pvec) mat))

(declaim (ftype (function (pmat) pvec) mtrg))
(declaim (inline mtrg))
(defun mtrg (mat) ;; matrix-translate-get
  "Shortname for PM-TRFM-GET-TRANS."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-trans mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pm-trfm-get-raw-axes-into (xdir ydir zdir mat)
  "Store the raw direction vectors into XDIR, YDIR, and ZDIR, from the transformation matrix MAT. Return values of XDIR, YDIR, and ZDIR."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-multiple-pvec-accessors ((x xdir) (y ydir) (z zdir))
      (psetf xx m00 xy m10 xz m20
             yx m01 yy m11 yz m21
             zx m02 zy m12 zz m22)))
  (values xdir ydir zdir))

(defun mrvgi (xdir ydir zdir mat) ;; matrix-rotation-vectors-get-into
  "Shortname for PM-TRFM-GET-RAW-AXES-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-raw-axes-into xdir ydir zdir mat))

;;; ;;;;;;;;

(defun pm-trfm-get-raw-axes (mat)
  "Allocate and return three values that record the raw direction
vectors into XDIR, YDIR, and ZDIR, from MAT.  Return values of XDIR,
YDIR, and ZDIR."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-raw-axes-into (pvec) (pvec) (pvec) mat))

(defun mrvg (mat) ;; matrix-rotation-vectors-get
  "Shortname for PM-TRFM-GET-RAW-AXES."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-raw-axes mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec pvec pvec) pmat)
                pm-trfm-set-raw-axes-into))
(defun pm-trfm-set-raw-axes-into (mat xdir ydir zdir)
  "Assign the supplied direction vectors XDIR YDIR ZDIR into the
transformation MAT and return MAT."
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

(declaim (ftype (function (pmat pvec pvec pvec) pmat) mrvsi))
(declaim (inline mrvsi))
(defun mrvsi (mat xdir ydir zdir) ;; matrix-rotation-vectors-set-into
  "Shortname for PM-TRFM-SET-RAW-AXES-INTO"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-raw-axes-into mat xdir ydir zdir))

;;; ;;;;;;;;

(declaim (ftype (function (pvec pvec pvec) pmat) pm-trfm-set-raw-axes))
(defun pm-trfm-set-raw-axes (xdir ydir zdir)
  "Allocate an identity transformation matrix and then set the raw
axes supplied as XDIR YDIR ZDIR into the rotation components of the
matrix. Return the transformation matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-raw-axes-into (matrix-identity) xdir ydir zdir))

(declaim (ftype (function (pvec pvec pvec) pmat) mrvs))
(declaim (inline mrvs))
(defun mrvs (xdir ydir zdir) ;; matrix-rotation-vectors-set
  "Shortname for PM-TRFM-SET-RAW-AXES"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-raw-axes xdir ydir zdir))

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

(declaim (ftype (function (pvec pmat keyword) pvec) mrdgi))
(declaim (inline mrdgi))
(defun mrdgi (pv mat dir) ;; matrix-rotation-direction-get-into
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-dir-into pv mat dir))

;;; ;;;;;;;;

(declaim (ftype (function (pmat keyword) pvec) pm-trfm-get-dir))
(defun pm-trfm-get-dir (mat dir)
  "Allocate and return a new pvec into which is stored the DIR (one
of :X, :Y, or :Z) direction vector from the rotation matrix MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-dir-into (pvec) mat dir))

(declaim (ftype (function (pmat keyword) pvec) mrdg))
(declaim (inline mrdg))
(defun mrdg (mat dir) ;; matrix-rotation-direction-get
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-get-dir mat dir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat keyword pvec) pmat) pm-trfm-set-dir-into))
(defun pm-trfm-set-dir-into (mat dir pv)
  "Store the direction vector PV into the rotation operation axis specified
by DIR (one of :X, :Y, or :Z) in the transformation matrix MAT.
Return MAT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pvec-accessors (p pv)
      (ecase dir
        ((:x)
         (psetf m00 px
                m10 py
                m20 pz))
        ((:y)
         (psetf m01 px
                m11 py
                m21 pz))
        ((:z)
         (psetf m02 px
                m12 py
                m22 pz)))))
  mat)

(declaim (ftype (function (pmat keyword pvec) pmat) mrdsi))
(declaim (inline mrdsi))
(defun mrdsi (mat dir pv) ;; matrix-rotation-direction-set-into
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-dir-into mat dir pv))

;;; ;;;;;;;;

(declaim (ftype (function (keyword pvec) pmat) pm-trfm-set-dir))
(defun pm-trfm-set-dir (dir pv)
  "Allocate and return a new identity transformation matrix into which
we store the direction vector PV into the rotation operation axis
specified by DIR (one of :X, :Y, or :Z)."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-dir-into (matrix-identity) dir pv))

(declaim (ftype (function (keyword pvec) pmat) mrds))
(declaim (inline mrds))
(defun mrds (dir pv) ;; matrix-rotation-direction-set
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-set-dir dir pv))

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

(declaim (ftype (function (pmat pvec) pmat) mfi))
(declaim (inline mfi))
(defun mfi (mat fly-vec) ;; matrix-fly-into
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-fly-into mat fly-vec))

;;; ;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) pm-trfm-fly))
(defun pm-trfm-fly (mat fly-vec)
  "Copy the transformation matrix MAT and add each component of the
FLY-VEC to the translation portion of the transformation matrix MAT in
the direction of the respective basis direction in the rotation
submatrix and write back into MAT. Return the copy."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-fly-into (matrix-copy mat) fly-vec))

(declaim (ftype (function (pmat pvec) pmat) mf))
(declaim (inline mf))
(defun mf (mat fly-vec) ;; matrix-fly
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-fly mat fly-vec))

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

(declaim (ftype (function (pvec pmat pvec) pvec) mai))
(declaim (inline mai))
(defun mai (transformed-point mat point) ;; matrix-apply-into
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-apply-into transformed-point mat point))

;;; ;;;;;;;;

(declaim (ftype (function (pmat pvec) pvec) pm-trfm-apply))
(defun pm-trfm-apply (mat point)
  "Multiply the transformation matrix MAT against the POINT and return
the transformed point in a newly allocated pvec."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-apply-into (pvec) mat point))

(declaim (ftype (function (pmat pvec) pvec) ma))
(declaim (inline ma))
(defun ma (mat point) ;; matrix-apply
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-apply mat point))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat) pmat) pm-copy-rotation-into))
(defun pm-copy-rotation-into (dst src)
  "Store just the 3x3 rotation submatrix from the SRC transformation matrix
into the DST transformation matrix then return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (unless (eq dst src)
    (with-multiple-pmat-accessors ((d dst) (s src))
      (psetf d00 s00 d01 s01 d02 s02
             d10 s10 d11 s11 d12 s12
             d20 s20 d21 s21 d22 s22)))
  dst)

(declaim (ftype (function (pmat pmat) pmat) mcri))
(declaim (inline mcri))
(defun mcri (dst src) ;; matrix-copy-rotation-into
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-copy-rotation-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) pm-copy-rotation))
(defun pm-copy-rotation (src)
  "Allocate and return an identity transformation matrix into which
just the 3X3 rotation submatrix of the SRC transformation matrix has
been stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-copy-rotation-into (matrix-identity) src))

(declaim (ftype (function (pmat) pmat) mcr))
(declaim (inline mcr))
(defun mcr (src) ;; matrix-copy-rotation
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-copy-rotation src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO, fix this to write the output into a destination (which can be
;; EQ to a src matrix) like the others...


(declaim (ftype (function (pmat pmat pvec &key (:stabilize t)) pmat)
                pm-trfm-local-axis-rotate-into))
(defun pm-trfm-local-axis-rotate-into (result trfm rotation-vec &key (stabilize t))
  "Store into RESULT a rotation of the transformation matrix TRFM as a
relative rotation (AKA the local axis rotation) as specified by the
ROTATION-VEC. The components of the ROTATION-VEC define the relative
rotation in radians around each axis in the rotation submatrix. The
rotation is stabilized if keyword argument :STABILIZED is T (the
default) but not re-orthogonalized. Return RESULT."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((rot (matrix-identity)))
    (with-pmat-accessors (rt rot)
      (with-pvec-accessors (rv rotation-vec)

        ;; We overwrite these values for each rotation in such a
        ;; manner as to leave everything consistent with mostly
        ;; minimal stores no matter what rotations may happen.

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
                        (matrix-multiply-into result trfm rot)
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
      (matrix-stabilize-into result result)
      result))

(declaim (ftype (function (pmat pmat pvec &key (:stabilize t)) pmat) mlari))
(declaim (inline mlari))
(defun mlari (result trfm rotation-vec &key (stabilize t)) ;; matrix-local-axis-rotate-into
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-local-axis-rotate-into result trfm rotation-vec
                                  :stabilize stabilize))

;;; ;;;;;;;;

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
  (pm-trfm-local-axis-rotate-into (matrix-identity) trfm rotation-vec
                                  :stabilize stabilize))

(declaim (ftype (function (pmat pvec &key (:stabilize t)) pmat) mlar))
(declaim (inline mlar))
(defun mlar (trfm rotation-vec &key (stabilize t)) ;; matrix-local-axis-rotate
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-trfm-local-axis-rotate trfm rotation-vec :stabilize stabilize))

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
           (tmp-rot (matrix-invert-trfm-into (mi) (pm-trfm-set-raw-axes u v n)))
           ;; create an inverted translation matrix
           (tmp-trans (pm-trfm-set-trans
                       (pv-negate-into (pm-trfm-get-trans camera)))))
      ;; Create the actual inverted view matrix.
      (matrix-multiply-into view tmp-rot tmp-trans))))

;;; ;;;;;;;;

(declaim (ftype (function (pmat keyword keyword) pmat) pm-create-view))
(defun pm-create-view (camera at-dir up-dir)
  "Allocate and return a transformation matrix into which the inverted view of
the camera is stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (pm-create-view-into (matrix-identity) camera at-dir up-dir))

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
