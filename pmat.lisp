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

#+(or (not option-9-optimize-pmat) option-9-debug)
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
    `(with-accessors ((,(make-accessor-symbol prefix-symbol "00") pmat-m00)
                      (,(make-accessor-symbol prefix-symbol "01") pmat-m01)
                      (,(make-accessor-symbol prefix-symbol "02") pmat-m02)
                      (,(make-accessor-symbol prefix-symbol "03") pmat-m03)
                      (,(make-accessor-symbol prefix-symbol "10") pmat-m10)
                      (,(make-accessor-symbol prefix-symbol "11") pmat-m11)
                      (,(make-accessor-symbol prefix-symbol "12") pmat-m12)
                      (,(make-accessor-symbol prefix-symbol "13") pmat-m13)
                      (,(make-accessor-symbol prefix-symbol "20") pmat-m20)
                      (,(make-accessor-symbol prefix-symbol "21") pmat-m21)
                      (,(make-accessor-symbol prefix-symbol "22") pmat-m22)
                      (,(make-accessor-symbol prefix-symbol "23") pmat-m23)
                      (,(make-accessor-symbol prefix-symbol "30") pmat-m30)
                      (,(make-accessor-symbol prefix-symbol "31") pmat-m31)
                      (,(make-accessor-symbol prefix-symbol "32") pmat-m32)
                      (,(make-accessor-symbol prefix-symbol "33") pmat-m33))
         ,pmat
       ,@body))

  (defmacro with-multiple-pmat-accessors (sbinds &body body)
    (if (null sbinds)
        `(progn ,@body)
        `(with-pmat-accessors ,(car sbinds)
           (with-multiple-pmat-accessors ,(cdr sbinds) ,@body)))))

(defun matrix-print (str obj)
  "Define a pretty printer for nicely formatted pmats.

   NOTE: I must use the pprint-dispatch table instead of PRINT-OBJECT
   because pmats aren't a CLASS due to the defstruct definition I am using."
  (print-unreadable-object (obj str)
    (with-pmat-accessors (m obj)
      (format str
              "[~A ~A ~A ~A]~%  [~A ~A ~A ~A]~%  [~A ~A ~A ~A]~%  [~A ~A ~A ~A]"
              m00 m01 m02 m03
              m10 m11 m12 m13
              m20 m21 m22 m23
              m30 m31 m32 m33))))
;; Priority of 1 given incase other arrays of type single-float (16) are
;; pretty printed. This will enable ours to take precedence.
(set-pprint-dispatch 'pmat 'matrix-print 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pmat-aref (mat row col)
  ;; linearize the row/column index into a linear index
  (the double-float(aref mat (+ (* row 4) col))))

(defun (setf pmat-aref) (new-val mat row col)
  ;; linearize the row/column index into a linear index.
  (setf (the double-float (aref mat (+ (* row 4) col)))
        (the double-float new-val)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-identity-into))
(defun matrix-identity-into (dst)
  "Fill the matrix DST with an identity matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (d dst)
    (psetf d00 1.0d0 d01 0.0d0 d02 0.0d0 d03 0.0d0
           d10 0.0d0 d11 1.0d0 d12 0.0d0 d13 0.0d0
           d20 0.0d0 d21 0.0d0 d22 1.0d0 d23 0.0d0
           d30 0.0d0 d31 0.0d0 d32 0.0d0 d33 1.0d0))
  dst)

(declaim (ftype (function (pmat) pmat) mii))
(declaim (inline mii))
(defun mii (dst) ;; matrix-identity-into
  "Shortname for MATRIX-IDENTITY-INTO."
  (matrix-identity-into dst))

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
(defun mcpi (dst src) ;; matrix-copy-into
  "Shortname for MATRIX-COPY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-copy))
(defun matrix-copy (src)
  "Return a newly allocated pmat into which SRC was copied."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-into (mi) src))

(declaim (ftype (function (pmat) pmat) mcp))
(declaim (inline mcp))
(defun mcp (src) ;; matrix-copy
  "Shortname for MATRIX-COPY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pmat) matrix-clamp-into))
(defun matrix-clamp-into (dst src &key (min-val least-negative-double-float)
                                    (max-val most-positive-double-float))
  "Read all values from the SRC matrix and clamp them between MIN-VAL and
MAX-VAL. DST and SRC may be EQ. Return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (s src))
    ;; This macro isn't entirely lexically/once-only safe, so don't abuse it.
    (macrolet ((clamp (read-place)
                 `(cond
                    ((< (as-double-float ,read-place)
                        (as-double-float min-val))
                     (as-double-float min-val))

                    ((> (as-double-float ,read-place)
                        (as-double-float max-val))
                     (as-double-float max-val))

                    (t
                     ,read-place))))
      (psetf d00 (clamp s00)
             d01 (clamp s01)
             d02 (clamp s02)
             d03 (clamp s03)
             d10 (clamp s10)
             d11 (clamp s11)
             d12 (clamp s12)
             d13 (clamp s13)
             d20 (clamp s20)
             d21 (clamp s21)
             d22 (clamp s22)
             d23 (clamp s23)
             d30 (clamp s30)
             d31 (clamp s31)
             d32 (clamp s32)
             d33 (clamp s33)))
    dst))

(declaim (ftype (function (pmat pmat &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pmat) mci))
(declaim (inline mci))
(defun mci (dst src &key (min-val least-negative-double-float)
                      (max-val most-positive-double-float)) ;; matrix-clamp-into
  "Shortname for MATRIX-CLAMP-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-clamp-into dst src :min-val min-val :max-val max-val))

;;; ;;;;;;;;

(declaim (ftype (function (pmat &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pmat) matrix-clamp))
(defun matrix-clamp (src &key (min-val least-negative-double-float)
                           (max-val most-positive-double-float))
  "Return a newly allocated matrix that contains the clamped values
from SRC."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((m (matrix-copy src)))
    (matrix-clamp-into m m :min-val min-val :max-val max-val)))

(declaim (ftype (function (pmat &key
                                (:min-val double-float)
                                (:max-val double-float))
                          pmat) mc))
(declaim (inline mc))
(defun mc (src &key (min-val least-negative-double-float)
                 (max-val most-positive-double-float)) ;; matrix-clamp
  "Shortname for MATRIX-CLAMP."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-clamp src :min-val min-val :max-val max-val))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matrix-test-into (dst)
  "Construct a test pattern into the 4x4 matrix DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst))
    (psetf d00 1d0 d01 2d0 d02 3d0 d03 4d0
           d10 5d0 d11 6d0 d12 7d0 d13 8d0
           d20 9d0 d21 10d0 d22 11d0 d23 12d0
           d30 13d0 d31 14d0 d32 15d0 d33 16d0))
  dst)

(defun mtsti (dst) ;; matrix-test-into
  "Shortname for MATRIX-TEST-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-test-into dst))

;;; ;;;;;;;;

(defun matrix-test ()
  "Return a newly allocated 4x4 matrix that contains a test pattern."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((dst (pmat)))
    (matrix-test-into dst)))

(defun mtst () ;; matrix-test
  "Shortname for MATRIX-TEST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-test))

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
(defun matrix-multiply-into (dst mat0 mat1)
  "Perform matrix multiply of MAT0 * MAT1 and store into DST. DST can
be EQ to MAT0 or MAT1."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (a mat0) (b mat1))
    (psetf d00 (as-double-float
                (+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30)))
           d10 (as-double-float
                (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30)))
           d20 (as-double-float
                (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30)))
           d30 (as-double-float
                (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30)))

           d01 (as-double-float
                (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31)))
           d11 (as-double-float
                (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31)))
           d21 (as-double-float
                (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31)))
           d31 (as-double-float
                (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31)))

           d02 (as-double-float
                (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32)))
           d12 (as-double-float
                (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32)))
           d22 (as-double-float
                (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32)))
           d32 (as-double-float
                (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32)))

           d03 (as-double-float
                (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33)))
           d13 (as-double-float
                (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33)))
           d23 (as-double-float
                (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33)))
           d33 (as-double-float
                (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33)))))
  dst)

(declaim (ftype (function (pmat pmat pmat) pmat) mmi))
(declaim (inline mmi))
(defun mmi (dst mat0 mat1) ;; matrix-multiply-into
  "Shortname for MATRIX-MULTIPLY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-multiply-into dst mat0 mat1))

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

(declaim (ftype (function (pmat double-float pvec) pmat)
                matrix-rotate-around-into))
(defun matrix-rotate-around-into (rotation angle axis)
  "Store a computed rotation matrix with a (0 0 0) translation vector
into the ROTATION transformation matrix that will rotate around the
vector AXIS by the specified ANGLE. This assumes a right handed
coordinate system. Similar to glRotate()."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((norm-axis (vnormalize axis))
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

  rotation)

(declaim (ftype (function (pmat double-float pvec) pmat)
                mrai))
(declaim (inline mrai))
(defun mrai (rotation angle axis);; matrix-rotate-around-into
  "Shortname for PM-TRFM-ROTATE-AROUND."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotate-around-into rotation angle axis))

;;; ;;;;;;;;

(declaim (ftype (function (double-float pvec) pmat)
                matrix-rotate-around))
(defun matrix-rotate-around (angle axis)
  "Allocate and return a transformation matrix with a (0 0 0)
translation vector that will rotate around the vector AXIS by the
specified ANGLE. This assumes a right handed coordinate system."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotate-around-into (matrix-identity) angle axis))

(declaim (ftype (function (double-float pvec) pmat)
                mra))
(declaim (inline mra))
(defun mra (angle axis) ;; matrix-rotate-around
  "Shortname for PM-TRFM-ROTATE-AROUND."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotate-around angle axis))

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
(defun msci (dst pvec) ;; matrix-scale-into
  "Shortname for MATRIX-SCALE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale-into dst pvec))

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

(declaim (ftype (function (pmat pmat double-float) pmat) matrix-scale-by-into))
(declaim (inline matrix-scale-by-into))
(defun matrix-scale-by-into (dst src scalar)
  "Store a matrix into DST that is SRC scaled by the scalar
SCALAR. Return DST. DST may be EQ to SRC."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pmat-accessors ((d dst) (s src))
    (psetf d00 (as-double-float (* s00 scalar))
           d10 (as-double-float (* s10 scalar))
           d20 (as-double-float (* s20 scalar))
           d30 (as-double-float (* s30 scalar))

           d01 (as-double-float (* s01 scalar))
           d11 (as-double-float (* s11 scalar))
           d21 (as-double-float (* s21 scalar))
           d31 (as-double-float (* s31 scalar))

           d02 (as-double-float (* s02 scalar))
           d12 (as-double-float (* s12 scalar))
           d22 (as-double-float (* s22 scalar))
           d32 (as-double-float (* s32 scalar))

           d03 (as-double-float (* s03 scalar))
           d13 (as-double-float (* s13 scalar))
           d23 (as-double-float (* s23 scalar))
           d33 (as-double-float (* s33 scalar))))
  dst)

(declaim (ftype (function (pmat pmat double-float) pmat) mscbi))
(declaim (inline msci))
(defun mscbi (dst src scalar) ;; matrix-scale-by-into
  "Shortname for MATRIX-SCALE-BY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale-by-into dst src scalar))

;;; ;;;;;;;;

(declaim (ftype (function (pmat double-float) pmat) matrix-scale-by))
(defun matrix-scale-by (src scalar)
  "Return a newly allocated identity matrix into which is stored SRC
which has been scaled by the scalar SCALAR."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale-by-into (pmat) src scalar))

(declaim (ftype (function (pmat double-float) pmat) mscb))
(declaim (inline mscb))
(defun mscb (src scalar) ;; matrix-scale-by
  "Shortname for MATRIX-SCALE-BY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-scale-by src scalar))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) matrix-translate-into))
(declaim (inline matrix-translate-into))
(defun matrix-translate-into (dst pvec)
  "Store a transformation matrix into DST that translates the
coordinate system by the respective PVEC amounts. Return DST. Similar
to glTranslate()"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-identity-into dst)
  (with-pmat-accessors (d dst)
    (with-pvec-accessors (p pvec)
      (psetf d03 px
             d13 py
             d23 pz)))
  dst)

(declaim (ftype (function (pmat pvec) pmat) mtri))
(declaim (inline mtri))
(defun mtri (dst pvec) ;; matrix-translate-into
  "Shortname for MATRIX-TRANSLATE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-into dst pvec))

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
(defun matrix-invert-trfm-into (dst src)
  "Invert (specifically an) orthonormal transformation matrix (with a
rotation operator in the upper left 3x3 matrix and a translation
vector in the 4x1 column on the right) SRC and store into DST. DST can
be EQ with SRC. This means 1) store the transpose the 3x3 rotation
matrix contained in the upper left hand of the transformation matrix,
2) store the application of the inverted rotation to the negation of
the 4x1 translation column.  Return the DST. This function
will not invert arbitrary 4x4 matricies."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (unless (eq dst src)
    (matrix-copy-into dst src))

  (with-pmat-accessors (d dst)
    ;; Transpose the upper left square 3x3 portion of the rotation matrix
    (rotatef d10 d01)
    (rotatef d20 d02)
    (rotatef d21 d12)

    ;; Invert the translation by applying the inverted rotation to the
    ;; negated translation

    (psetf d03 (as-double-float
                (+ (* d00 (- d03)) (* d01 (- d13)) (* d02 (- d23))))
           d13 (as-double-float
                (+ (* d10 (- d03)) (* d11 (- d13)) (* d12 (- d23))))
           d23 (as-double-float
                (+ (* d20 (- d03)) (* d21 (- d13)) (* d22 (- d23))))))

  dst)

(declaim (ftype (function (pmat pmat) pmat) minvti))
(declaim (inline minvti))
(defun minvti (dst src) ;; matrix-invert-transform-into
  "Shortname for MATRIX-INVERT-TRFM-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-trfm-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-invert-trfm))
(defun matrix-invert-trfm (src)
  "Invert the transformation matrix SRC."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-trfm-into (pmat) src))

(declaim (ftype (function (pmat) pmat) minvt))
(declaim (inline minvt))
(defun minvt (src) ;; matrix-invert-transform
  "Shortname for MATRIX-INVERT-TRFM."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-trfm src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: In order to compile this with optimizations on, I need to:
;; (setf *inline-expansion-limit* 1024) somehow only for this function,
;; maybe EVAL-WHEN?
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

      ;; Bail if the determinent is 0 or too close to it.
      (when (< (as-double-float det-s) (as-double-float *pvec-tol*))
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

      (values dst t))))

(declaim (ftype (function (pmat pmat) (values pmat t)) minvi))
(declaim (inline minvi))
(defun minvi (dst src) ;; matrix-invert-into
  "Shortname for MATRIX-INVERT-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) (values pmat t)) matrix-invert))
(defun matrix-invert (src)
  "Allocate a new matrix and place into it the inverse of the arbitrary
4x4 matrix SRC. Return the values of a inverted matrix and T if
the inversion was possible, or an identity matrix and NIL if it wasn't
 possible."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert-into (mi) src))

(declaim (ftype (function (pmat) (values pmat t)) minv))
(declaim (inline minv))
(defun minv (src) ;; matrix-invert
  "Shortname for MATRIX-INVERT."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-invert src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Work in progress.

(declaim (ftype (function (pmat double-float double-float double-float
                                double-float double-float double-float) pmat)
                matrix-perspective-projection-into))
(declaim (inline matrix-perspective-projection-into))
(defun matrix-perspective-projection-into (dst left right bottom top near far)
  "Store a perspective projection matrix into DST described by LEFT, RIGHT,
BOTTOM, TOP, NEAR, and FAR. Similar to glFrustum()."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (d dst)
    (psetf d00 (/ (* 2d0 near) (- right left))
           d10 0d0
           d20 0d0
           d30 0d0

           d01 0d0
           d11 (/ (* 2d0 near) (- top bottom))
           d21 0d0
           d31 0d0

           d02 (/ (+ right left) (- right left))
           d12 (/ (+ top bottom) (- top bottom))
           d22 (- (/ (+ far near) (- far near)))
           d32 -1d0

           d03 0d0
           d13 0d0
           d23 (- (/ (* 2d0 far near) (- far near)))
           d33 0d0)
    dst))

(declaim (ftype (function (pmat double-float double-float double-float
                                double-float double-float double-float) pmat)
                mppi))
(declaim (inline mppi))
(defun mppi (dst left right bottom top near far)
  "Shortname for MATRIX-PERSPECTIVE-PROJECTION-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-perspective-projection-into dst left right bottom top near far))

;;; ;;;;;;;;

(defun matrix-perspective-projection (left right bottom top near far)
  "Allocate and return a perspective projection matrix described by
LEFT, RIGHT, BOTTOM, TOP, NEAR, and FAR. Similar to glFrustum()."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-perspective-projection-into (pmat) left right bottom top near far))

(declaim (ftype (function (double-float double-float double-float double-float
                                        double-float double-float) pmat)
                mpp))
(declaim (inline mpp))
(defun mpp (left right bottom top near far)
  "Shortname for MATRIX-PERSPECTIVE-PROJECTION."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-perspective-projection left right bottom top near far))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat double-float double-float double-float
                                double-float double-float double-float) pmat)
                matrix-orthographic-projection-into))
(declaim (inline matrix-orthographic-projection-into))
(defun matrix-orthographic-projection-into (dst left right bottom top near far)
  "Store an orthographics projection matrix into DST described by the
LEFT, RIGHT, BOTTOM, TOP, NEAR, and FAR arguments. Similar to glOrtho()."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (d dst)
    (psetf d00 (as-double-float (/ 2d0 (- right left)))
           d10 0d0
           d20 0d0
           d30 0d0

           d01 0d0
           d11 (as-double-float (/ 2d0 (- top bottom)))
           d21 0d0
           d31 0d0

           d02 0d0
           d12 0d0
           d22 (as-double-float (- (/ 2d0 (- far near))))
           d32 0d0

           d03 (as-double-float (- (/ (+ right left) (- right left))))
           d13 (as-double-float (- (/ (+ top bottom) (- top bottom))))
           d23 (as-double-float (- (/ (+ far near) (- far near))))
           d33 1d0)
    dst))

(declaim (ftype (function (pmat double-float double-float double-float
                                double-float double-float double-float) pmat)
                mopi))
(declaim (inline mopi))
(defun mopi (dst left right bottom top near far)
  "Shortname for MATRIX-ORTHOGRAPHIC-PROJECTION-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-orthographic-projection-into dst left right bottom top near far))

;;; ;;;;;;;;

(declaim (ftype (function (double-float double-float double-float double-float
                                        double-float double-float) pmat)
                matrix-orthographic-projection))
(declaim (inline matrix-orthographic-projection))
(defun matrix-orthographic-projection (left right bottom top near far)
  "Allocate and return an orthographic projection matrix defined by
LEFT, RIGHT, BOTTOM, TOP, NEAR, and FAR."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-orthographic-projection-into (pmat) left right bottom top near far))

(declaim (ftype (function (double-float double-float double-float double-float
                                        double-float double-float) pmat)
                mop))
(declaim (inline mop))
(defun mop (left right bottom top near far)
  "Shortname for MATRIX-ORTHOGRAPHIC-PROJECTION."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-orthographic-projection left right bottom top near far))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function ((simple-array float (16)) pmat)
                          (simple-array float (16)))
                matrix-convert-to-opengl-into))
(declaim (inline matrix-convert-to-opengl-into))
(defun matrix-convert-to-opengl-into (ogl src)
  "Convert the MAT matrix into OGL, which is a column-major OpenGL
matrix represented as a (simple-array float (16)), and then
return OGL. Precision is lost going from double-float to float here."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array float (16)) ogl))
  (with-pmat-accessors (s src)
    (psetf (aref ogl 0) (float s00 1.0)
           (aref ogl 1) (float s10 1.0)
           (aref ogl 2) (float s20 1.0)
           (aref ogl 3) (float s30 1.0)

           (aref ogl 4) (float s01 1.0)
           (aref ogl 5) (float s11 1.0)
           (aref ogl 6) (float s21 1.0)
           (aref ogl 7) (float s31 1.0)

           (aref ogl 8) (float s02 1.0)
           (aref ogl 9) (float s12 1.0)
           (aref ogl 10) (float s22 1.0)
           (aref ogl 11) (float s32 1.0)

           (aref ogl 12) (float s03 1.0)
           (aref ogl 13) (float s13 1.0)
           (aref ogl 14) (float s23 1.0)
           (aref ogl 15) (float s33 1.0)))
  ogl)

(declaim (ftype (function ((simple-array float (16)) pmat)
                          (simple-array float (16)))
                mctoi))
(declaim (inline mctoi))
(defun mctoi (ogl src) ;; matrix-convert-to-opengl-into
  "Shortname for MATRIX-CONVERT-TO-OPENGL-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-convert-to-opengl-into ogl src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) (simple-array float (16)))
                matrix-convert-to-opengl))
(declaim (inline matrix-convert-to-opengl))
(defun matrix-convert-to-opengl (src)
  "Convert the SRC matrix into newly allocated column-major ordered
simple-array float (16) suitable for OpenGL and return it. Precision is lost
going form double-float to float here."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((ogl (make-array 16 :element-type 'float
                         :initial-element 0.0)))
    (declare (type (simple-array float (16)) ogl))
    (matrix-convert-to-opengl-into ogl src)
    ogl))

(declaim (ftype (function (pmat) (simple-array float (16))) mcto))
(declaim (inline mcto))
(defun mcto (src) ;; matrix-convert-to-opengl
  "Shortname for MATRIX-CONVERT-TO-OPENGL."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-convert-to-opengl src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat (simple-array double-float (16))) pmat)
                matrix-convert-from-opengl-into))
(declaim (inline matrix-convert-from-opengl-into))
(defun matrix-convert-from-opengl-into (dst ogl)
  "Convert the OGL OpenGL matrix, which is a (simple-arry
float (16)) and in column major form, into the DST row major
format and return DST. Precision is lost going from doule-float to float here."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (16)) ogl))
  (with-pmat-accessors (d dst)
    (psetf d00 (aref ogl 0)
           d10 (aref ogl 1)
           d20 (aref ogl 2)
           d30 (aref ogl 3)

           d01 (aref ogl 4)
           d11 (aref ogl 5)
           d21 (aref ogl 6)
           d31 (aref ogl 7)

           d02 (aref ogl 8)
           d12 (aref ogl 9)
           d22 (aref ogl 10)
           d32 (aref ogl 11)

           d03 (aref ogl 12)
           d13 (aref ogl 13)
           d23 (aref ogl 14)
           d33 (aref ogl 15)))
  dst)

(declaim (ftype (function (pmat (simple-array double-float (16))) pmat) mcfoi))
(declaim (inline mcfoi))
(defun mcfoi (dst ogl) ;; matrix-convert-from-opengl-into
  "Shortname for MATRIX-CONVERT-FROM-OPENGL-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-convert-from-opengl-into dst ogl))

;;; ;;;;;;;;

(declaim (ftype (function ((simple-array double-float (16))) pmat)
                matrix-convert-from-opengl))
(declaim (inline matrix-convert-from-opengl))
(defun matrix-convert-from-opengl (ogl)
  "Convert the column-major OGL OpenGL matrix into a newly allocated
row-major MAT format and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array double-float (16)) ogl))
  (matrix-convert-from-opengl-into (pmat) ogl))

(defun mcfo (ogl) ;; matrix-convert-from-opengl
  "Shortname for MATRIX-CONVERT-FROM-OPENGL."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-convert-from-opengl ogl))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) matrix-translate-set-into))
(declaim (inline matrix-translate-set-into))
(defun matrix-translate-set-into (dst trans)
  "Set the translation vector in the pvec TRANS into the translation
column in the transformation matrix DST and return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (d dst)
    (with-pvec-accessors (v trans)
      (psetf d03 vx
             d13 vy
             d23 vz)))
  dst)

(declaim (ftype (function (pmat pvec) pmat) mtrsi))
(declaim (inline mtrsi))
(defun mtrsi (dst trans) ;; matrix-translate-set-into
  "Shortname for MATRIX-TRANSLATE-SET-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-set-into dst trans))

;;; ;;;;;;;;

(declaim (ftype (function (pvec) pmat) matrix-translate-set))
(declaim (inline matrix-translate-set))
(defun matrix-translate-set (trans)
  "Allocate and return a new identity transformation matrix with TRANS
as its translation."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-set-into (matrix-identity) trans))

(declaim (ftype (function (pvec) pmat) mtrs))
(declaim (inline mtrs))
(defun mtrs (trans) ;; matrix-translate-set
  "Shortname for MATRIX-TRANSLATE-SET."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-set trans))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pmat) pvec) matrix-translate-get-into))
(declaim (inline matrix-translate-get-into))
(defun matrix-translate-get-into (trans src)
  "Get the translation column from the transformation matrix SRC and
put into the pvec TRANS. Return TRANS."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (s src)
    (with-pvec-accessors (v trans)
      (psetf vx s03
             vy s13
             vz s23)))
  trans)

(declaim (ftype (function (pvec pmat) pvec) mtrgi))
(declaim (inline mtrgi))
(defun mtrgi (trans src) ;; matrix-translate-get-into
  "Shortname for MATRIX-TRANSLATE-GET-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-get-into trans src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pvec) matrix-translate-get))
(declaim (inline matrix-translate-get))
(defun matrix-translate-get (src)
  "Return the translation column of the transformation matrix SRC as a
newly allocated PVEC and return it."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-get-into (pvec) src))

(declaim (ftype (function (pmat) pvec) mtrg))
(declaim (inline mtrg))
(defun mtrg (src) ;; matrix-translate-get
  "Shortname for MATRIX-TRANSLATE-GET."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-translate-get src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pvec pvec pmat) (values pvec pvec pvec))
                matrix-rotation-vectors-get-into))
(defun matrix-rotation-vectors-get-into (xdir ydir zdir src)
  "Store the raw direction vectors into XDIR, YDIR, and ZDIR, from the
transformation matrix SRC. Return values of XDIR, YDIR, and ZDIR."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (s src)
    (with-multiple-pvec-accessors ((x xdir) (y ydir) (z zdir))
      (psetf xx s00 xy s10 xz s20
             yx s01 yy s11 yz s21
             zx s02 zy s12 zz s22)))
  (values xdir ydir zdir))

(declaim (ftype (function (pvec pvec pvec pmat) (values pvec pvec pvec)) mrvgi))
(defun mrvgi (xdir ydir zdir src) ;; matrix-rotation-vectors-get-into
  "Shortname for MATRIX-ROTATION-VECTORS-GET-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-vectors-get-into xdir ydir zdir src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) (values pvec pvec pvec))
                matrix-rotation-vectors-get))
(defun matrix-rotation-vectors-get (src)
  "Allocate and return three values that record the raw direction
vectors from SRC.  Return values of XDIR, YDIR, and ZDIR."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-vectors-get-into (pvec) (pvec) (pvec) src))

(declaim (ftype (function (pmat) (values pvec pvec pvec)) mrvg))
(defun mrvg (src) ;; matrix-rotation-vectors-get
  "Shortname for MATRIX-ROTATION-VECTORS-GET."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-vectors-get src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pvec pvec pvec) pmat)
                matrix-rotation-vectors-set-into))
(declaim (inline matrix-rotation-vectors-set-into))
(defun matrix-rotation-vectors-set-into (dst xdir ydir zdir)
  "Assign the supplied direction vectors XDIR YDIR ZDIR into the
transformation DST and return DST."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (d dst)
    (with-multiple-pvec-accessors ((x xdir) (y ydir) (z zdir))
      (psetf d00 xx
             d10 xy
             d20 xz

             d01 yx
             d11 yy
             d21 yz

             d02 zx
             d12 zy
             d22 zz)))
  dst)

(declaim (ftype (function (pmat pvec pvec pvec) pmat) mrvsi))
(declaim (inline mrvsi))
(defun mrvsi (dst xdir ydir zdir) ;; matrix-rotation-vectors-set-into
  "Shortname for MATRIX-ROTATION-VECTORS-SET-INTO"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-vectors-set-into dst xdir ydir zdir))

;;; ;;;;;;;;

(declaim (ftype (function (pvec pvec pvec) pmat) matrix-rotation-vectors-set))
(declaim (inline matrix-rotation-vectors-set))
(defun matrix-rotation-vectors-set (xdir ydir zdir)
  "Allocate an identity transformation matrix and then set the raw
axes supplied as XDIR YDIR ZDIR into the rotation components of the
matrix. Return the transformation matrix."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-vectors-set-into (matrix-identity) xdir ydir zdir))

(declaim (ftype (function (pvec pvec pvec) pmat) mrvs))
(declaim (inline mrvs))
(defun mrvs (xdir ydir zdir) ;; matrix-rotation-vectors-set
  "Shortname for MATRIX-ROTATION-VECTORS-SET"
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-vectors-set xdir ydir zdir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pmat keyword) pvec)
                matrix-rotation-direction-get-into))
(defun matrix-rotation-direction-get-into (pv src dir)
  "Select the DIR (one of :X, :Y, or :Z) direction vector from the
rotation submatrix encoded into the transformation matrix SRC and
store into PV. Return PV."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (s src)
    (with-pvec-accessors (p pv)
      (ecase dir
        ((:x)
         (psetf px s00
                py s10
                pz s20))
        ((:y)
         (psetf px s01
                py s11
                pz s21))
        ((:z)
         (psetf px s02
                py s12
                pz s22)))))
  pv)

(declaim (ftype (function (pvec pmat keyword) pvec) mrdgi))
(declaim (inline mrdgi))
(defun mrdgi (pv src dir) ;; matrix-rotation-direction-get-into
  "Shortname for MATIX-ROTATION-DIRECTION-GET-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-direction-get-into pv src dir))

;;; ;;;;;;;;

(declaim (ftype (function (pmat keyword) pvec) matrix-rotation-direction-get))
(declaim (inline matrix-rotation-direction-get))
(defun matrix-rotation-direction-get (src dir)
  "Allocate and return a new pvec into which is stored the DIR (one
of :X, :Y, or :Z) direction vector from the rotation matrix SRC."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-direction-get-into (pvec) src dir))

(declaim (ftype (function (pmat keyword) pvec) mrdg))
(declaim (inline mrdg))
(defun mrdg (src dir) ;; matrix-rotation-direction-get
  "Shortname for MATRIX-ROTATION-DIRECTION-GET."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-direction-get src dir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat keyword pvec) pmat)
                matrix-rotation-direction-set-into))
(defun matrix-rotation-direction-set-into (dst dir pv)
  "Store the direction vector PV into the rotation operation axis specified
by DIR (one of :X, :Y, or :Z) in the transformation matrix dst.
Return dst."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (d dst)
    (with-pvec-accessors (p pv)
      (ecase dir
        ((:x)
         (psetf d00 px
                d10 py
                d20 pz))
        ((:y)
         (psetf d01 px
                d11 py
                d21 pz))
        ((:z)
         (psetf d02 px
                d12 py
                d22 pz)))))
  dst)

(declaim (ftype (function (pmat keyword pvec) pmat) mrdsi))
(declaim (inline mrdsi))
(defun mrdsi (dst dir pv) ;; matrix-rotation-direction-set-into
  "Shortname for MATRIX-ROTATION-DIRECTION-SET-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-direction-set-into dst dir pv))

;;; ;;;;;;;;

(declaim (ftype (function (keyword pvec) pmat) matrix-rotation-direction-set))
(defun matrix-rotation-direction-set (dir pv)
  "Allocate and return a new identity transformation matrix into which
we store the direction vector PV into the rotation operation axis
specified by DIR (one of :X, :Y, or :Z)."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-direction-set-into (matrix-identity) dir pv))

(declaim (ftype (function (keyword pvec) pmat) mrds))
(declaim (inline mrds))
(defun mrds (dir pv) ;; matrix-rotation-direction-set
  "Shortname for MATRIX-ROTATION-DIRECTION-SET."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-rotation-direction-set dir pv))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat pvec) pmat) matrix-fly-into))
(defun matrix-fly-into (dst src fly-vec)
  "Store into DST the transformation matrix of SRC which has been
translated down each basis axis in the rotation operator in SRC by the
respective components of FLY-VEC. So, a FLY-VEC of [0d0 0d0 1d0] would
translate the SRC transformation matrix in the direction of the Z axis
in the basis and store into DST. Return DST. DST may be EQ to SRC."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (unless (eq dst src)
    (matrix-copy-into dst src))

  (with-pvec-accessors (f fly-vec)
    (with-pmat-accessors (d dst)
      (let ((ox d03)
            (oy d13)
            (oz d23))

        ;; Have fly-vec contribute in each basis direction the
        ;; specified movement of the origin for this transformation
        ;; matrix.
        (when (> (as-double-float (abs fx))
                 (as-double-float *pvec-tol*))
          (incf ox (as-double-float (* d00 fx)))
          (incf oy (as-double-float (* d10 fx)))
          (incf oz (as-double-float (* d20 fx))))

        (when (> (as-double-float (abs fy))
                 (as-double-float *pvec-tol*))
          (incf ox (as-double-float (* d01 fy)))
          (incf oy (as-double-float (* d11 fy)))
          (incf oz (as-double-float (* d21 fy))))

        (when (> (as-double-float (abs fz))
                 (as-double-float *pvec-tol*))
          (incf ox (as-double-float (* d02 fz)))
          (incf oy (as-double-float (* d12 fz)))
          (incf oz (as-double-float (* d22 fz))))

        ;; set the updated entries.
        (psetf d03 ox
               d13 oy
               d23 oz)
        dst))))

(declaim (ftype (function (pmat pmat pvec) pmat) mfi))
(declaim (inline mfi))
(defun mfi (dst src fly-vec) ;; matrix-fly-into
  "Shortname for MATRIX-FLY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-fly-into dst src fly-vec))

;;; ;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat) matrix-fly))
(declaim (inline matrix-fly))
(defun matrix-fly (src fly-vec)
  "Copy the transformation matrix SRC and add each component of the
FLY-VEC to the translation portion of the transformation matrix SRC in
the direction of the respective basis direction in the rotation
submatrix. Return the copy."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-fly-into (pmat) src fly-vec))

(declaim (ftype (function (pmat pvec) pmat) mf))
(declaim (inline mf))
(defun mf (src fly-vec) ;; matrix-fly
  "Shortname for MATRIX-FLY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-fly src fly-vec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pvec pmat pvec) pvec) matrix-apply-into))
(declaim (inline matrix-apply-into))
(defun matrix-apply-into (transformed-point mat point)
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
  "Shortname for MATRIX-APPLY-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-apply-into transformed-point mat point))

;;; ;;;;;;;;

(declaim (ftype (function (pmat pvec) pvec) matrix-apply))
(declaim (inline matrix-apply))
(defun matrix-apply (mat point)
  "Multiply the transformation matrix MAT against the POINT and return
the transformed point in a newly allocated pvec."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-apply-into (pvec) mat point))

(declaim (ftype (function (pmat pvec) pvec) ma))
(declaim (inline ma))
(defun ma (mat point) ;; matrix-apply
  "Shortname for NATRIX-APPLY."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-apply mat point))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat) pmat) matrix-copy-rotation-into))
(defun matrix-copy-rotation-into (dst src)
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
  "Shortname for MATRIX-COPY-ROTATION-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-rotation-into dst src))

;;; ;;;;;;;;

(declaim (ftype (function (pmat) pmat) matrix-copy-rotation))
(declaim (inline matrix-copy-rotation))
(defun matrix-copy-rotation (src)
  "Allocate and return an identity transformation matrix into which
just the 3X3 rotation submatrix of the SRC transformation matrix has
been stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-rotation-into (matrix-identity) src))

(declaim (ftype (function (pmat) pmat) mcr))
(declaim (inline mcr))
(defun mcr (src) ;; matrix-copy-rotation
  "Shortname for MATRIX-COPY-ROTATION."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-copy-rotation src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pmat pmat pvec) pmat)
                matrix-local-axis-rotate-into))
(defun matrix-local-axis-rotate-into
    (result trfm rotation-vec)
  "Store into RESULT a rotation of the transformation matrix TRFM as a
relative rotation (AKA the local axis rotation) as specified by the
ROTATION-VEC. The components of the ROTATION-VEC define the relative
rotation in radians around each axis in the rotation submatrix. Return RESULT.
 RESULT may be EQ to TRFM"

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (let ((rot (matrix-identity)))
    ;; We incrementally rotate the result matrix we initialize with trfm
    ;; by each direction in the rotation-vec. We then return the result.
    (matrix-copy-into result trfm)

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
                        (let ((,sin-sym (as-double-float (sin ,axis)))
                              (,cos-sym (as-double-float (cos ,axis))))
                          ,@body)
                        (matrix-multiply-into result result rot))))

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
  result)

(declaim (ftype (function (pmat pmat pvec) pmat) mlari))
(declaim (inline mlari))
(defun mlari (result trfm rotation-vec) ;; matrix-local-axis-rotate-into
  "Shortname for MATRIX-LOCAL-AXIS-ROTATE-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-local-axis-rotate-into result trfm rotation-vec))

;;; ;;;;;;;;

(declaim (ftype (function (pmat pvec) pmat)
                matrix-local-axis-rotate))
(declaim (inline matrix-local-axis-rotate))
(defun matrix-local-axis-rotate (trfm rotation-vec)
  "Return a newly allocated transformation matrix which was the
relative rotation (AKA the local axis rotation) as specified by the
ROTATION-VEC as applied to TRFM. The ROTATION-VEC defines the relative
rotation around each axis in the rotation submatrix (note, it does not
specify a vector around which a rotation happens)."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-local-axis-rotate-into (matrix-identity) trfm rotation-vec))

(declaim (ftype (function (pmat pvec) pmat) mlar))
(declaim (inline mlar))
(defun mlar (trfm rotation-vec) ;; matrix-local-axis-rotate
  "Shortname for MATRIX-LOCAL-AXIS-ROTATE."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-local-axis-rotate trfm rotation-vec))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX This might be wrong, check very carefully.
(declaim (ftype (function (pmat pmat keyword keyword) pmat)
                matrix-create-view-into))
(defun matrix-create-view-into (view camera at-dir up-dir)
  "Compute a view transformation matrix from a CAMERA
world-transformation matrix and store it in VIEW. AT-DIR represents
the :X, :Y, or :Z axis in the CAMERA rotation submatrix that will be
used as the 'look at' direction. UP-DIR represents the :X, :Y, or :Z
axis that will be used as the 'up' direction in the rotation
submatrix. Do not use the same vector for both AT-DIR and UP-DIR."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (xv yv zv) (matrix-rotation-vectors-get camera)
    (let* ((at (ecase at-dir ((:x) xv) ((:y) yv) ((:z) zv)))
           (up (ecase up-dir ((:x) xv) ((:y) yv) ((:z) zv)))
           ;; Create the u v n of the new view matrix
           (u (vnormalize (vcross up at)))
           (n (vnormalize at))
           (v (vcross n u))
           ;; create an inverted camera rotation
           (inv-rot (matrix-invert-trfm (matrix-rotation-vectors-set u v n)))
           ;; create an inverted translation matrix
           (trans (matrix-translate-get camera))
           (inv-trans (matrix-translate (vnegi trans trans))))
      ;; Create the actual inverted view matrix.
      (matrix-multiply-into view inv-rot inv-trans))))

(defun mcvi (view camera at-dir up-dir)
  "Shortname for MATRIX-CREATE-VIEW-INTO."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-create-view-into view camera at-dir up-dir))

;;; ;;;;;;;;

(declaim (ftype (function (pmat keyword keyword) pmat) matrix-create-view))
(defun matrix-create-view (camera at-dir up-dir)
  "Allocate and return a transformation matrix into which the inverted view of
the camera is stored."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-create-view-into (matrix-identity) camera at-dir up-dir))

(defun mcv (camera at-dir up-dir)
  "Shortname for MATRIX-CREATE-VIEW."
  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  (matrix-create-view camera at-dir up-dir))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX This might be wrong, check very carefully.
(defun matrix-plane-eqn (mat plane &key (multiple-value :t))
  "Compute the double-float A,B,C,D coeffs for the plane specified in
the transformation matrix MAT, one of :XY, :XZ, or :YZ, and which
passes through the translation point in the basis and return them in
that order as multiple values if the keyword :MULTIPLE-VALUE is T (the
default) or as a list."

  #+option-9-optimize-pmat (declare (optimize (speed 3) (safety 0)))
  #+(and :option-9-optimize-pvec :sbcl)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((p0 (matrix-translate-get mat))
         ;; All axes are chosen such that their cross leads to a
         ;; vector pointing in the positive direction of the third
         ;; axis.
         (choice-a (ecase plane ((:xy) :x) ((:xz) :z) ((:yz) :y)))
         (choice-b (ecase plane ((:xy) :y) ((:xz) :x) ((:yz) :z)))
         (axis-a (matrix-rotation-direction-get mat choice-a))
         (axis-b (matrix-rotation-direction-get mat choice-b))
         ;; These are actual points.
         (p1 (vadd p0 axis-a))
         (p2 (vadd p0 axis-b))
         ;; Compute the vector we'll be crossing.
         (vec-a (vvect p0 p1))
         (vec-b (vvect p0 p2))
         (norm (vcross vec-a vec-b))
         (d (vdot norm p0)))
    (with-pvec-accessors (n norm)
      (if multiple-value
          (values nx ny nz d)
          (list nx ny nz d)))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX This might be wrong, check very carefully.
;; TODO, spruce this up to emit quadrants too, eventually.
(declaim (ftype (function (pmat pvec keyword) cons) pm-classify-point))
(defun matrix-classify-point (basis point plane)
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
    (multiple-value-bind (a b c d) (matrix-plane-eqn basis plane)
      (let ((side (as-double-float (- (+ (* a px) (* b py) (* c pz)) d))))
        ;; "FRONT" is considered down the positive Z axis when :XY
        ;; "LEFT" is considered down the positive X axis when :YZ
        ;; "ABOVE" is considered down the positive Y axis when :XZ
        (cond
          ((< (as-double-float (abs side))
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
