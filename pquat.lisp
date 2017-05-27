(in-package :option-9)

#+(or (not option-9-optimize-pvec) option-9-debug)
(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An implementation of quaternions.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *quaternion-slerp-cutoff* (cos (* 5d0 (/ pi 180d0)))) ;; 5 degrees

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype pquat () `(simple-array double-float (4)))
  (defstruct (pquat
               (:type (vector double-float))
               (:constructor make-pquat)
               (:constructor pquat (&optional w x y z)))

    (w 1.0d0 :type double-float)
    (x 0.0d0 :type double-float)
    (y 0.0d0 :type double-float)
    (z 0.0d0 :type double-float))

  (defmacro with-pquat-accessors ((prefix-symbol pquat) &body body)
    `(with-accessors
           ((,(make-accessor-symbol prefix-symbol "W") pquat-w)
            (,(make-accessor-symbol prefix-symbol "X") pquat-x)
            (,(make-accessor-symbol prefix-symbol "Y") pquat-y)
            (,(make-accessor-symbol prefix-symbol "Z") pquat-z))
         ,pquat
       ,@body))


  (defmacro with-multiple-pquat-accessors (sbinds &body body)
    (if (null sbinds)
        `(progn ,@body)
        `(with-pquat-accessors ,(car sbinds)
           (with-multiple-pquat-accessors ,(cdr sbinds) ,@body))))

  ;; NOTE: I must use the pprint-dispatch table to emit nicely formatted
  ;; pvecs because they aren't a CLASS due to the defstruct definition
  ;; I am using. So PRINT-OBJECT doesn't work on PVEC types.
  (set-pprint-dispatch
   'pquat #'(lambda (str pobj)
              (with-pquat-accessors (p pobj)
                (print-unreadable-object (pobj str)
                  (format str "[~A + ~Ai + ~Aj + ~Ak]" pw px py pz))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pquat pquat) pquat) quat-copy-into))
(declaim (inline quat-copy-into))
(defun quat-copy-into (dst src)
  "Copy the SRC quaternion into the DST quaternion."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pquat-accessors ((d dst) (s src))
    (psetf dw sw
           dx sx
           dy sy
           dz sz))
  dst)

(defun quat-copy (src)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-copy-into (pquat) src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-add-into (dst qa qb)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pquat-accessors ((d dst) (a qa) (b qb))
    (psetf dw (as-double-float (+ aw bw))
           dx (as-double-float (+ ax bx))
           dy (as-double-float (+ ay by))
           dz (as-double-float (+ az bz))))
  dst)

(defun quat-add (qa qb)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-add-into (pquat) qa qb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-mul-into (dst qa qb)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pquat-accessors ((d dst) (a qa) (b qb))
    (psetf
     dw (as-double-float (- (* aw bw) (* ax bx) (* ay by) (* az bz)))
     dx (as-double-float (- (+ (* aw bx) (* ax bw) (* ay bz)) (* az by)))
     dy (as-double-float (- (+ (* aw by) (* ay bw) (* az bx)) (* ax bz)))
     dz (as-double-float (- (+ (* aw bz) (* az bw) (* ax by)) (* ay bx)))))
  dst)

(defun quat-mul (qa qb)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-mul-into (pquat) qa qb))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-scale-into (dst quat scale-factor)
  (with-multiple-pquat-accessors ((d dst) (q quat))
    (psetf dw (as-double-float (* qw scale-factor))
           dx (as-double-float (* qx scale-factor))
           dy (as-double-float (* qy scale-factor))
           dz (as-double-float (* qz scale-factor))))
  dst)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-conjugate-into (dst quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pquat-accessors ((d dst) (q quat))
    (psetf dw qw
           dx (as-double-float (- qx))
           dy (as-double-float (- qy))
           dz (as-double-float (- qz))))
  dst)

(defun quat-conjugate (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-conjugate-into (pquat) quat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-norm (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pquat-accessors (q quat)
    (as-double-float (sqrt (+ (* qw qw) (* qx qx) (* qy qy) (* qz qz))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-normalize-into (dst quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pquat-accessors ((d dst) (q quat))
    (let ((norm (quat-norm quat)))
      (psetf dw (as-double-float (/ qw norm))
             dx (as-double-float (/ qx norm))
             dy (as-double-float (/ qy norm))
             dz (as-double-float (/ qz norm)))))
  dst)

(defun quat-normalize (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-normalize-into (pquat) quat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-inverse-into (dst quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (let* ((norm (quat-norm quat))
         (norm2 (as-double-float (* norm norm))))

    ;; inverse into DST and then we'll finish by dividing it by norm2.
    (quat-conjugate-into dst quat)

    (with-pquat-accessors (d dst)
      (psetf dw (as-double-float (/ dw norm2))
             dx (as-double-float (/ dx norm2))
             dy (as-double-float (/ dy norm2))
             dz (as-double-float (/ dz norm2)))))
  dst)

(defun quat-inverse (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-inverse-into (pquat) quat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-pure-p (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pquat-accessors (q quat)
    (= qw 0d0)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-create-pure-into (quat point)
  "Construct a pure quaternion from a pvec POINT into the pquat DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pquat-accessors (q quat)
    (with-pvec-accessors (p point)
      (psetf qw 0d0
             qx px
             qy py
             qz pz)))
  quat)

(defun quat-create-pure (point)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-create-pure-into (pquat) point))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-extract-pure-into (point quat)
  "Extract the imaginary portion of the pquat SRC into the pvec DST."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pvec-accessors (p point)
    (with-pquat-accessors (q quat)
      (psetf px qx
             py qy
             pz qz)))
  point)

(defun quat-extract-pure (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-extract-pure-into (pvec) quat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-rotate-around-into (quat angle-in-radians axis-of-rotation)
  "Create a quaternion into QUAT that represents a rotation of
ANGLE-IN-RADIANS around the arbitrary vector AXIS-OF-ROTATION. Return QUAT."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (let ((normalized-axis-of-rotation (vnormalize axis-of-rotation))
        (sin-of-half-angle (as-double-float (sin (/ angle-in-radians 2d0)))))
    (declare (type double-float sin-of-half-angle))
    (with-pvec-accessors (a normalized-axis-of-rotation)
      (with-pquat-accessors (q quat)
        (psetf qw (as-double-float (cos (/ angle-in-radians 2d0)))
               qx (as-double-float (* ax sin-of-half-angle))
               qy (as-double-float (* ay sin-of-half-angle))
               qz (as-double-float (* az sin-of-half-angle))))))
  quat)

(defun quat-rotate-around (angle-in-radians axis-of-rotation)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-rotate-around-into (pquat) angle-in-radians axis-of-rotation))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quat-apply-into (dst-point point qrot)
  "Rotate a pvec POINT by the rotation stored in QROT and store the rotated
point in the pvec DST-POINT."
  (quat-extract-pure-into dst-point
                          (quat-mul qrot
                                    (quat-mul (quat-create-pure point)
                                              (quat-inverse qrot)))))

(defun quat-apply (point qrot)
  (quat-apply-into (pvec) point qrot))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implemented from "Advanced Animation and Rendering Techniques" by Watt et al.
;; page 363.
;;
;; NOTE: The book has a row-major ordering of the matrix, but we use
;; column major in this code, so I've manually transposed it.
;;
;; TODO: Check when s is 0. Not all quaternions are convertible to matricies.
(defun quat-qtom-into (mat quat) ;; seems to work now.
  "Converts a unit quaternion to a matrix representation."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pmat-accessors (m mat)
    (with-pquat-accessors (q quat)
      (let* ((s (as-double-float
                 (/ 2d0 (+ (* qw qw) (* qx qx) (* qy qy) (* qz qz)))))
             (xs (as-double-float (* qx s)))
             (ys (as-double-float (* qy s)))
             (zs (as-double-float (* qz s)))
             (xx (as-double-float (* qx xs)))
             (xy (as-double-float (* qx ys)))
             (xz (as-double-float (* qx zs)))
             (yy (as-double-float (* qy ys)))
             (yz (as-double-float (* qy zs)))
             (zz (as-double-float (* qz zs)))
             (wx (as-double-float (* qw xs)))
             (wy (as-double-float (* qw ys)))
             (wz (as-double-float (* qw zs))))
        (psetf m00 (as-double-float (- 1d0 (+ yy zz)))
               m10 (as-double-float (+ xy wz))
               m20 (as-double-float (- xz wy))
               m30 0d0

               m01 (as-double-float (- xy wz))
               m11 (as-double-float (- 1d0 (+ xx zz)))
               m21 (as-double-float (+ yz wx))
               m31 0d0

               m02 (as-double-float (+ xz wy))
               m12 (as-double-float (- yz wx))
               m22 (as-double-float (- 1d0 (+ xx yy)))
               m32 0d0

               m03 0d0
               m13 0d0
               m23 0d0
               m33 1d0))))
  mat)

(defun quat-qtom (quat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-qtom-into (pmat) quat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gotten from
;; http://www.gamasutra.com/view/feature/131686/rotating_objects_using_quaternions.php
;; which looks like it was gotten from "Advanced Animation and
;; Rendering Techniques" by Watt et al page 363-364
;;
;; NOTE: I fixed this to be column major ordering.

;; TODO: Fix optimization. PROGN is supposed to be AS-DOUBLE-FLOAT but it it too
;; dumb to notice the function calls.
(defun quat-mtoq-into (quat mat)
  "Converts the orthonormal rotation portion of pmat MAT into pquat QUAT."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-pquat-accessors (q quat)
    (with-pmat-accessors (m mat)
      (let ((nxt #(1 2 0))
            (tr (+ m00 m11 m22))
            (tmpq
             ;; a quat represented as x,y,z,w in this algorithm
             (make-array 4 :element-type 'double-float :initial-element 0d0))
            (s 0d0))
        (cond
          ;; check the trace, if positive we're numerically stable
          ((> tr 0d0)
           (setf s (as-double-float (sqrt (+ tr 1d0)))
                 qw (as-double-float (/ s 2d0))
                 s (as-double-float (/ .5d0 s))
                 qx (as-double-float (* (- m21 m12) s))
                 qy (as-double-float (* (- m02 m20) s))
                 qz (as-double-float (* (- m10 m01) s))))
          ;; trace is negative, determine best means to compute quaternion.
          (t
           (let ((i 0))
             (when (> m11 m00)
               (setf i 1))
             (when (> m22 (pmat-aref mat i i))
               (setf i 2))

             (let* ((j (aref nxt i))
                    (k (aref nxt j))
                    (s (progn
                         (sqrt (+ 1d0 (- (pmat-aref mat i i)
                                         (+ (pmat-aref mat j j)
                                            (pmat-aref mat k k))))))))
               (setf (aref tmpq i) (as-double-float (* s 0.5d0)))
               (unless (= s 0d0) ;; TODO bad floating point check
                 (setf s (as-double-float (/ .5d0 s))))
               (setf (aref tmpq 3) (progn
                                     (* (- (pmat-aref mat k j)
                                           (pmat-aref mat j k)) s))
                     (aref tmpq j) (progn
                                     (* (+ (pmat-aref mat j i)
                                           (pmat-aref mat i j)) s))
                     (aref tmpq k) (progn
                                     (* (+ (pmat-aref mat k i)
                                           (pmat-aref mat i k)) s)))

               (psetf qw (aref tmpq 3)
                      qx (aref tmpq 0)
                      qy (aref tmpq 1)
                      qz (aref tmpq 2)))))))))
  quat)

(defun quat-mtoq (mat)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-mtoq-into (pquat) mat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Assumes quaternions are normalized
(defun quat-slerp-into (quat-result quat-src quat-dst interp)
  "Compute the SLERP from QUAT-SRC to QUAT-DST by INTERP [0d0 to 1d0] and
store the resultant quaternion into QUAT-RESULT. Return QUAT-RESULT."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (let ((dsttmp (pquat)))
    (with-multiple-pquat-accessors ((r quat-result) (s quat-src) (d quat-dst)
                                    (dt dsttmp))
      (let ((cosom (as-double-float
                    (+ (* sx dx) (* sy dy) (* sz dz) (* sw dw)))))
        ;; adjust signs of necessary
        (cond
          ((< cosom 0d0)
           (psetf cosom (as-double-float (- cosom))
                  dtw (as-double-float (- dw))
                  dtx (as-double-float (- dx))
                  dty (as-double-float (- dy))
                  dtz (as-double-float (- dz))))
          (t
           (psetf dtw (as-double-float dw)
                  dtx (as-double-float dx)
                  dty (as-double-float dy)
                  dtz (as-double-float dz))))

        ;; calculate coefficients
        (multiple-value-bind (scale0 scale1)
            (cond
              ((< cosom *quaternion-slerp-cutoff*)
               (let* ((omega (as-double-float (acos cosom)))
                      (sinom (as-double-float (sin omega))))
                 (values
                  (as-double-float (/ (sin (* (- 1d0 interp) omega)) sinom))
                  (as-double-float (/ (sin (* interp omega)) sinom)))))
              (t
               (values
                (as-double-float (- 1d0 interp))
                (as-double-float interp))))

          ;; calculate final values
          (psetf rw (as-double-float (+ (* scale0 sw) (* scale1 dtw)))
                 rx (as-double-float (+ (* scale0 sx) (* scale1 dtx)))
                 ry (as-double-float (+ (* scale0 sy) (* scale1 dty)))
                 rz (as-double-float (+ (* scale0 sz) (* scale1 dtz))))))))
  quat-result)

(defun quat-slerp (quat-src quat-dst interp)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (quat-slerp-into (pquat) quat-src quat-dst interp))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
