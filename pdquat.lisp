(in-package :option-9)

#+(or (not option-9-optimize-pvec) option-9-debug)
(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An implementation of dual quaternions.
;;; The reason I chose not to use two quaternions in a structure and instead
;;; reimplement this stuff as a new structure with new math operations is that
;;; I can more reliably believe how much space this stuff takes while
;;; making it easier to pass to opengl shaders.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype pdquat () `(simple-array double-float (8)))
  (defstruct (pdquat
               (:type (vector double-float))
               (:constructor make-pdquat)
               (:constructor pdquat (&optional rw rx ry rz dw dx dy dz)))

    (rw 1d0 :type double-float)
    (rx 0d0 :type double-float)
    (ry 0d0 :type double-float)
    (rz 0d0 :type double-float)
    (dw 0d0 :type double-float)
    (dx 0d0 :type double-float)
    (dy 0d0 :type double-float)
    (dz 0d0 :type double-float))

  (defmacro with-pdquat-accessors ((prefix-symbol pdquat) &body body)
    `(with-accessors
           ((,(alexandria:symbolicate prefix-symbol "RW") pdquat-rw)
            (,(alexandria:symbolicate prefix-symbol "RX") pdquat-rx)
            (,(alexandria:symbolicate prefix-symbol "RY") pdquat-ry)
            (,(alexandria:symbolicate prefix-symbol "RZ") pdquat-rz)
            (,(alexandria:symbolicate prefix-symbol "DW") pdquat-dw)
            (,(alexandria:symbolicate prefix-symbol "DX") pdquat-dx)
            (,(alexandria:symbolicate prefix-symbol "DY") pdquat-dy)
            (,(alexandria:symbolicate prefix-symbol "DZ") pdquat-dz))
         ,pdquat
       ,@body))


  (defmacro with-multiple-pdquat-accessors (sbinds &body body)
    (if (null sbinds)
        `(progn ,@body)
        `(with-pdquat-accessors ,(car sbinds)
           (with-multiple-pdquat-accessors ,(cdr sbinds) ,@body))))

  ;; NOTE: I must use the pprint-dispatch table to emit nicely formatted
  ;; pvecs because they aren't a CLASS due to the defstruct definition
  ;; I am using. So PRINT-OBJECT doesn't work on PVEC types.
  (set-pprint-dispatch
   'pdquat #'(lambda (str pobj)
               (with-pdquat-accessors (p pobj)
                 (print-unreadable-object (pobj str)
                   (format str
                           "[~A + ~Ai + ~Aj + ~Ak] + [~A + ~Ai + ~Aj + ~Ak]e"
                           prw prx pry prz pdw pdx pdy pdz))))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (pdquat pdquat) pdquat) dquat-copy-into))
(declaim (inline dquat-copy-into))
(defun dquat-copy-into (dst src)
  "Copy the SRC dual quaternion into the DST dual quaternion."
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  (with-multiple-pdquat-accessors ((d dst) (s src))
    (psetf
     ;; real part
     drw (as-double-float srw)
     drx (as-double-float srx)
     dry (as-double-float sry)
     drz (as-double-float srz)
     ;; dual part
     ddw (as-double-float sdw)
     ddx (as-double-float sdx)
     ddy (as-double-float sdy)
     ddz (as-double-float sdz)))
  dst)

(defun dquat-copy (src)
  #+option-9-optimize-pvec (declare (optimize (speed 3) (safety 0)))
  "Shortname for DQUAT-COPY-INTO."
  (dquat-copy-into (pdquat) src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-scale-into (dst src scale-factor)
  (with-multiple-pdquat-accessors ((d dst) (s src))
    (psetf
     ;; real part
     drw (as-double-float (* srw scale-factor))
     drx (as-double-float (* srx scale-factor))
     dry (as-double-float (* sry scale-factor))
     drz (as-double-float (* srz scale-factor))
     ;; dual part
     ddw (as-double-float (* sdw scale-factor))
     ddx (as-double-float (* sdx scale-factor))
     ddy (as-double-float (* sdy scale-factor))
     ddz (as-double-float (* sdz scale-factor))))
  dst)

(defun dquat-scale (src scale-factor)
  (dquat-scale-into (pdquat) src scale-factor))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-add-into (dst dquat-a dquat-b)
  (with-multiple-pdquat-accessors ((d dst) (a dquat-a) (b dquat-b))
    (psetf
     ;; real part
     drw (as-double-float (+ arw brw))
     drx (as-double-float (+ arx brx))
     dry (as-double-float (+ ary bry))
     drz (as-double-float (+ arz brz))
     ;; dual part
     ddw (as-double-float (+ adw bdw))
     ddx (as-double-float (+ adx bdx))
     ddy (as-double-float (+ ady bdy))
     ddz (as-double-float (+ adz bdz))))
  dst)

(defun dquat-add (dquat-a dquat-b)
  (dquat-add-into (pdquat) dquat-a dquat-b))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: I think this is wrong, verify in octave.
(defun dquat-mul-into (dst dquat-a dquat-b)
  (with-multiple-pdquat-accessors ((d dst) (a dquat-a) (b dquat-b))
    ;; Slow and uses memory.

    (let* ((qar (pquat arw arx ary arz))
           (qad (pquat adw adx ady adz))
           (qbr (pquat brw brx bry brz))
           (qbd (pquat bdw bdx bdy bdz))

           (qdr (quat-mul qar qbr))
           (qdd (quat-add (quat-mul qar qbd)
                          (quat-mul qad qbr))))
      (with-multiple-pquat-accessors ((qdr- qdr) (qdd- qdd))
        (psetf
         drw (as-double-float qdr-w)
         drx (as-double-float qdr-x)
         dry (as-double-float qdr-y)
         drz (as-double-float qdr-z)
         ddw (as-double-float qdd-w)
         ddx (as-double-float qdd-x)
         ddy (as-double-float qdd-y)
         ddz (as-double-float qdd-z)))))
  dst)

(defun dquat-mul (dquat-a dquat-b)
  (dquat-mul-into (pdquat) dquat-a dquat-b))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-conjugate-quaternion-into (dst src)
  "Q* = Q0* + Q1*e"
  (with-multiple-pdquat-accessors ((d dst) (s src))
    (psetf
     drw (as-double-float srw)
     drx (as-double-float (- srx))
     dry (as-double-float (- sry))
     drz (as-double-float (- srz))

     ddw (as-double-float sdw)
     ddx (as-double-float (- sdx))
     ddy (as-double-float (- sdy))
     ddz (as-double-float (- sdz))))
  dst)

(defun dquat-conjugate-quaternion (src)
  (dquat-conjugate-quaternion-into (pdquat) src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-conjugate-dual-into (dst src)
  "Qbar = Q0 - Q1e"
  (with-multiple-pdquat-accessors ((d dst) (s src))
    (psetf
     drw (as-double-float srw)
     drx (as-double-float srx)
     dry (as-double-float sry)
     drz (as-double-float srz)

     ddw (as-double-float (- sdw))
     ddx (as-double-float (- sdx))
     ddy (as-double-float (- sdy))
     ddz (as-double-float (- sdz))))
  dst)

(defun dquat-conjugate-dual (src)
  (dquat-conjugate-dual-into (pdquat) src))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-conjugate-dual-quaternion-into (dst src)
  "Qbar* = Q0* - Q1*e"
  (with-multiple-pdquat-accessors ((d dst) (s src))
    (psetf
     drw (as-double-float srw)
     drx (as-double-float (- srx))
     dry (as-double-float (- sry))
     drz (as-double-float (- srz))

     ddw (as-double-float (- sdw))
     ddx (as-double-float sdx)
     ddy (as-double-float sdy)
     ddz (as-double-float sdz)))
  dst)

(defun dquat-conjugate-dual-quaternion (src)
  (dquat-conjugate-dual-quaternion-into (pdquat) src))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This must return a dual number as a values.
(defun dquat-norm (src)
  (declare (ignore src))
  ;; TODO
  nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-pure-rotation-into (dst angle axis)
  (let ((naxis (vect-normalize axis))
        (cos-term (as-double-float (cos (/ angle 2d0))))
        (sin-term (as-double-float (sin (/ angle 2d0)))))
    (with-pvec-accessors (n naxis)
      (with-pdquat-accessors (d dst)
        (psetf
         drw (as-double-float cos-term)
         drx (as-double-float (* nx sin-term))
         dry (as-double-float (* ny sin-term))
         drz (as-double-float (* nz sin-term))
         ddw 0d0
         ddx 0d0
         ddy 0d0
         ddz 0d0))))
  dst)

(defun dquat-pure-rotation (angle axis)
  (dquat-pure-rotation-into (pdquat) angle axis))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-pure-translation-into (dst translation)
  (with-pvec-accessors (tv translation)
    (with-pdquat-accessors (d dst)
      (psetf
       drw 1d0
       drx 0d0
       dry 0d0
       drz 0d0
       ddw 0d0
       ddx (as-double-float (/ tvx 2d0))
       ddy (as-double-float (/ tvy 2d0))
       ddz (as-double-float (/ tvz 2d0)))))
  dst)

(defun dquat-pure-translation (translation)
  (dquat-pure-translation-into (pdquat) translation))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-create-point-into (dst point)
  "Convert the pvec 3d POINT into a dual quaternion. Note, this cannot be used
as a translation, use dquat-pure-translation for that."
  (with-pvec-accessors (p point)
    (with-pdquat-accessors (d dst)
      (psetf drw 1d0
             drx 0d0
             dry 0d0
             drz 0d0
             ddw 0d0
             ddx (as-double-float px)
             ddy (as-double-float py)
             ddz (as-double-float pz))))
  dst)

(defun dquat-create-point (point)
  (dquat-create-point-into (pdquat) point))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dquat-extract-point-into (point src-dquat)
  "Extract the 3D point represented in the SRC-DQUAT and store into the pvec
POINT."
  (with-pvec-accessors (p point)
    (with-pdquat-accessors (s src-dquat)
      (psetf px (as-double-float sdx)
             py (as-double-float sdy)
             pz (as-double-float sdz))))
  point)

(defun dquat-extract-point (src-dquat)
  (dquat-extract-point-into (pvec) src-dquat))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Figure out more detail about how the inverse and conjugate relate
;; in dual quaternion theory. They relate over normalization like in quaternions
;; but I don't know what normalized dual quaternions mean in terms of rigid
;; mody representation.
(defun dquat-apply-into (dst-point transform src-point)
  "Apply a dual quaternion TRANSFORM to the 3D SRC-POINT and put the
result into DST-POST."
  (let ((dquat-result
         (dquat-mul transform
                    (dquat-mul (dquat-create-point src-point)
                               (dquat-conjugate-dual-quaternion transform)))))
    (dquat-extract-point-into dst-point dquat-result)))

(defun dquat-apply (transform src-point)
  (dquat-apply-into (pvec) transform src-point))
