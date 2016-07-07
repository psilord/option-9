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
                   (format str "[~A + ~Ai + ~Aj + ~Ak]:[~A + ~Ai + ~Aj + ~Ak]"
                           prw prx pry prz pdw pdx pdy pdz))))))
