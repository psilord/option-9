(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun lerp (a b interp &key truncp)
  "Perform a mathematically stable linear interpolation from A to B by INTERP.
The keyword argument TRUNCP indicates if the result should be TRUNCATed or not."
  (let ((result (+ (* (- 1.0 interp) a) (* interp b))))
    (if truncp (truncate result) result)))

(defmethod distance ((a frame) (b frame) &key (sqrt t))
  (let ((ao (matrix-translate-get (world-basis a)))
        (bo (matrix-translate-get (world-basis b))))
    (vdist ao bo :sqrt sqrt)))

(defun random-sign ()
  (if (zerop (random 2)) 1d0 -1d0))

(defun random-delta (&key (velocity .02d0))
  (* (random velocity) (random-sign)))

(defun random-in-range (start end)
  "Produce a random double-float in the range specified."
  ;; I wish I didn't have to have a branch here...
  (let ((range (abs (- end start))))
    (if (zerop range)
        0d0
        (+ start (random (coerce (abs (- end start)) 'double-float))))))

(defun clamp (val minimum maximum)
  "Clamp VAL into the range MINIMUM and MAXIMUM."
  (cond
    ((< val minimum)
     minimum)
    ((> val maximum)
     maximum)
    (t
     val)))

;; From LOL
(defun group (lst n)
  (when (zerop n) (error "A zero group size is illegal"))
  (labels ((rec (lst acc)
             (let ((rst (nthcdr n lst)))
               (if (consp rst)
                   (rec rst (cons (subseq lst 0 n)
                                  acc))
                   (nreverse
                    (cons lst acc))))))
    (if lst (rec lst nil) nil)))

;; Not all of this is used, it is here because it is the full
;; utility macro sketch of hash table utilities.
;; Help with making initialized hash tables.
(defun miht-set-key/value (key value ht)
  `(setf (gethash ,key ,ht) ,value))

(defun miht-make-hash-table (&rest args)
  (if (equal args '(nil))
      `(make-hash-table)
      `(make-hash-table ,@args)))

;; use like:
;; (make-initialized-hash-table (:test #'equal) :a 0 :b 1 :c 2 ....)
;; The init-forms or keys/values can be empty if desired.
(defmacro make-initialized-hash-table ((&rest init-form) &body keys/values)
  (let ((h (gensym)))
    `(let ((,h ,(apply #'miht-make-hash-table init-form)))
       ,@(mapcar #'(lambda (key/value)
                     (when (= (length key/value) 1)
                       (error "make-initalized-hash-table: Please supply a value for key ~S"
                              (car key/value)))
                     (destructuring-bind (key value) key/value
                       (miht-set-key/value key value h)))
                 (group keys/values 2))
       ,h)))

;; Shorter helper macros for more brevity
(defmacro miht ((&rest init-form) &body keys/values)
  `(make-initialized-hash-table (,@init-form) ,@keys/values))

;; Really short macros for common cases.
(defmacro mihteq (&body keys/values)
  `(make-initialized-hash-table (:test #'eq) ,@keys/values))

(defmacro mihteql (&body keys/values)
  `(make-initialized-hash-table (:test #'eql) ,@keys/values))

(defmacro mihtequal (&body keys/values)
  `(make-initialized-hash-table (:test #'equal) ,@keys/values))

(defmacro mihtequalp (&body keys/values)
  `(make-initialized-hash-table (:test #'equalp) ,@keys/values))

;; TODO: This math utility needs a better home and memory optimization.
(defun dist-line-point (s e p)
  "Find the shortest distance from point P to the infinite line
defined by the start point S to the end point E. S, E, P are all
pvectors which happen to be used as points."

  ;; See this page:
  ;; http://geomalgorithms.com/a02-_lines.html#Distance-to-Infinite-Line
  ;; This implements the d(P, L) equation.

  (let* (;; Compute the vector l from the two points: l = e - s
         (l (vsub e s))
         ;; Compute the vector w from the line starting from s and ending at p
         (w (vsub p s))
         ;; Compute the cross product of l and w
         (lw-cross (vcross l w))
         ;; Get the magnitude of lw-cross
         (lw-cross-mag (vnorm lw-cross))
         ;; Then compute the magnitiude of l
         (l-norm (vnorm l)))

    ;; Now, compute the distance and return it
    (/ lw-cross-mag l-norm)))


;; single threaded cache of memory for the enclosed function to prevent
;; a lot of memory churn during the transform interpolation code.
(let ((q-from (pquat))
      (q-to (pquat))
      (q-inter (pquat))
      (trans-from (pvec))
      (trans-to (pvec))
      (trans-inter (pvec)))

  (defun interpolate-transform-matricies-into (mat-dst mat-from mat-to interp)
    "Interpolate from MAT-FROM to MAT-TO by INTERP amont and store into MAT-DST.
Return MAT-DST. The interpolation happens via conversion to quaternions."
    ;; Compute the rotational interpolation
    (quat-mtoq-into q-from mat-from)
    (quat-mtoq-into q-to mat-to)
    (quat-slerp-into q-inter q-from q-to interp)
    ;; Compute the translational interpolation.
    (matrix-translate-get-into trans-from mat-from)
    (matrix-translate-get-into trans-to mat-to)
    (vect-interpolate-into trans-inter trans-from trans-to interp)
    ;; Now assemble everything into the interpolated matrix.
    (quat-qtom-into mat-dst q-inter)
    (matrix-translate-set-into mat-dst trans-inter)

    mat-dst))

(defun interpolate-transform-matricies (mat-from mat-to interp)
  (interpolate-transform-matricies-into (pmat) mat-from mat-to interp))


(defun get-monitor-refresh-rate ()
  ;; TODO: Put a config entry in here in case I need to specify it manually.
  ;; This isn't exactly right, since some LCD monitors use 59.95 and
  ;; SDL just truncates it to an integer.
  (let ((val (nth-value 3 (sdl2:get-current-display-mode 0))))
    (if (or (null val) (zerop val))
        60d0
        59.95d0 #++(float val 1d0))))
