(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun euclidean-distance (fx fy tx ty)
  (let ((factor-1 (- tx fx))
        (factor-2 (- ty fy)))
    (let ((ndist (+ (* factor-1 factor-1)
                    (* factor-2 factor-2))))
      (values (sqrt ndist) ndist))))

(defun unit-vector (from tx ty)
  (multiple-value-bind (dist ndist)
      (euclidean-distance (x from) (y from) tx ty)
    (values (/ (- tx (x from)) dist)
            (/ (- ty (y from)) dist)
            ndist)))

(defun e-field-one-point (from tx ty)
  (multiple-value-bind (ux uy ndist)
      (unit-vector from tx ty)
    (let ((c (/ (charge from) ndist)))
      (values (* c ux)
              (* c uy)))))

(defun e-field (charges tx ty)
  (let* ((const (/ 1.0 (* 4.0 pi 8.854187817e-12)))
         (nx 0) (ny 0))
    (dolist (q charges)
      (if (< (euclidean-distance (x q) (y q) tx ty)
             (radius q))
          (return-from e-field (values :collision 0 0 q))
          (multiple-value-bind (qx qy)
              (e-field-one-point q tx ty)
            (incf nx qx)
            (incf ny qy))))
    (values :tracing (* const nx) (* const ny) nil)))

(defun normalize-vector (dx dy)
  (let ((dist (euclidean-distance 0 0 dx dy)))
    (if (= dist 0)
        (values 0 0)
        (values (/ dx dist) (/ dy dist)))))

(defun e-field-direction (dx dy)
  (normalize-vector dx dy))

;; Allocate the whole array which could hold up to 'steps' locations.
(defun gen-path (steps)
  (make-array (list steps)
              :initial-contents
              (loop repeat steps collecting (make-instance 'location))))

;; When we construct a fieldpath class, we initially set up all of the
;; memory we'll need to deal with it. Since we'll compute a fair
;; amount of information per frame, we don't want to generate too much
;; garbage.
(defun make-fieldpath (max-steps)
  (make-instance 'fieldpath :path (gen-path max-steps)))

(defun make-pathcontact ()
  (make-instance 'pathcontact))

(defun gen-paths (num-paths steps)
  (make-array (list num-paths)
              :initial-contents
              (loop repeat num-paths collecting
                   (make-fieldpath steps))))

;; Return true if c1 and c2 are both positive, or both negative, charges
(defun same-polarityp (c1 c2)
  (or (and (> (charge c1) 0) (> (charge c2) 0))
      (and (< (charge c1) 0) (< (charge c2) 0))))



