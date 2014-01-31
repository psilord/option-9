(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defmethod distance ((a frame) (b frame) &key (sqrt t))
  (let ((ao (pm-get-trans (world-basis a)))
        (bo (pm-get-trans (world-basis b))))
    (pv-dist ao bo :sqrt sqrt)))

(defun random-sign ()
  (if (zerop (random 2)) 1d0 -1d0))

(defun random-delta (&key (velocity .02d0))
  (* (random velocity) (random-sign)))
