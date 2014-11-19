(in-package #:option-9)

;; By default, something cannot increase its own power.
(defmethod increase-power (thing)
  nil)

;; We don't waste an increase of power if possible.
(defmethod increase-power ((tf tesla-field))
  (if (zerop (random 2))
      (if (power-density-maxp tf)
          (increase-range tf)
          (increase-density tf))
      (if (power-range-maxp tf)
          (increase-density tf)
          (increase-range tf))))

;; Recharge the shield back to full (meaning it hasn't absorbed any shots).
;; This is ok behavior for now, we'll see if I do something more complex later.
(defmethod increase-power ((sh shield))
  (setf (shots-absorbed sh) 0))
