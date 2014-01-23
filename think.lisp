(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; By default, nothing thinks...
(defmethod think (ent)
  nil)

;; but enemies think...
(defmethod think ((ent enemy))
  (when (until-next-action ent)
    (cond
      ((zerop (until-next-action ent))
       (idea ent)
       (setf (until-next-action ent) (+ 15 (random 105))))
      (t
       (decf (until-next-action ent))))))

