(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; Thinking happens on each physics fixed timestep.

;; By default, nothing thinks...
(defmethod think (ent)
  nil)

;; but enemies think...
(defmethod think ((ent enemy))
  (cond
    ((plusp (time-to-next-action ent))
     ;; If there is more time to go, we'll have to wait.
     (decf (time-to-next-action ent) *dt*))
    (t
     ;; Otherwise we have an idea to do something.
     (idea ent)
     ;; To remove periodicity, we'll recompute the idea-rate
     (setf (idea-rate ent) (compute-ratespec (idea-rate-spec ent)))

     ;; Then, we wait until that time comes.
     (setf (time-to-next-action ent) (idea-rate ent)))))
