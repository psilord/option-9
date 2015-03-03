(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defmethod sparks ((ent entity))
  (+ (initial-sparks ent) (random (additional-sparks ent))))

