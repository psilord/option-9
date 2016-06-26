(in-package :option-9)

#+(or (not option-9-optimize-pvec) option-9-debug)
(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))
