(in-package #:option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defmethod turret ((ship ship) port-name)
  (gethash port-name (turrets ship)))

(defmethod (setf turret) (turret-instance (ship ship) port-name)
  (setf (gethash port-name (turrets ship)) turret-instance))
