(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; Marking and checking various status about the entities.
(defmethod mark-dead ((ent entity))
  (setf (status ent) :dead))

(defmethod deadp ((ent entity))
  (eq (status ent) :dead))

(defmethod mark-stale ((ent entity))
  (setf (status ent) :stale))

(defmethod stalep ((ent entity))
  (eq (status ent) :stale))

(defmethod mark-alive ((ent entity))
  (setf (status ent) :alive))

(defmethod alivep ((ent entity))
  (eq (status ent) :alive))
