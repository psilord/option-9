(in-package :option-9)

(defmethod generate (ent source-charge charges)
  ;; by default, nothing generates a field.
  nil)

;; Actually generate the field and compute path contact information.
(defmethod generate ((f field) source-charge charges)
  ;; Clear the contact hash because we're doing a new trace of the field.
  (clrhash (entity-contacts f))
  (trace-field-lines f source-charge charges))
