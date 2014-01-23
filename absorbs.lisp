(in-package :option-9)

;; By default, the shield will absorb the collider.
(defmethod absorbs (collider (collidee shield))
  (when (> (shots-absorbed collidee) 0)
    (decf (shots-absorbed collidee)))
  (values t (zerop (shots-absorbed collidee))))

;; However, if a ship hits a shot-shield, the shield doesn't stop it
;; and the shield is destroyed.
(defmethod absorbs ((collider ship) (collidee shot-shield))
  (values nil t))

