(in-package :option-9)

;; By default, the shield will absorb the collider until it is used up.
(defmethod absorbs (collider (collidee shield))
  (incf (shots-absorbed collidee))
  (values t (>= (shots-absorbed collidee) (max-shot-absorption collidee))))

;; However, if a ship hits a shot-shield, the shield doesn't stop it
;; and the shield is destroyed.
(defmethod absorbs ((collider ship) (collidee shot-shield))
  (values nil t))
