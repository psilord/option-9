(in-package :option-9)

(defmethod idea (ent)
  ;; By default, nothing ever has an idea.
  nil)

;; and enemies have ideas about what they want to do in the world...
(defmethod idea ((ent enemy))
  ;; Instead of doing anything cool like inspect the world and doing
  ;; AI algorithms, we'll just shoot
  (shoot ent :front-weapon-port))
