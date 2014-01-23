(in-package :option-9)

;; Create an explosion centered about the entity using the spark
;; amount in the entity itself.  TODO: This spawns sparks into the
;; :universe. It might be better to allow more flexible parentage for
;; where the sparks show up for certain visual effects.

(defmethod explode ((ent entity))
  (spawn 'sp-sparks
         :insts/sparks ent (game-context ent) :num-sparks (sparks ent)))

