(in-package :option-9)

;; Create an explosion centered about the entity using the spark
;; amount in the entity itself.  TODO: This spawns sparks into the
;; :universe. It might be better to allow more flexible parentage for
;; where the sparks show up for certain visual effects.

(defmethod explode ((ent entity))
  ;; We either span shrapnel or sparks, but not both.
  (let ((per (shrapnel-generation ent)))
    (if (and per (< (random 1.0) per))
        ;; If the entity has any shrapnel, spawn each instance of it in a
        ;; from the center of the entity.
        (let ((shrapnel-pieces (shrapnel ent)))
          (when shrapnel-pieces
            (dolist (shrapnel shrapnel-pieces)
              (spawn 'sp-shrapnel shrapnel ent (game-context ent)))))
        ;; Emit a number of sparks related to the death of the entity if
        ;; we have decided there is no shrapnel to emit.
        (spawn 'sp-sparks
               :insts/sparks ent (game-context ent)
               ;; Give the small impression of a shockwave
               :initial-velocity .005d0
               :num-sparks (sparks ent)))))


