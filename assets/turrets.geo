;; -*- mode: Lisp; -*-

`(
  ;; unique name of the geometry specification, they usually happen to
  ;; be the same name as the instance that uses them, but that isn't
  ;; required. The name of the geometry and the name of the instance
  ;; that may use it are in different namespaces.

  ;; ;;;;;;;;;;;;;;;;;;;;
  ;; turret geometries
  ;; ;;;;;;;;;;;;;;;;;;;;
  (:small-yellow-triangle
   (:primitives
    ;; just a small yellow triangle for now so I can see where it is placed.
    ((:line-loop ((-.5d0 0d0 0d0) (1 1 0))
                 ((0d0 .5d0 0d0) (1 1 0))
                 ((.5d0 0d0 0d0) (1 1 0))))))

  (:small-red-triangle
   (:primitives
    ;; just a small red triangle for now so I can see where it is placed.
    ((:line-loop ((-.5d0 0d0 0d0) (1 0 0))
                 ((0d0 .5d0 0d0) (1 0 0))
                 ((.5d0 0d0 0d0) (1 0 0))))))


  )
