;; -*- mode: Lisp; -*-

;; A file that holds geometry data for the primitives. Each geometry
;; is named by a unique keyword. These eventually end up an initargs
;; to a geometry class when it is being created.

`(
  ;; unique name of the geometry specification, they usually happen to
  ;; be the same name as the instance that uses them, but that isn't
  ;; required. The name of the geometry and the name of the instance
  ;; that may use it are in different namespaces.
  (:player-simple-shot
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 1 1))
                 ((1d0 0d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))))))

  (:player-hardnose-shot
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 0 0))
                 ((1d0 0d0 0d0) (1 0 0))
                 ((0d0 -1d0 0d0) (1 0 0))
                 ((-1d0 0d0 0d0) (1 0 0))))))

  (:player-super-shot
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (0 1 0))
                 ((1d0 0d0 0d0) (0 1 0))
                 ((0d0 -1d0 0d0) (0 1 0))
                 ((-1d0 0d0 0d0) (0 1 0))))))


  (:enemy-1-simple-shot
   (:primitives
    ((:line-loop ((.5d0 -1.5d0 0d0) (1 1 1))
                 ((-.5d0 -.5d0 0d0) (1 1 1))
                 ((.5d0 .5d0 0d0) (1 1 1))
                 ((-.5d0 1.5d0 0d0) (1 1 1))
                 ((.5d0 .5d0 0d0) (1 1 1))
                 ((-.5d0 -.5d0 0d0) (1 1 1))))))

  (:enemy-2-simple-shot
   (:primitives
    ((:line-loop ((0d0 -1d0 0d0) (1 1 1))
                 ((1d0 0d0 0d0) (1 1 1))
                 ((0d0 2d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))))))

  (:enemy-3-simple-shot
   (:primitives
    ((:line-loop ((0d0 -1d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))))))


  (:player
   (:primitives
    ((:line-loop ((0d0 4d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((2d0 1d0 0d0) (1 1 1))
                 ((3d0 3d0 0d0) (1 1 1))
                 ((3d0 -1d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((0d0 -2d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-3d0 -1d0 0d0) (1 1 1))
                 ((-3d0 3d0 0d0) (1 1 1))
                 ((-2d0 1d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))))
    ;; Port-specs name locations of sub-hierarchical bases in the geometry
    ;; that define locations and orientations of where to put things.
    :port-specs
    ((:active-shield
      :basis ,(pm-eye))
     (:passive-shield
      :basis ,(pm-eye))
     (:main-cannon
      ;; basis is the location that it can be placed.
      :basis ,(pm-translate-into (pm-eye) (pvec 0d0 4d0 0d0))
      ;; direction of fire
      :dof :y)
     (:left-cannon
      :basis ,(pm-translate-into (pm-eye) (pvec -3d0 3d0 0d0))
      :dof :y)
     (:right-cannon
      :basis ,(pm-translate-into (pm-eye) (pvec 3d0 3d0 0d0))
      :dof :y))))

  (:ship-shot-shield
   (:primitives
    ((:line-loop ((0d0 5d0 0d0) (1 1 1))
                 ((1d0 4d0 0d0) (1 1 1))
                 ((2d0 5d0 0d0) (1 1 1))
                 ((5d0 2d0 0d0) (1 1 1))
                 ((4d0 1d0 0d0) (1 1 1))
                 ((5d0 0d0 0d0) (1 1 1))
                 ((2d0 -3d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((0d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))
                 ((-2d0 -3d0 0d0) (1 1 1))
                 ((-5d0 0d0 0d0) (1 1 1))
                 ((-4d0 1d0 0d0) (1 1 1))
                 ((-5d0 2d0 0d0) (1 1 1))
                 ((-2d0 5d0 0d0) (1 1 1))
                 ((-1d0 4d0 0d0) (1 1 1))))))

  (:ship-ship-shield
   (:primitives
    ((:line-loop ((1d0 5d0 0d0) (1 1 1))
                 ((1d0 3d0 0d0) (1 1 1))
                 ((2d0 3d0 0d0) (1 1 1))
                 ((2d0 4d0 0d0) (1 1 1))
                 ((4d0 4d0 0d0) (1 1 1))
                 ((4d0 -2d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((0d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))
                 ((-4d0 -2d0 0d0) (1 1 1))
                 ((-4d0 4d0 0d0) (1 1 1))
                 ((-2d0 4d0 0d0) (1 1 1))
                 ((-2d0 3d0 0d0) (1 1 1))
                 ((-1d0 3d0 0d0) (1 1 1))
                 ((-1d0 5d0 0d0) (1 1 1))))))


  (:enemy-3-shot-shield
   (:primitives
    ((:line-loop ((1d0 -5d0 0d0) (1 1 1))
                 ((3d0 -3d0 0d0) (1 1 1))
                 ((3d0 -0d0 0d0) (1 1 1))
                 ((0d0 5d0 0d0) (1 1 1))
                 ((-3d0 -0d0 0d0) (1 1 1))
                 ((-3d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -5d0 0d0) (1 1 1))))))

  (:enemy-1
   (:primitives
    ((:line-loop ((0d0 -4d0 0d0) (1 1 1))
                 ((3d0 -2d0 0d0) (1 1 1))
                 ((3d0 1d0 0d0) (1 1 1))
                 ((2d0 2d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((0d0 2d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((-2d0 2d0 0d0) (1 1 1))
                 ((-3d0 1d0 0d0) (1 1 1))
                 ((-3d0 -2d0 0d0) (1 1 1))))))

  (:enemy-1/shrapnel-1
   (:primitives
    ((:line-loop ((-1d0 -3d0 0d0) (1 1 1))
                 ((2d0 -1d0 0d0) (1 1 1))
                 ((2d0 2d0 0d0) (1 1 1))
                 ((1d0 3d0 0d0) (1 1 1))
                 ((0d0 2d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((-2d0 -1d0 0d0) (1 1 1))))))

  (:enemy-1/shrapnel-2
   (:primitives
    ((:line-loop ((0d0 -2.5d0 0d0) (1 1 1))
                 ((0d0 -1.5d0 0d0) (1 1 1))
                 ((1d0 -.5d0 0d0) (1 1 1))
                 ((0d0 .5d0 0d0) (1 1 1))
                 ((-1d0 -.5d0 0d0) (1 1 1))))))

  (:enemy-1/shrapnel-3
   (:primitives
    ((:line-loop ((1.5d0 -3d0 0d0) (1 1 1))
                 ((.5d0 -1d0 0d0) (1 1 1))
                 ((1.5d0 0d0 0d0) (1 1 1))
                 ((.5d0 2d0 0d0) (1 1 1))
                 ((-.5d0 3d0 0d0) (1 1 1))
                 ((-1.5d0 2d0 0d0) (1 1 1))
                 ((-1.5d0 -1d0 0d0) (1 1 1))))))

  (:enemy-2
   (:primitives
    ((:line-loop ((0d0 -4d0 0d0) (1 1 1))
                 ((1d0 -3d0 0d0) (1 1 1))
                 ((1d0 -4d0 0d0) (1 1 1))
                 ((2d0 -3d0 0d0) (1 1 1))
                 ((2d0 -2d0 0d0) (1 1 1))
                 ((3d0 -2d0 0d0) (1 1 1))
                 ((4d0 -1d0 0d0) (1 1 1))
                 ((4d0 0d0 0d0) (1 1 1))
                 ((3d0 1d0 0d0) (1 1 1))
                 ((2d0 1d0 0d0) (1 1 1))
                 ((2d0 2d0 0d0) (1 1 1))
                 ((1d0 3d0 0d0) (1 1 1))
                 ((1d0 2d0 0d0) (1 1 1))
                 ((0d0 3d0 0d0) (1 1 1))
                 ((-1d0 2d0 0d0) (1 1 1))
                 ((-1d0 3d0 0d0) (1 1 1))
                 ((-2d0 2d0 0d0) (1 1 1))
                 ((-2d0 1d0 0d0) (1 1 1))
                 ((-3d0 1d0 0d0) (1 1 1))
                 ((-4d0 0d0 0d0) (1 1 1))
                 ((-4d0 -1d0 0d0) (1 1 1))
                 ((-3d0 -2d0 0d0) (1 1 1))
                 ((-2d0 -2d0 0d0) (1 1 1))
                 ((-2d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -4d0 0d0) (1 1 1))
                 ((-1d0 -3d0 0d0) (1 1 1))))))

  (:enemy-2/shrapnel-1
   (:primitives
    ((:line-loop ((0d0 -2d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((2d0 -1d0 0d0) (1 1 1))
                 ((2d0 0d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 2d0 0d0) (1 1 1))
                 ((-1d0 3d0 0d0) (1 1 1))
                 ((-1d0 2d0 0d0) (1 1 1))
                 ((-2d0 0d0 0d0) (1 1 1))))))

  (:enemy-2/shrapnel-2
   (:primitives
    ((:line-loop ((-1.5d0 -0.5d0 0d0) (1 1 1))
                 ((-0.5d0 1.5d0 0d0) (1 1 1))
                 ((-1.5d0 2.5d0 0d0) (1 1 1))
                 ((-2.5d0 1.5d0 0d0) (1 1 1))
                 ((-2.5d0 2.5d0 0d0) (1 1 1))
                 ((-3.5d0 1.5d0 0d0) (1 1 1))
                 ((-3.5d0 0.5d0 0d0) (1 1 1))
                 ((-4.5d0 0.5d0 0d0) (1 1 1))
                 ((-5.5d0 -0.5d0 0d0) (1 1 1))
                 ((-5.5d0 -1.5d0 0d0) (1 1 1))
                 ((-4.5d0 -2.5d0 0d0) (1 1 1))
                 ((-2.5d0 -0.5d0 0d0) (1 1 1))))))

  (:enemy-2/shrapnel-3
   (:primitives
    ((:line-loop ((0.5d0 -2d0 0d0) (1 1 1))
                 ((1.5d0 -1d0 0d0) (1 1 1))
                 ((1.5d0 -2d0 0d0) (1 1 1))
                 ((2.5d0 -1d0 0d0) (1 1 1))
                 ((2.5d0 0d0 0d0) (1 1 1))
                 ((0.5d0 2d0 0d0) (1 1 1))
                 ((-0.5d0 2d0 0d0) (1 1 1))
                 ((-2.5d0 0d0 0d0) (1 1 1))
                 ((-1.5d0 0d0 0d0) (1 1 1))
                 ((-1.5d0 -1d0 0d0) (1 1 1))
                 ((-0.5d0 -2d0 0d0) (1 1 1))
                 ((-0.5d0 -1d0 0d0) (1 1 1))))))

  (:enemy-3
   (:primitives
    ((:line-loop ((0d0 -2d0 0d0) (1 1 1))
                 ((1d0 -4d0 0d0) (1 1 1))
                 ((2d0 -2d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((2d0 0d0 0d0) (1 1 1))
                 ((0d0 4d0 0d0) (1 1 1))
                 ((-2d0 0d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-2d0 -2d0 0d0) (1 1 1))
                 ((-1d0 -4d0 0d0) (1 1 1))))))


  (:enemy-3/shrapnel-1
   (:primitives
    ((:line-loop ((0d0 0d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((2d0 0d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((-2d0 2d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((-2d0 0d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))))))

  (:enemy-3/shrapnel-2
   (:primitives
    ((:line-loop ((1d0 0d0 0d0) (1 1 1))
                 ((2d0 1d0 0d0) (1 1 1))
                 ((0d0 5d0 0d0) (1 1 1))
                 ((-2d0 1d0 0d0) (1 1 1))))))

  (:proximity-mine
   (:primitives
    ((:line-loop ((1d0 3d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((3d0 1d0 0d0) (1 1 1))
                 ((3d0 -1d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((1d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-3d0 -1d0 0d0) (1 1 1))
                 ((-3d0 1d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((-1d0 3d0 0d0) (1 1 1))))))

  (:field-mine
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1)))
     (:lines ((1d0 1d0 0d0) (1 1 1))
             ((3d0 3d0 0d0) (1 1 1))
             ((1d0 0d0 0d0) (1 1 1))
             ((3d0 0d0 0d0) (1 1 1))
             ((1d0 -1d0 0d0) (1 1 1))
             ((3d0 -3d0 0d0) (1 1 1))
             ((0d0 -1d0 0d0) (1 1 1))
             ((0d0 -3d0 0d0) (1 1 1))
             ((-1d0 -1d0 0d0) (1 1 1))
             ((-3d0 -3d0 0d0) (1 1 1))
             ((-1d0 0d0 0d0) (1 1 1))
             ((-3d0 0d0 0d0) (1 1 1))
             ((-1d0 1d0 0d0) (1 1 1))
             ((-3d0 3d0 0d0) (1 1 1))
             ((0d0 1d0 0d0) (1 1 1))
             ((0d0 3d0 0d0) (1 1 1))))))

  (:digit-0
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))))))

  (:digit-1
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))))))

  (:digit-2
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))))))

  (:digit-3
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))))))

  (:digit-4
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))))))

  (:digit-5
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))))))

  (:digit-6
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))))))

  (:digit-7
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))))))

  (:digit-8
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))))))

  (:digit-9
   (:primitives
    ((:line-loop ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))))))

  (:spark-1
   (:primitives
    ((:line-loop ((-.5d0 0d0 0d0) (.5 .5 .4))
                 ((0d0 .5d0 0d0) (.5 .5 .4))
                 ((.5d0 0d0 0d0) (.5 .5 .4))))))

  (:spark-2
   (:primitives
    ((:line-loop ((-.5d0 0d0 0d0) (.5 .5 .4))
                 ((0d0 -.5d0 0d0) (.5 .5 .4))
                 ((0d0 0d0 0d0) (.5 .5 .4))
                 ((.5d0 0d0 0d0) (.5 .5 .4))
                 ((0d0 .5d0 0d0) (.5 .5 .4))))))

  (:spark-3
   (:primitives
    ((:line-loop ((0d0 .5d0 0d0) (.5 .5 .4))
                 ((-.5d0 0d0 0d0) (.5 .5 .4))
                 ((.5d0 0d0 0d0) (.5 .5 .4))
                 ((0d0 -.5d0 0d0) (.5 .5 .4))))))

  )












