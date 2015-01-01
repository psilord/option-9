;; -*- mode: Lisp; -*-

`(
  (:enemy-2-simple-shot
   (:primitives
    ((:line-loop ((1d0 0d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((0d0 2d0 0d0) (1 1 1))))))

  (:enemy-2-ship
   (:primitives
    ((:line-loop ((0d0 4d0 0d0) (1 1 1))
                 ((1d0 3d0 0d0) (1 1 1))
                 ((1d0 4d0 0d0) (1 1 1))
                 ((2d0 3d0 0d0) (1 1 1))
                 ((2d0 2d0 0d0) (1 1 1))
                 ((3d0 2d0 0d0) (1 1 1))
                 ((4d0 1d0 0d0) (1 1 1))
                 ((4d0 0d0 0d0) (1 1 1))
                 ((3d0 -1d0 0d0) (1 1 1))
                 ((2d0 -1d0 0d0) (1 1 1))
                 ((2d0 -2d0 0d0) (1 1 1))
                 ((1d0 -3d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((0d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))
                 ((-1d0 -3d0 0d0) (1 1 1))
                 ((-2d0 -2d0 0d0) (1 1 1))
                 ((-2d0 -1d0 0d0) (1 1 1))
                 ((-3d0 -1d0 0d0) (1 1 1))
                 ((-4d0 0d0 0d0) (1 1 1))
                 ((-4d0 1d0 0d0) (1 1 1))
                 ((-3d0 2d0 0d0) (1 1 1))
                 ((-2d0 2d0 0d0) (1 1 1))
                 ((-2d0 3d0 0d0) (1 1 1))
                 ((-1d0 4d0 0d0) (1 1 1))
                 ((-1d0 3d0 0d0) (1 1 1))))
    :ports
    ((:shield-port
      (:orientation ,(mi)))

     (:passive-weapon-port
      (:orientation ,(mi)))

     (:front-weapon-port
      (:orientation ,(mtr (pvec 0d0 4d0 0d0))))
     (:left-weapon-port
      (:orientation ,(mtr (pvec -3d0 2d0 0d0))))
     (:center-weapon-port
      (:orientation ,(mi)))
     (:right-weapon-port
      (:orientation ,(mtr (pvec 3d0 2d0 0d0))))
     (:rear-weapon-port
      ;; Turn so the pos :y axis of the turret points down the
      ;; negative :y of the ship.
      (:orientation ,(mm (mtr (pvec 0d0 -3d0 0d0))
                         (mra pi (pvec 0d0 0d0 1d0))))))))


  (:enemy-2/shrapnel-1
   (:primitives
    ((:line-loop ((0d0 2d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((1d0 2d0 0d0) (1 1 1))
                 ((2d0 1d0 0d0) (1 1 1))
                 ((2d0 2d0 0d0) (1 1 1))
                 ((3d0 1d0 0d0) (1 1 1))
                 ((3d0 0d0 0d0) (1 1 1))
                 ((2d0 0d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))
                 ((-2d0 -2d0 0d0) (1 1 1))
                 ((-2d0 -4d0 0d0) (1 1 1))
                 ((-3d0 -3d0 0d0) (1 1 1))
                 ((-3d0 -2d0 0d0) (1 1 1))
                 ((-3d0 -1d0 0d0) (1 1 1))
                 ((-2d0 0d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))))))

  (:enemy-2/shrapnel-2
   (:primitives
    ((:line-loop ((1d0 -1.5d0 0d0) (1 1 1))
                 ((0d0 -1.5d0 0d0) (1 1 1))
                 ((-1d0 -.5d0 0d0) (1 1 1))
                 ((-2d0 -.5d0 0d0) (1 1 1))
                 ((-3d0 .5d0 0d0) (1 1 1))
                 ((-2d0 .5d0 0d0) (1 1 1))
                 ((-1d0 1.5d0 0d0) (1 1 1))
                 ((0d0 1.5d0 0d0) (1 1 1))
                 ((1d0 1.5d0 0d0) (1 1 1))
                 ((2d0 .5d0 0d0) (1 1 1))
                 ((2d0 -.5d0 0d0) (1 1 1))))))

  (:enemy-2/shrapnel-3
   (:primitives
    ((:line-loop ((3d0 -1d0 0d0) (1 1 1))
                 ((2d0 -2d0 0d0) (1 1 1))
                 ((2d0 -1d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((0d0 -2d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-2d0 -1d0 0d0) (1 1 1))
                 ((-2d0 1d0 0d0) (1 1 1))
                 ((-1d0 1d0 0d0) (1 1 1))
                 ((0d0 2d0 0d0) (1 1 1))
                 ((1d0 1d0 0d0) (1 1 1))
                 ((2d0 1d0 0d0) (1 1 1))
                 ((3d0 0d0 0d0) (1 1 1))))))

  )
