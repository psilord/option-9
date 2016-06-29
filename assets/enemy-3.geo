;; -*- mode: Lisp; -*-

`(
  (:enemy-3-simple-shot
   (:primitives
    ((:line-loop ((0d0 -1d0 0d0) (1 1 1))
                 ((1d0 -2d0 0d0) (1 1 1))
                 ((1d0 -1d0 0d0) (1 1 1))
                 ((0d0 1d0 0d0) (1 1 1))
                 ((-1d0 -1d0 0d0) (1 1 1))
                 ((-1d0 -2d0 0d0) (1 1 1))))))

  (:enemy-3-shot-shield
   (:primitives
    ((:line-loop ((1d0 -5d0 0d0) (1 .7 .7))
                 ((3d0 -3d0 0d0) (1 .7 .7))
                 ((3d0 0d0 0d0) (1 .7 .7))
                 ((0d0 5d0 0d0) (1 .7 .7))
                 ((-3d0 0d0 0d0) (1 .7 .7))
                 ((-3d0 -3d0 0d0) (1 .7 .7))
                 ((-1d0 -5d0 0d0) (1 .7 .7))))))

  (:enemy-3-ship
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
                 ((-1d0 -4d0 0d0) (1 1 1))))
    :ports
    ((:shield-port
      (:orientation ,(mi)))

     (:passive-weapon-port
      (:orientation ,(mi)))

     (:front-weapon-port
      (:orientation ,(mtr (pvec 0d0 4d0 0d0))))
     (:left-weapon-port
      (:orientation ,(mtr (pvec -2d0 0d0 0d0))))
     (:center-weapon-port
      (:orientation ,(mi)))
     (:right-weapon-port
      (:orientation ,(mtr (pvec 2d0 0d0 0d0))))
     (:rear-weapon-port
      ;; Turn so the pos :y axis of the turret points down the
      ;; negative :y of the ship.
      (:orientation ,(mm (mtr (pvec 0d0 -2d0 0d0))
                         (mra pi (pvec 0d0 0d0 1d0))))))))


  (:enemy-3/shrapnel-1
   (:primitives
    ((:line-loop ((1d0 -2d0 0d0) (1 1 1))
                 ((2d0 -1d0 0d0) (1 1 1))
                 ((0d0 3d0 0d0) (1 1 1))
                 ((-2d0 -1d0 0d0) (1 1 1))))))

  (:enemy-3/shrapnel-2
   (:primitives
    ((:line-loop ((1d0 -2.5d0 0d0) (1 1 1))
                 ((2d0 -.5d0 0d0) (1 1 1))
                 ((1d0 .5d0 0d0) (1 1 1))
                 ((-2d0 1.5d0 0d0) (1 1 1))
                 ((-1d0 .5d0 0d0) (1 1 1))
                 ((-2d0 -.5d0 0d0) (1 1 1))
                 ((-1d0 -2.5d0 0d0) (1 1 1))
                 ((0d0 -.5d0 0d0) (1 1 1))))))
  )
