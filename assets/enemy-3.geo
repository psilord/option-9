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
    ((:line-loop ((1d0 -5d0 0d0) (1 1 1))
                 ((3d0 -3d0 0d0) (1 1 1))
                 ((3d0 0d0 0d0) (1 1 1))
                 ((0d0 5d0 0d0) (1 1 1))
                 ((-3d0 0d0 0d0) (1 1 1))
                 ((-3d0 -3d0 0d0) (1 1 1))
                 ((-1d0 -5d0 0d0) (1 1 1))))))

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
      (:orientation ,(pm-eye)))

     (:passive-weapon-port
      (:orientation ,(pm-eye)))

     (:front-weapon-port
      (:orientation ,(pm-trfm-displace-into (pm-eye) (pvec 0d0 4d0 0d0))))
     (:left-weapon-port
      (:orientation ,(pm-trfm-displace-into (pm-eye) (pvec -2d0 0d0 0d0))))
     (:center-weapon-port
      (:orientation ,(pm-eye)))
     (:right-weapon-port
      (:orientation ,(pm-trfm-displace-into (pm-eye) (pvec 2d0 0d0 0d0))))
     (:rear-weapon-port
      ;; Turn so the pos :y axis of the turret points down the
      ;; negative :y of the ship.
      (:orientation ,(pm-trfm-displace-into
                      (pm-trfm-local-axis-rotate-into
                       (pm-eye) (pvec 0d0 0d0 pi))
                      (pvec 0d0 -2d0 0d0)))))))


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
