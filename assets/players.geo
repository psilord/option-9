;; -*- mode: Lisp; -*-

`(
  ;; unique name of the geometry specification, they usually happen to
  ;; be the same name as the instance that uses them, but that isn't
  ;; required. The name of the geometry and the name of the instance
  ;; that may use it are in different namespaces.

  ;; ;;;;;;;;;;;;;;;;;;;;
  ;; ship geometries
  ;; ;;;;;;;;;;;;;;;;;;;;
  (:player-1-ship
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

    ;; We name locations in the frame of this geometry where things can be
    ;; put and default directions they can point.
    ;; Front is positive :y, up is positive :z, right is positive :x.
    :ports
    (
     (:shield-port
      (:orientation ,(mi)))

     (:passive-weapon-port
      (:orientation ,(mi)))

     (:front-weapon-port
      (:orientation ,(mtr (pvec 0d0 4d0 0d0))))
     (:left-weapon-port
      (:orientation ,(mtr (pvec -3d0 3d0 0d0))))
     (:center-weapon-port
      (:orientation ,(mi)))
     (:right-weapon-port
      (:orientation ,(mtr (pvec 3d0 3d0 0d0))))
     (:rear-weapon-port
      ;; Turn so the :y axis points down the negative :y of the ship.
      (:orientation ,(mm (mtr (pvec  0d0 -1d0 0d0))
                         (mra pi (pvec 0d0 0d0 1d0))))))
    ))

  ;; ;;;;;;;;;;;;;;;;;;;;
  ;; shot geometries
  ;; ;;;;;;;;;;;;;;;;;;;;

  (:player-1-simple-shot
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 1 1))
                 ((1d0 0d0 0d0) (1 1 1))
                 ((0d0 -1d0 0d0) (1 1 1))
                 ((-1d0 0d0 0d0) (1 1 1))))))

  (:player-1-hardnose-shot
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (1 0 0))
                 ((1d0 0d0 0d0) (1 0 0))
                 ((0d0 -1d0 0d0) (1 0 0))
                 ((-1d0 0d0 0d0) (1 0 0))))))

  (:player-1-super-shot
   (:primitives
    ((:line-loop ((0d0 1d0 0d0) (0 1 0))
                 ((1d0 0d0 0d0) (0 1 0))
                 ((0d0 -1d0 0d0) (0 1 0))
                 ((-1d0 0d0 0d0) (0 1 0))))))

  ;; ;;;;;;;;;;;;;;;;;;;;
  ;; shield geometries
  ;; ;;;;;;;;;;;;;;;;;;;;

  (:player-1-shot-shield
   (:primitives
    ((:line-loop ((0d0 5d0 0d0) (.7 .7 1))
                 ((1d0 4d0 0d0) (.7 .7 1))
                 ((2d0 5d0 0d0) (.7 .7 1))
                 ((5d0 2d0 0d0) (.7 .7 1))
                 ((4d0 1d0 0d0) (.7 .7 1))
                 ((5d0 0d0 0d0) (.7 .7 1))
                 ((2d0 -3d0 0d0) (.7 .7 1))
                 ((1d0 -2d0 0d0) (.7 .7 1))
                 ((0d0 -3d0 0d0) (.7 .7 1))
                 ((-1d0 -2d0 0d0) (.7 .7 1))
                 ((-2d0 -3d0 0d0) (.7 .7 1))
                 ((-5d0 0d0 0d0) (.7 .7 1))
                 ((-4d0 1d0 0d0) (.7 .7 1))
                 ((-5d0 2d0 0d0) (.7 .7 1))
                 ((-2d0 5d0 0d0) (.7 .7 1))
                 ((-1d0 4d0 0d0) (.7 .7 1))))))

  (:player-1-ship-shield
   (:primitives
    ((:line-loop ((1d0 5d0 0d0) (.7 .7 1))
                 ((1d0 3d0 0d0) (.7 .7 1))
                 ((2d0 3d0 0d0) (.7 .7 1))
                 ((2d0 4d0 0d0) (.7 .7 1))
                 ((4d0 4d0 0d0) (.7 .7 1))
                 ((4d0 -2d0 0d0) (.7 .7 1))
                 ((1d0 -2d0 0d0) (.7 .7 1))
                 ((0d0 -3d0 0d0) (.7 .7 1))
                 ((-1d0 -2d0 0d0) (.7 .7 1))
                 ((-4d0 -2d0 0d0) (.7 .7 1))
                 ((-4d0 4d0 0d0) (.7 .7 1))
                 ((-2d0 4d0 0d0) (.7 .7 1))
                 ((-2d0 3d0 0d0) (.7 .7 1))
                 ((-1d0 3d0 0d0) (.7 .7 1))
                 ((-1d0 5d0 0d0) (.7 .7 1))))))

  )
