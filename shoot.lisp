(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; The shoot verb has become odd. It really means to activate the
;; thing in the turret, so this verb probably has to change. I've introduce
;; the verb fire which is specific to a muzzle. Also, there is a vague
;; starting and stopping of the charge effects too.

;; XXX Fix shoot to take context which is :now or :charged. :now means
;; RIGHT NOW, but :charged only means do it if the charge has accumulated
;; enough. If it hadn't, there might be another effect like a pile of sparks
;; if you were within 5% of the non-zero charging time. Maybe we just pass the
;; context to fire and it deals with it actually.


(defgeneric fire (ship spawn-class context muzzle turret)
  (:documentation "Fire a muzzle"))


;; The method for when the player ship shoots
(defmethod shoot ((ship player) port context)
  (let* ((turret (turret ship port))
         (muzzle (payload turret)))
    (when muzzle
      ;; The 'sp-player-shot spawn class encompasses all kinds of
      ;; shots the player can make, including: shots, missles,
      ;; mines. The spawn class mainly details to role of the object,
      ;; plus some other useful things an object might need to know
      ;; while spawning.
      (fire ship 'sp-player-shot context muzzle turret)
      (unless (eq context :charged)
        (modify-score (game-context ship) -1)))))

(defmethod shoot ((ship enemy) port context)
  (let* ((turret (turret ship port))
         (muzzle (payload turret)))
    (when muzzle
      ;; The 'sp-enemy-shot spawn class encompasses all kinds of
      ;; shots the enemy can make, including: shots, missles,
      ;; mines. The spawn class mainly details to role of the object,
      ;; plus some other useful things an object might need to know
      ;; while spawning.
      (fire ship 'sp-enemy-shot context muzzle turret))))

(defmethod fire ((ship ship) spawn-class context (muzzle muzzle) turret)
  (let ((shot-name (shot-instance-name muzzle)))
    (flet ((spawn-the-shot (loc/ent)
             ;; Actually fire.
             (spawn spawn-class
                    ;; Specialize the shot in the muzzle to be appropriate for
                    ;; the ship firing it.
                    (specialize-generic-instance-name
                     (instance-name ship) shot-name)
                    loc/ent
                    (game-context ship)
                    ;; And transfer the muzzle's charge to the thing I'm about
                    ;; to fire.
                    :extra-init
                    `(:charge-percentage ,(charge-percentage muzzle)))))
      (ecase context
        (:now
         (spawn-the-shot turret))
        (:charged
         (when (>= (charge-percentage muzzle) 1d0)
           (spawn-the-shot turret))))

      ;; Stop charging the muzzle and reset, because we've fired
      (setf (chargingp muzzle) nil
            (charge-percentage muzzle) 0.0))))

(defmethod fire ((ship ship) spawn-class context (muzzle muzzle)
                 (turret n-shot-turret))
  (let ((shot-name (shot-instance-name muzzle)))
    (flet ((spawn-the-shot (loc/ent)

             (let* ((spread-angle (spread-angle turret))
                    (num-sectors (num-sectors turret))

                    ;; Clean up the inputs to be always valid.
                    (spread-angle (clamp (abs spread-angle) 0d0 (* pi 2d0)))
                    (num-sectors (if (< num-sectors 1) 1 num-sectors))

                    (half-spread-angle (/ spread-angle 2d0))

                    (turret-world (matrix-copy loc/ent))
                    (turret-rotation (mcri (mi) turret-world))
                    (turret-translation (mtrg turret-world))
                    (wiggle (* (/ pi 4d0) (sin (get-internal-real-time))))
                    (half-spread-angle (+ half-spread-angle wiggle))            

                    ;; get starting orientation of turret
                    (start-turret-rotation
                      (mlar turret-rotation (pvec 0d0 0d0 half-spread-angle)))

                    ;; The temporary frame this stuff gets shoved into for
                    ;; spawning purposes.
                    (fire-turret-frame (make-instance 'frame)))

               ;; Actually fire once for each sector.
               (dotimes (sector num-sectors)

                 ;; starting from the start-turret-rotation, rotate clockwise
                 ;; and compute fire angle.
                 (let* ((sector-start-angle
                          (lerp 0d0 spread-angle (/ sector num-sectors)))
                        (sector-end-angle
                          (lerp 0d0 spread-angle (/ (+ sector 1) num-sectors)))
                        (middle-sector-angle
                          (/ (+ sector-start-angle sector-end-angle) 2d0)))

                   ;; Take the start-orientation and rote it clockwise by the
                   ;; requested amount.
                   (mlari (world-basis fire-turret-frame)
                          start-turret-rotation
                          ;; - for clockwise rotation.
                          (pvec 0d0 0d0 (- middle-sector-angle)))

                   ;; don't forget to store the translation back into the frame
                   (mtrsi (world-basis fire-turret-frame) turret-translation)

                   (spawn spawn-class
                          ;; Specialize the shot in the muzzle to be
                          ;; appropriate for the ship firing it.
                          (specialize-generic-instance-name
                           (instance-name ship) shot-name)
                          fire-turret-frame
                          (game-context ship)
                          ;; And transfer the muzzle's charge to the thing I'm
                          ;; about to fire.
                          :extra-init
                          `(:charge-percentage ,(charge-percentage muzzle))))))
             ))
      (ecase context
        (:now
         (spawn-the-shot (world-basis turret)))
        (:charged
         (when (>= (charge-percentage muzzle) 1d0)
           (spawn-the-shot (world-basis turret)))))

      ;; Stop charging the muzzle and reset, because we've fired
      (setf (chargingp muzzle) nil
            (charge-percentage muzzle) 0.0))))

(defmethod fire ((ship ship) spawn-class context (muzzle mine-muzzle) turret)
  (let ((shot-name (shot-instance-name muzzle)))
    ;; Actually fire
    (cond
      ((plusp (mine-count muzzle))
       (spawn spawn-class
              ;; Specialize the shot in the muzzle to be appropriate for the
              ;; ship firing it.
              (specialize-generic-instance-name
               (instance-name ship) shot-name)
              turret (game-context ship)
              ;; And transfer the muzzle's charge to the thing I'm about to fire.
              :extra-init `(:charge-percentage ,(charge-percentage muzzle)))
       (decf (mine-count muzzle)))
      ((zerop (mine-count muzzle))
       ;; TODO do some visual effect, noise that you're out of mines
       nil))))



(defmethod start-charging ((ship player) port)
  (let* ((turret (turret ship port))
         (payload (payload turret)))
    (when (and payload (chargeablep payload))
      ;; Let the payload to accumulate a charge. Whatever it does with
      ;; its charge is its own deal.
      (setf (chargingp payload) t))))


(defmethod stop-charging ((ship player) port)
  (let* ((turret (turret ship port))
         (payload (payload turret)))
    (when (and payload (chargeablep payload))
      ;; The charge stops accumulating.
      (setf (chargingp payload) nil))))
