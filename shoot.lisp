(in-package :option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; The shoot verb has become odd. It really means to activate the
;; thing in the turret, so this verb probably has to change.

(defgeneric fire (ship spawn-class muzzle turret)
  (:documentation "Fire a muzzle"))


;; The method for when the player ship shoots
(defmethod shoot ((ship player) port)
  (let* ((turret (turret ship port))
         (muzzle (payload turret)))
    (when muzzle
      ;; The 'sp-player-shot spawn class encompasses all kinds of
      ;; shots the player can make, including: shots, missles,
      ;; mines. The spawn class mainly details to role of the object,
      ;; plus some other useful things an object might need to know
      ;; while spawning.
      (fire ship 'sp-player-shot muzzle turret)
      (modify-score (game-context ship) -1))))

(defmethod shoot ((ship enemy) port)
  (let* ((turret (turret ship port))
         (muzzle (payload turret)))
    (when muzzle
      ;; The 'sp-enemy-shot spawn class encompasses all kinds of
      ;; shots the enemy can make, including: shots, missles,
      ;; mines. The spawn class mainly details to role of the object,
      ;; plus some other useful things an object might need to know
      ;; while spawning.
      (fire ship 'sp-enemy-shot muzzle turret))))

(defmethod fire ((ship ship) spawn-class (muzzle muzzle) turret)
  (let ((shot-name (shot-instance-name muzzle)))
    (spawn spawn-class
           ;; Specialize the shot in the muzzle to be appropriate for the
           ;; ship firing it.
           (specialize-generic-instance-name
            (instance-name ship)
            (weighted-choice shot-name))
           turret (game-context ship))))
