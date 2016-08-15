;; Copyright 2010 Peter K. Keller (psilord@cs.wisc.edu)
;;
;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License. You may
;; obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
;; or implied. See the License for the specific language governing
;; permissions and limitations under the License.

(in-package #:option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun make-game (&key (window-width +game-width+) (window-height +game-height+)
                    (game-width +game-width+) (game-height +game-height+))
  (let ((game (make-instance 'game
                             :scene-man (make-instance 'scene-manager)
                             :window-width window-width
                             :window-height window-height
                             :game-width game-width
                             :game-height game-height)))

    ;; and we insert our game context into the universe drawable...
    (setf (game-context (root (scene-man game))) game)
    game))


(defun window-aspect-ratio (game)
  (coerce (/ (window-width game) (window-height game)) 'double-float))

(defun game-aspect-ratio (game)
  (coerce (/ (game-width game) (game-height game)) 'double-float))

(defmethod per-window-width ((game game) percentage)
  (coerce (* (window-width game) percentage) 'double-float))

(defmethod per-window-width ((drawable drawable) percentage)
  (coerce (per-window-width (game-context drawable) percentage) 'double-float))

(defmethod per-window-height ((game game) percentage)
  (coerce (* (window-height game) percentage) 'double-float))

(defmethod per-window-height ((drawable drawable) percentage)
  (coerce (per-window-height (game-context drawable) percentage) 'double-float))

(defmethod per-game-width ((game game) percentage)
  (coerce (* (game-width game) (/ percentage 100d0)) 'double-float))

(defmethod per-game-width ((drawable drawable) percentage)
  (coerce (per-game-width (game-context drawable) percentage) 'double-float))

(defmethod per-game-height ((game game) percentage)
  (coerce (* (game-height game) (/ percentage 100d0)) 'double-float))

(defmethod per-game-height ((drawable drawable) percentage)
  (coerce (per-game-height (game-context drawable) percentage) 'double-float))

(defun add-spawnable (spawnable game)
  (push spawnable (spawnables game)))

(defun clear-spawnables (game)
  (setf (spawnables game) nil))

(defun toggle-paused (g)
  (if (paused g)
      (setf (paused g) nil)
      (setf (paused g) t)))

(defmacro with-game-init ((filename &key
                                    (window-width +game-width+)
                                    (window-height +game-height+)
                                    (game-width +game-width+)
                                    (game-height +game-height+))
                          &body body)
  ;; The let* is important to bind *id* before MAKE-GAME is called.
  `(let* ((*assets* (load-dat-file ,filename))
          (*id* 0)
          (*game* (make-game :window-width ,window-width
                             :window-height ,window-height
                             :game-width ,game-width
                             :game-height ,game-height)))
     ,@body))

(defun move-player-keyboard (game state dir)
  ;; XXX this is for player one and it only assumes one player!
  (let ((p (car (entities-with-role (scene-man game) :player))))
    (when p
      (ecase state
        (:begin
         (ecase dir
           (:up
            (setf (dfvy p) (per-hz (forward-speed p))))
           (:down
            (setf (dfvy p) (per-hz (backward-speed p))))
           (:left
            (setf (dfvx p) (per-hz (strafe-left-speed p))))
           (:right
            (setf (dfvx p) (per-hz (strafe-right-speed p))))))
        (:end
         (ecase dir
           (:up
            (when (> (dfvy p) (? :v-zero))
              (setf (dfvy p) (? :v-zero))))
           (:down
            (when (< (dfvy p) (? :v-zero))
              (setf (dfvy p) (? :v-zero))))
           (:left
            (when (< (dfvx p) (? :v-zero))
              (setf (dfvx p) (? :v-zero))))
           (:right
            (when (> (dfvx p) (? :v-zero))
              (setf (dfvx p) (? :v-zero))))))))))

(defun move-player-joystick (game axis amount)
  (let ((p (car (entities-with-role (scene-man game) :player))))
    (when p
      (cond
        ((= axis 1);; y axis
         (setf (dfvy p) (* -1d0 1.5d0 amount)))
        ((= axis 0) ;; x axis
         (setf (dfvx p) (* 1.5d0 amount)))))))



;; TODO: Condense this into a higher order function loop with flet and lambdas.
;; Get rid of the INTEGER->LIST call and just do it raw.
(defun realize-score-boards (game)
  (when (modified-score-p game) ;; don't redraw unless required.
    (let ((db #(:digit-0 :digit-1 :digit-2 :digit-3 :digit-4 :digit-5
                :digit-6 :digit-7 :digit-8 :digit-9)))
      (flet ((update-score (xstart ystart the-score role)
               ;; First, remove from the scene the current score.
               (dolist (digit (entities-with-role (scene-man game) role))
                 (remove-from-scene (scene-man game) digit))

               ;; Then add the new score into the scene.
               (let ((xstart xstart)
                     (xstep -2)
                     (ci 0))
                 (cond
                   ((zerop the-score)
                    (spawn 'sp-alphanum
                           :digit-0
                           (pvec
                            (coerce (+ xstart (* xstep ci))
                                    'double-float)
                            ystart
                            0d0)
                           game
                           :roles `(,role)))
                   (t
                    ;; Strip off the ones place each iteration and
                    ;; write the number from right to left.
                    (loop with num = the-score until (zerop num) do
                         (multiple-value-bind (q r) (floor num 10)
                           (spawn 'sp-alphanum
                                  (aref db r)
                                  (pvec (coerce (+ xstart (* xstep ci))
                                                'double-float)
                                        ystart
                                        0d0)
                                  game
                                  :roles `(,role))
                           (incf ci)

                           (setf num q) r)))))))


        ;; realize the score board
        (update-score (per-game-width game 85.0)
                      (per-game-height game 98.0)
                      (score game) :score-board)

        ;; realize the high score board
        (update-score (per-game-width game 15.0)
                      (per-game-height game 98.0)
                      (highscore game) :high-score-board)

        (setf (modified-score-p game) nil)))))


(defun reset-score-to-zero (game)
  (setf (score game) 0
        (modified-score-p game) t))

(defun modify-score (game points)
  (incf (score game) points)
  (setf (modified-score-p game) t)

  (when (< (score game) 0)
    (setf (score game) 0))

  (when (> (score game) (highscore game))
    (incf (highscore game) (- (score game) (highscore game)))))

(defun display (game jutter-interpolant)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (walk-frame-hierarchy (root (scene-man game))
                        (lambda (entity)
                          (render entity jutter-interpolant))))

(defun step-game (game)
  (unless (paused game)
    (let ((scene (scene-man game)))

      ;; 1. If there is no player, spawn one.
      (unless (entities-with-role scene :player)
        (reset-score-to-zero game)
        (spawn 'sp-player :player-1 NIL game))

      ;; 2. If enough time as passed, spawn an enemy
      ;;#+ignore
      (progn
        (decf (enemy-spawn-timer game))
        (when (zerop (enemy-spawn-timer game))
          (setf (enemy-spawn-timer game) (1+ (random 90)))
          ;; Just pick a random enemy from the various enemy instances
          (spawn 'sp-enemy :insts/enemies NIL game))
        )

      ;; 3. Show the score boards
      (realize-score-boards game)

      ;; 4. If possible, make concrete all currently pending spawns into
      ;; the scene tree.  This makes them actually show up in the game
      ;; world.
      (realize-spawns game)

      ;; 5. After the spawns, THEN compute where everyone is to take
      ;; into consideration one time translation vectors, flight,
      ;; rotation.
      (walk-frame-hierarchy (root scene)
                            #'(lambda (frame)
                                (active-step-once frame)
                                (resolve-world-basis frame)))

      ;; 5. Note: This cannot be folded into the above walk. Passive
      ;; effects require a fully computed world-basis for all objects
      ;; since the passive effects (generally fields and such things)
      ;; compute intersections with other objects in the world and all
      ;; physical locations must be resolved before those intersections
      ;; can happen.
      (walk-frame-hierarchy (root scene) #'passive-step-once)


      ;; 7. Each brain-entity can now think about what it wants to do
      ;; since the only left in the world are things that have survived
      ;; colliding, clipping, and time to live. It may shoot, change its
      ;; direction vector, insert other objects into the scene, or do
      ;; something else.
      (walk-frame-hierarchy (root scene) #'think)

      ;; 8. Perform collision detection between all of the collision
      ;; sets
      (dolist (plan (collision-plan *assets*))
        (destructuring-bind (role-fists role-faces) plan
          (let ((fist-entities
                 (apply #'all-entities-in-roles scene role-fists))
                (face-entities
                 (apply #'all-entities-in-roles scene role-faces)))
            ;; Only loop at all if there are things to collide!
            (when (and fist-entities face-entities)
              (dolist (fist fist-entities)
                (dolist (face face-entities)
                  (collide fist face)))))))

      ;; 8. removal of not alive objects out of the world.
      (flet ((remove-y/n (e) (not (alivep e)))
             ;; make this a method like UPON-DEATH or something.  XXX
             ;; Or maybe DIE is the right place for modify-score to be
             ;; called since that is not called on stale things. Hrm,
             ;; need to think about it.
             (assign-points-y/n (e)
               (when (deadp e)
                 (modify-score game (points e)))))

        ;; XXX Check for hash-table removal violation problems, if any.
        (let ((all-views (views scene)))
          (loop for role being the hash-keys of all-views do
             ;; Figure out what needs to be removed for this role.
               (let ((removable-entities
                      (remove-if-not #'remove-y/n (gethash role all-views))))
                 (dolist (ent removable-entities)
                   ;; assign points if needed
                   (assign-points-y/n ent)
                   ;; Then actually remove it from the scene tree and
                   ;; all views.
                   (remove-from-scene scene ent))))))
      )))
