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

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun make-game ()
  (make-instance 'game
                 :scene-man (make-instance 'scene-manager)))

(defun add-spawnable (spawnable game)
  (push spawnable (spawnables game)))

(defun clear-spawnables (game)
  (setf (spawnables game) nil))

(defun toggle-paused (g)
  (if (paused g)
      (setf (paused g) nil)
      (setf (paused g) t)))

(defmacro with-game-init ((filename) &body body)
  ;; The let* is important to bind *id* before MAKE-GAME is called.
  `(let* ((*assets* (load-dat-file ,filename))
          (*id* 0)
          (*game* (make-game)))
     ,@body))

(defun move-player-keyboard (game state dir)
  ;; XXX this is for player one and it only assumes one player!
  (let ((p (car (entities-with-role (scene-man game) :player))))
    (when p
      (ecase state
        (:begin
         (ecase dir
           (:up
            (setf (dfvy p) .015d0))
           (:down
            (setf (dfvy p) -.015d0))
           (:left
            (setf (dfvx p) -.015d0))
           (:right
            (setf (dfvx p) .015d0))))
        (:end
         (ecase dir
           (:up
            (when (> (dfvy p) 0d0)
              (setf (dfvy p) 0d0)))
           (:down
            (when (< (dfvy p) 0d0)
              (setf (dfvy p) 0d0)))
           (:left
            (when (< (dfvx p) 0d0)
              (setf (dfvx p) 0d0)))
           (:right
            (when (> (dfvx p) 0d0)
              (setf (dfvx p) 0d0)))))))))

(defun move-player-joystick (game axis amount)
  (let ((p (car (entities-with-role (scene-man game) :player))))
    (when p
      (cond
        ((= axis 1);; y axis
         (setf (dfvy p) (* -1d0 .015d0 amount)))
        ((= axis 0) ;; x axis
         (setf (dfvx p) (* .015d0 amount)))))))


(defun random-sign ()
  (if (zerop (random 2)) 1d0 -1d0))

(defun random-delta (&key (velocity .02d0))
  (* (random velocity) (random-sign)))

(defun realize-score-boards (game)
  (when (modified-score-p game) ;; don't redraw unless required.
    (let ((db `((#\0 . :digit-0)
                (#\1 . :digit-1)
                (#\2 . :digit-2)
                (#\3 . :digit-3)
                (#\4 . :digit-4)
                (#\5 . :digit-5)
                (#\6 . :digit-6)
                (#\7 . :digit-7)
                (#\8 . :digit-8)
                (#\9 . :digit-9)))
          (score-chars (reverse (map 'list #'identity
                                     (format nil "~D" (score game)))))
          (highscore-chars (reverse (map 'list #'identity
                                         (format nil "~D" (highscore game))))))

      ;; realize the score board
      (dolist (digit (entities-with-role (scene-man game) :score-board))
        (remove-from-scene (scene-man game) digit))
      (let ((xstart .85)
            (xstep -.02)
            (ci 0))
        (dolist (c score-chars)
          (spawn 'sp-alphanum
                 (cdr (assoc c db))
                 (pvec (coerce (+ xstart (* xstep ci)) 'double-float)
                       .98d0
                       0d0)
                 game
                 :roles '(:score-board))
          (incf ci)))

      ;; realize the highscore board
      (dolist (digit (entities-with-role (scene-man game) :high-score-board))
        (remove-from-scene (scene-man game) digit))
      (let ((xstart .15)
            (xstep -.02)
            (ci 0))
        (dolist (c highscore-chars)
          (spawn 'sp-alphanum
                 (cdr (assoc c db))
                 (pvec (coerce (+ xstart (* xstep ci)) 'double-float)
                       .98d0
                       0d0)
                 game
                 :roles '(:high-score-board))
          (incf ci))))

    (setf (modified-score-p game) nil)))


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

(defun display (game)
  (gl:clear :color-buffer-bit)
  (walk-frame-hierarchy (root (scene-man game))
                        #'(lambda (obj)
                            (render obj `(.01d0 .01d0)))))
(defun step-game (game)
  (unless (paused game)
    (let ((scene (scene-man game)))

      ;; 1. If there is no player, spawn one.
      (unless (entities-with-role scene :player)
        (reset-score-to-zero game)
        (spawn 'sp-player :player NIL game))

      ;; 2. If enough time as passed, spawn an enemy
      (decf (enemy-spawn-timer game))
      (when (zerop (enemy-spawn-timer game))
        (setf (enemy-spawn-timer game) (1+ (random 120)))
        ;; Just pick a random enemy from the various enemy instances
        (spawn 'sp-enemy :insts/enemies NIL game))

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





