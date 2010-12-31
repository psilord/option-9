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

;; Each thing in the world is kept is its particular list. This makes it
;; easy to perform collision detection only as necessary.
(defclass game ()
  ((players :initarg :players
            :initform nil
            :accessor players)
   (player-shots :initarg :player-shots
                 :initform nil
                 :accessor player-shots)
   (enemies :initarg :enemies
            :initform nil
            :accessor enemies)
   (enemy-shots :initarg :enemy-shots
                :initform nil
                :accessor enemy-shots)
   (sparks :initarg :sparks
           :initform nil
           :accessor sparks)
   (power-ups :initargs :power-ups
              :initform nil
              :accessor power-ups)
   (score :initarg :score
          :initform 0
          :accessor score)
   (score-board :initarg :score-board
                :initform nil
                :accessor score-board)
   (highscore :initarg :highscore
              :initform 0
              :accessor highscore)
   (highscore-board :initarg :highscore-board
                    :initform nil
                    :accessor highscore-board)
   (enemy-spawn-timer :initarg :enemy-spawn-timer
                      :initform 60
                      :accessor enemy-spawn-timer))
  (:documentation "The Game Class"))

(defun make-game ()
  (make-instance 'game))

(defmacro with-game-init ((filename) &body body)
  `(let ((*all-entities* (load-all-entities ,filename))
         (*game* (make-game)))
     ,@body))

(defun spawn-player (game)
  (setf (players game) (list (make-entity :player :x .5 :y .05))))

(defun move-player (game state dir)
  ;; XXX this is for player one and it only assumes one player!
  (let ((p (car (players game))))
    (ecase state
      (:begin
       (ecase dir
         (:up (setf (dy p) .015))
         (:down (setf (dy p) -.015))
         (:left (setf (dx p) -.015))
         (:right (setf (dx p) .015))))
      (:end
       (ecase dir
         (:up (when (> (dy p) 0) (setf (dy p) 0)))
         (:down (when (< (dy p) 0) (setf (dy p) 0)))
         (:left (when (< (dx p) 0) (setf (dx p) 0)))
         (:right (when (> (dx p) 0) (setf (dx p) 0))))))))

(defun spawn-enemy (game)
  (let ((bad-guys (vector :enemy-1 :enemy-2 :enemy-3))
        (xloc (random 1.0)))
    (push (make-entity (svref bad-guys (random (length bad-guys)))
                       :x xloc :y .95
                       :dx (* (random .001) (if (> xloc .5) -1 1))
                       :dy (- (random .01)))
          (enemies game))))

(defun render-game (game scale)
  (with-accessors ((players players)
                   (player-shots player-shots)
                   (enemies enemies)
                   (enemy-shots enemy-shots)
                   (sparks sparks)
                   (power-ups power-ups)
                   (score-board score-board)
                   (highscore-board highscore-board))
      game

    (loop for i in (list score-board highscore-board players player-shots
                         enemies enemy-shots sparks power-ups) do
         (loop for e in i do
              (render e scale)))))

;; Create an explosion centered about the entity using the spark amount
;; in the entity itself.
(defun make-explosion (ent)
  (let ((db (vector :spark-1 :spark-2 :spark-3)))
    (dotimes (p (+ (initial-sparks ent) (random (additional-sparks ent))))
      (push (make-entity
             (svref db (random (length db)))
             :x (x ent) :y (y ent)
             :dx (* (random .02) (if (zerop (random 2)) 1 -1))
             :dy (* (random .02) (if (zerop (random 2)) 1 -1)))
            (sparks (game-context ent))))))

;; Pick a random powerup to place in place of the enemy
(defun make-powerup (ent)
  (let ((db (vector :powerup-hardnose :powerup-super-shot
                    :powerup-shot-shield :powerup-ship-shield)))
    (push (make-entity (svref db (random (length db)))
                       :x (x ent) :y (y ent))
          (power-ups (game-context ent)))))

(defun realize-score-boards (game)
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
    (setf (score-board game) nil)
    (let ((xstart .85)
          (xstep -.02)
          (ci 0))
      (dolist (c score-chars)
        (push (make-entity (cdr (assoc c db))
                           :x (+ xstart (* xstep ci)) :y .98)
              (score-board game))
        (incf ci)))

    ;; realize the highscore board
    (setf (highscore-board game) nil)
    (let ((xstart .15)
          (xstep -.02)
          (ci 0))
      (dolist (c highscore-chars)
        (push (make-entity (cdr (assoc c db))
                           :x (+ xstart (* xstep ci)) :y .98)
              (highscore-board game))
        (incf ci)))))

(defun reset-score-to-zero (game)
  (setf (score game) 0)
  (realize-score-boards game))

(defun modify-score (game points)
  (incf (score game) points)
  (when (< (score game) 0)
    (setf (score game) 0))
  (when (> (score game) (highscore game))
    (incf (highscore game) (- (score game) (highscore game))))
  (realize-score-boards game))

(defun step-game (game)
  (with-accessors ((players players)
                   (player-shots player-shots)
                   (enemies enemies)
                   (enemy-shots enemy-shots)
                   (sparks sparks)
                   (power-ups power-ups)
                   (score score)
                   (highscore highscore)
                   (enemy-spawn-timer enemy-spawn-timer))
      game

    ;; 1. Simulate one step for each entity. Specific methods may do
    ;; pretty complex things during their simulation step....
    (loop for i in (list players player-shots enemies enemy-shots sparks
                         power-ups ) do
         (loop for e in i do
              (step-once e)))

    ;; 2. Collide the various entity sets
    (flet ((collide-game-entity-sets (fist face)
             (dolist (left fist)
               (dolist (right face)
                 ;; generic function does anything it wants
                 (collide left right)))))

      (collide-game-entity-sets players power-ups)
      (collide-game-entity-sets player-shots enemy-shots)
      (collide-game-entity-sets player-shots enemies)
      (collide-game-entity-sets enemy-shots players)
      (collide-game-entity-sets enemies players))

    ;; 3. Remove any dead or stale entities, assigning points if necessary
    (flet ((remove-y/n (e)
             (when (eq (status e) :dead)
               (modify-score game (points e)))

             (or (eq (status e) :dead) (eq (status e) :stale)
                 ;; remove if out of the displayed game world...
                 (< (x e) -.05) (> (x e) 1.05)
                 (< (y e) -.05) (> (y e) 1.05))))

      (setf player-shots (remove-if #'remove-y/n player-shots)
            enemies (remove-if #'remove-y/n enemies)
            enemy-shots (remove-if #'remove-y/n enemy-shots)
            players (remove-if #'remove-y/n players)
            sparks (remove-if #'remove-y/n sparks)
            power-ups (remove-if #'remove-y/n power-ups)))

    ;; 4. If the player died, respawn a new one at the start location
    ;; and set the score back to zero. Bummer.
    (unless players
      (reset-score-to-zero game)
      (spawn-player game))

    ;; 5. If enough time as passed, spawn an enemy
    (decf enemy-spawn-timer)
    (when (zerop enemy-spawn-timer)
      (setf enemy-spawn-timer (1+ (random 120)))
      (spawn-enemy game))

    t))



