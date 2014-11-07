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

;; Written by pjb with minor modification for finishing the output
;; from me.
(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition
         (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&")
       (finish-output))
     (condition
         (err)
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err)
       (finish-output))))

;; Written by pjb with minor modification for finishing the output and
;; starting from zero in the history.
(defun repl ()
  (do ((+eof+ (gensym))
       (hist 0 (1+ hist)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (finish-output)
    (handling-errors
      (setf +++ ++
            ++ +
            + -
            - (read *standard-input* nil +eof+))
      (when (or (eq - +eof+)
                (member - '((quit) (exit) (continue)) :test #'equal))
        (return-from repl))
      (setf /// //
            // /
            / (multiple-value-list (eval -)))
      (setf *** **
            ** *
            * (first /))
      (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
      (finish-output))))

;; Eval any typed in expressions in the option-9 package.
(defun text-console ()
  (format t "Welcome to Text Console~%")
  (let ((*package* (find-package 'option-9)))
    ;; If the mouse cursor is not shown, show it when we enter the
    ;; console.  Otherwise it freaks me out and I think my machine is
    ;; hung.
    (let* ((sdl-initp (sdl:initialized-subsystems-p))
           (cursor-not-shown (not (sdl:query-cursor))))
      (unwind-protect
           (progn
             (when (and sdl-initp cursor-not-shown)
               (sdl:show-cursor t))
             (repl))
        (when (and sdl-initp cursor-not-shown)
          (sdl:show-cursor nil)))
      (format t "Resuming game.~%"))))


#+ignore(defun option-9-profiled ()
          (sb-sprof:profile-call-counts "OPTION-9")
          (sb-sprof:with-profiling (:max-samples 1000000
                                                 :report :flat
                                                 :loop nil)
            (option-9)))

(defun initialize-joysticks ()
  (let ((num-sticks (sdl:num-joysticks)))
    (cond
      ((zerop num-sticks)
       (format t "No joysticks available. Keyboard control only.~%"))
      (t
       (format t "Using only joystick zero: ~A~%" (sdl:sdl-joystick-name 0))
       (sdl-cffi::sdl-joystick-open 0)))))


(defun option-9 ()
  (format t "Welcome to Option 9, Version 0.9!~%")
  (format t "A space shoot'em up game written in CLOS.~%")
  (format t "Written by Peter Keller <psilord@cs.wisc.edu>~%")
  (format t "Ship Designs by Stephanie Keller <aset_isis@hotmail.com>~%")

  (with-game-init ("option-9.dat")
    (sdl:with-init (sdl:sdl-init-everything)
      (sdl:window 700 700
                  :title-caption "Option 9 Version 0.9"
                  :icon-caption "Option 9"
                  :opengl t
                  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
                  :fps (make-instance 'sdl:fps-fixed :target-frame-rate 60))
      (sdl:show-cursor nil)
      (gl:clear-color 0 0 0 0)
      ;; Initialize viewing values.
      (gl:matrix-mode :projection)
      (gl:load-identity)
      ;; The world is (0,0) to (100,100)
      (gl:ortho 0 100 0 100 -1 1)

      (initialize-joysticks)

      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
                         ;; (format t "Key down: ~S~%" key)
                         (case key
                           (:sdl-key-p (toggle-paused *game*))
                           (:sdl-key-e (text-console))
                           (:sdl-key-q (sdl:push-quit-event))
                           (:sdl-key-space
                            (let ((player (car (entities-with-role
                                                (scene-man *game*) :player))))
                              (when player
                                (shoot player :front-weapon-port))))
                           (:sdl-key-z
                            (let ((player (car (entities-with-role
                                                (scene-man *game*) :player))))
                              (when player
                                (shoot player :left-weapon-port))))
                           (:sdl-key-d
                            (let ((player (car (entities-with-role
                                                (scene-man *game*) :player))))
                              (when player
                                (shoot player :center-weapon-port))))
                           (:sdl-key-x
                            (let ((player (car (entities-with-role
                                                (scene-man *game*) :player))))
                              (when player
                                (shoot player :rear-weapon-port))))
                           (:sdl-key-c
                            (let ((player (car (entities-with-role
                                                (scene-man *game*) :player))))
                              (when player
                                (shoot player :right-weapon-port))))
                           (:sdl-key-up
                            (move-player-keyboard *game* :begin :up))
                           (:sdl-key-down
                            (move-player-keyboard *game* :begin :down))
                           (:sdl-key-left
                            (move-player-keyboard *game* :begin :left))
                           (:sdl-key-right
                            (move-player-keyboard *game* :begin :right))))
        (:key-up-event (:key key)
                       ;; (format t "Key up: ~S~%" key)
                       (case key
                         (:sdl-key-up
                          (move-player-keyboard *game* :end :up))
                         (:sdl-key-down
                          (move-player-keyboard *game* :end :down))
                         (:sdl-key-left
                          (move-player-keyboard *game* :end :left))
                         (:sdl-key-right
                          (move-player-keyboard *game* :end :right))))

        (:joy-axis-motion-event (:which which :axis axis :value value)
                                (assert (= which 0))
                                ;; XXX crappy zeroing to get rid of jitter.
                                ;; need a batter calibration algorithm.
                                (when (and (or (= axis 0) (= axis 1)) nil)
                                  (let ((val (/ value 32768d0)))
                                    (if (or (> val 7d0)
                                            (< val -7d0))
                                        (move-player-joystick *game* axis val)
                                        (move-player-joystick *game* axis 0d0)))))

        (:joy-button-down-event (:which which :button button :state state)
                                (format t "JOY BUTTON DN: ~A ~A ~A~%"
                                        which button state)
                                (cond
                                  ((= button 0)
                                   (let ((player (car (entities-with-role
                                                       (scene-man *game*)
                                                       :player))))
                                     (when player
                                       (shoot player :front-turret-port))))
                                  ((= button 6)
                                   (sdl:push-quit-event))))

        (:joy-button-up-event (:which which :button button :state state)
                              (format t "JOY BUTTON UP: ~A ~A ~A~%"
                                      which button state))

        ;; :axis instead of :hat. Stupid wrong docs.
        (:joy-hat-motion-event (:which which :axis axis :value value)
                               (format t "JOY HAT: ~A ~A ~A~%"
                                       which axis value))


        (:idle ()
               (step-game *game*)
               (display *game*)

               ;; Start processing buffered OpenGL routines.
               (gl:flush)
               (sdl:update-display))))))
