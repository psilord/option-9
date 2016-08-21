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

;; Plot, to be shown to the player at start of game: "Supreme
;; Commander Snarkill of the Snarlian Empire owes you 9 bucks and has
;; refused to pay. The only response to this affront is total
;; annihilation of the Snarlian Empire. You commence now."

(in-package #:option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

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
  (format t "Welcome to Text Console, (quit) exits the console!~%")
  (let ((*package* (find-package 'option-9)))
    ;; If the mouse cursor is not shown, show it when we enter the
    ;; console.  Otherwise it freaks me out and I think my machine is
    ;; hung.
    (unwind-protect
         (progn
           (sdl2:show-cursor)
           (repl))
      (sdl2:hide-cursor))
    (format t "Exiting Text Console.~%")))

#+-
(defun option-9-profiled ()
  (sb-sprof:profile-call-counts "OPTION-9")
  (sb-sprof:with-profiling (:max-samples 1000000
                                         :mode :time
                                         :report :flat
                                         :loop nil)
    (option-9)))

(defun initialize-joysticks ())
;; (let ((num-sticks (sdl2:joystick-count)))
;;   (cond
;;     ((zerop num-sticks)
;;      (format t "No joysticks available. Keyboard control only.~%"))
;;     (t
;;      (dotimes (controller-index num-sticks)
;;       (when (sdl2:game-controller-p controller-index)
;;         (let* ((controller-name (sdl2:game-controller-name-for-index controller-index))
;;                (game-controller (sdl2:game-controller-open controller-index))
;;                (joystick (sdl2:game-controller-get-joystick game-controller)))
;;           (format t "Controller ~A: ~A, haptic=~A~%"
;;                   controller-index
;;                   controller-name
;;                   has-haptic)))))))

(defun set-initial-game-window-size (game)
  (labels ((set-window-size (max-width max-height resolutions)
             (let ((width (caar resolutions))
                   (height (cdar resolutions)))
               (when resolutions
                 (if (and (< width max-width)
                          (< height max-height))
                     (setf (window-width game) width
                           (window-height game) height)
                     (set-window-size max-width max-height (cdr resolutions)))))))
    (multiple-value-bind (format
                          width
                          height) (sdl2:get-current-display-mode 0)
      (declare (ignorable format))
      (set-window-size width height *windowed-modes*))))

(defun game-main (&optional (ortho-p t))
  (format t "Welcome to Option 9, Version 0.9!~%")
  (format t "A space shoot'em up game written in CLOS.~%")
  (format t "Written by Peter Keller <psilord@cs.wisc.edu>~%")
  (format t "Ship Designs by Stephanie Keller <sakeller42@gmail.com>~%~%")

  (format t "Instructions:
  Arrow keys         Move as expected
  Spacebar           Fire (Hold to charge if you have the powerups)
  q                  Quits
  e                  Enter repl. (quit) ends it [and then unpause to play].
  p                  Pause/Unpause
  d                  Drop mines (if you have any)

Powerups:
  Spiral             Shield which protects only against shots. 5 charges.
  Triangle           Shield which protects against ships and shots. 5 charges.
  Hourglass          Restore some energy.
  Hash               Proximity Mines (recharge to hold up to 5).
  Red H in diamond   Hardnose-Shot (penetrate other shots).
  Green S in diamond Super-Shot (penetrate other shots AND ships).
  Asterisk           Telsa Weapon (get 7 of these in a row!).
  Downward Arrow     Charged shot. Get more of these for faster charging.
                     Hold space to charge the weapon and release to fire. If
                     you get enough of these, then it'll charge to max by the
                     time you release the spacebar to fire! Useful!

  Good luck!~%~%")


  (with-game-init ("option-9.dat")
    (sdl2:with-init (:video :gamecontroller :noparachute)
      (set-initial-game-window-size *game*)
      ;; for post processing smoothing of the lines and whantot.
      ;; set before I make the window and OpenGL context.
      ;;(sdl2:gl-set-attr :multisamplebuffers 1)
      ;;(sdl2:gl-set-attr :multisamplesamples 4)
      (sdl2:with-window (game-window :title "Option 9 Version 0.9"
                                     :w (window-width *game*)
                                     :h (window-height *game*)
                                     :flags '(:shown :opengl))
        ;; Set SDL GL attributes here...
        (sdl2:gl-set-attr :doublebuffer 1)
        (sdl2:gl-set-attr :depth-size 24)
        (sdl2:with-gl-context (gl-context game-window)
          (sdl2:gl-make-current game-window gl-context)
          (sdl2:gl-set-swap-interval 1) ;; Turn on vsync
          (sdl2:hide-cursor)
          (gl:clear-color 0 0 0 0)

          ;; Experimental messing about with a non-ortho camera view
          (gl:matrix-mode :projection)
          (if ortho-p
              ;; Set up my own orthographic projection matrix
              (gl:load-matrix
               (matrix-convert-to-opengl
                (matrix-orthographic-projection
                 0d0 (coerce (game-width *game*) 'double-float)
                 0d0 (coerce (game-height *game*) 'double-float)
                 -1d0 1d0)))
              ;; Or a perspective camer projection matrix.
              (gl:load-matrix
               (matrix-convert-to-opengl
                (matrix-perspective-projection
                 -50d0 50d0 #+ignore(coerce (* (game-width *game*)
                                               (window-aspect-ratio *game*)) 'double-float)
                 -50d0 50d0 #+ignore(coerce (game-height *game*) 'double-float)
                 50d0 256d0))))

          (gl:matrix-mode :modelview)
          (if ortho-p
              ;; identity camera for orthographic
              (gl:load-identity)
              ;; Or a placed 3D Camera to show the illusion of the
              ;; playing field.
              (gl:load-matrix
               (matrix-convert-to-opengl
                (matrix-create-view
                 (mm (mtr (pvec 50d0 -10d0 62d0))
                     (mra (/ pi 4d0) (pvec 1d0 0d0 0d0)))
                 :z
                 :y))))

          ;; nice antialiased lines, given the multisampling stuff.
          ;;(gl:enable :multisample)

          (initialize-joysticks)

          (format t "Monitor refresh rate is: ~A hz~%"
                  (get-monitor-refresh-rate))

          (let ((emit-fps-p nil)
                (now (local-time:now))
                (previous-time (local-time:now))
                (dt-accum 0)
                (frame-count 0)
                (frame-count-fps 0)
                (frame-time-accum 0)
                (frank-delta-buffer 0)
                (frank-frame-count 0)
                (last-coeff 0d0)
                (frank-previous-delta 0))
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:keydown (:keysym keysym)
                        (let ((scancode (sdl2:scancode-value keysym)))
                          (cond
                            ((sdl2:scancode= scancode :scancode-space)
                             ;; start charging
                             (let ((player (car (entities-with-role
                                                 (scene-man *game*) :player))))
                               (when player
                                 (start-charging player :front-weapon-port)
                                 )))
                            ((sdl2:scancode= scancode :scancode-p) ; Pause Game
                             (toggle-paused *game*))
                            ((sdl2:scancode= scancode :scancode-e) ; Enter Common Lisp REPL
                             (format t "Pausing game....~%")
                             (toggle-paused *game*)
                             (text-console)
                             (format t "Unpause to keep playing!~%"))
                            ((sdl2:scancode= scancode :scancode-q) ; Quit Game
                             (sdl2:push-event :quit))
                            ((sdl2:scancode= scancode :scancode-f) ; Toggle FPS
                             (setf emit-fps-p (not emit-fps-p)
                                   frame-count 0
                                   frame-time-accum 0)
                             (format t "Turned fps monitoring ~:[off~;on~]~%"
                                     emit-fps-p))
                            ((sdl2:scancode= scancode :scancode-z) ;; TEST: left weapon port fire
                             (let ((player (car (entities-with-role
                                                 (scene-man *game*) :player))))
                               (when player
                                 (shoot player :left-weapon-port))))
                            ((sdl2:scancode= scancode :scancode-d) ;; TEST: center weapon port fire
                             (let ((player (car (entities-with-role
                                                 (scene-man *game*) :player))))
                               (when player
                                 (shoot player :center-weapon-port))))
                            ((sdl2:scancode= scancode :scancode-x) ;; TEST: rear weapon port fire
                             (let ((player (car (entities-with-role
                                                 (scene-man *game*) :player))))
                               (when player
                                 (shoot player :rear-weapon-port))))
                            ((sdl2:scancode= scancode :scancode-c) ;; TEST: right weapon port fire
                             (let ((player (car (entities-with-role
                                                 (scene-man *game*) :player))))
                               (when player
                                 (shoot player :right-weapon-port))))
                            ((sdl2:scancode= scancode :scancode-up)
                             (move-player-keyboard *game* :begin :up))
                            ((sdl2:scancode= scancode :scancode-down)
                             (move-player-keyboard *game* :begin :down))
                            ((sdl2:scancode= scancode :scancode-left)
                             (move-player-keyboard *game* :begin :left))
                            ((sdl2:scancode= scancode :scancode-right)
                             (move-player-keyboard *game* :begin :right)))))
              (:keyup (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym)))
                        ;; (format t "Key up: ~S~%" key)
                        (cond
                          ((sdl2:scancode= scancode :scancode-space)
                           ;; Fire front weapon port
                           (let ((player (car (entities-with-role
                                               (scene-man *game*) :player))))
                             (when player
                               (shoot player :front-weapon-port))))
                          ((sdl2:scancode= scancode :scancode-up)
                           (move-player-keyboard *game* :end :up))
                          ((sdl2:scancode= scancode :scancode-down)
                           (move-player-keyboard *game* :end :down))
                          ((sdl2:scancode= scancode :scancode-left)
                           (move-player-keyboard *game* :end :left))
                          ((sdl2:scancode= scancode :scancode-right)
                           (move-player-keyboard *game* :end :right)))))

              #+ignore (:controlleraxismotion (:which controller-id :axis axis :value value)
                                              (when (and (or (= axis 0) (= axis 1)) nil)
                                                (let ((val (/ value 32768d0)))
                                                  (if (or (> val 7d0)
                                                          (< val -7d0))
                                                      (move-player-joystick *game* axis val)
                                                      (move-player-joystick *game* axis 0d0)))))

              #+ignore (:controllerbuttondown (:which which :button button :state state)
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
                                                 (sdl2:push-event :quit))))

              (:controllerbuttonup (:which which :button button :state state)
                                   (format t "JOY BUTTON UP: ~A ~A ~A~%"
                                           which button state))

              (:joyhatmotion (:which which :hat hat :value value)
                             (format t "JOY HAT: ~A ~A ~A~%"
                                     which hat value))

              (:idle ()
                     ;; The physics runs at 1/60th of a second time units.
                     (setf previous-time now)
                     (setf now (local-time:now))

                     (let* ((step-count 0)
                            (frame-time
                             (local-time:timestamp-difference
                              now previous-time))
                            (observed-frame-time frame-time))

                       (incf frame-count)

                       ;; Set maximum frame time in case we slow down beyond it.
                       (when (> frame-time (* *dt* 5d0))
                         (setf frame-time (* *dt* 5d0)))

                       (progn
                         ;; Do frank's algorithm
                         #+-(format t "~A frame-time ~A frank-delta-buffer ~A~%"
                                    frame-count frame-time frank-delta-buffer)
                         (incf frame-time frank-delta-buffer)
                         (setf frank-frame-count
                               (truncate (1+ (* frame-time
                                                (get-monitor-refresh-rate)))))
                         (when (<= frank-frame-count 0)
                           (setf frank-frame-count 1))
                         (setf frank-previous-delta frame-time)
                         (setf frame-time (/ frank-frame-count
                                             (get-monitor-refresh-rate)))
                         (setf frank-delta-buffer
                               (- frank-previous-delta frame-time))
                         #+-(format t "New frame-time = ~A~%" frame-time)
                         )

                       ;; accumulate the time we just spent doing the
                       ;; last frame.
                       (incf dt-accum frame-time)

                       ;; Consume the generated time in the physics
                       (loop while (>= dt-accum *dt*) do
                            (step-game *game*)
                            (incf step-count)
                            (decf dt-accum *dt*))

                       ;; Keep track of & emit stuff for FPS.
                       ;;
                       ;; TODO: Should change this to keep track of
                       ;; average usecs per frame instead.
                       #+-(when emit-fps-p
                            (incf frame-count-fps)
                            (incf frame-time-accum frame-time)
                            (when (>= frame-time-accum (in-usecs 1.0)) ;; every second..
                              (format t "frame-count = ~A frame-time-accum = ~A sec fps = ~A~%"
                                      frame-count-fps
                                      (/ frame-time-accum 1000000.0)
                                      (/ frame-count-fps (/ frame-time-accum 1000000.0)))
                              (finish-output)
                              (setf frame-count-fps 0
                                    frame-time-accum 0)))
                       #++(format t "~A ~A ~A ~A ~A ~A~%"
                                  frame-count step-count observed-frame-time frame-time dt-accum
                                  (/ dt-accum (float *dt* 1d0)))
                       ;; Compute the Rendering Interpolant to remove jutter.
                       (display *game* (lerp last-coeff (/ dt-accum (float *dt* 1d0)) .5))
                       (setf last-coeff (/ dt-accum (float *dt* 1d0)))

                       ;; Start processing buffered OpenGL routines.
                       (sdl2:gl-swap-window game-window)
                       ;;TODO: Crappy hack!
                       ;; Required to make my GTX 660 not microstutter.
                       ;; Also need the correct refresh rate for my monitor.
                       ;; This is actually horrible since it causes 100%
                       ;; cpu usage.
                      #+-(gl:finish))))))))))

(defun option-9 ()
  (sdl2:make-this-thread-main #'game-main))
