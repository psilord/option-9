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
    ;; If the mouse cursor isnot shown, show it when we enter the
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

(defun display ()
  (gl:clear :color-buffer-bit)
  (render-game *game* `(.01 .01)))

(defun option-9 ()
  (format t "Welcome to Option 9, Version 0.2!~%")
  (format t "A space shoot'em up game written in CLOS.~%")
  (format t "Written by Peter Keller <psilord@cs.wisc.edu>~%")
  (format t "Ship Designs by Stephanie Keller <aset_isis@hotmail.com>~%")

  (with-game-init ("option-9.dat")
    (reset-score-to-zero *game*)
    (spawn-player *game*)
    (sdl:with-init ()
      (sdl:window 640 640
                  :title-caption "Option 9 Version 0.2"
                  :icon-caption "Option 9"
                  :opengl t
                  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
                  :fps (make-instance 'sdl:fps-fixed :target-frame-rate 60))
      (sdl:show-cursor nil)
      (gl:clear-color 0 0 0 0)
      ;; Initialize viewing values.
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
                         ;;(format t "Key down: ~S~%" key)
                         (case key
                           (:sdl-key-e (text-console))
                           (:sdl-key-q (sdl:push-quit-event))
                           (:sdl-key-space
                            (shoot (car (players *game*))))
                           (:sdl-key-up (move-player *game* :begin :up))
                           (:sdl-key-down (move-player *game* :begin :down))
                           (:sdl-key-left (move-player *game* :begin :left))
                           (:sdl-key-right (move-player *game* :begin :right))))
        (:key-up-event (:key key)
                       ;; be careful to only zero out the movement if
                       ;; I got a key up event in the direction I'm
                       ;; already moving. This algorithm makes it very
                       ;; smooth in the play control when smashing
                       ;; buttons down for movement.
                       (case key
                         (:sdl-key-up (move-player *game* :end :up))
                         (:sdl-key-down (move-player *game* :end :down))
                         (:sdl-key-left (move-player *game* :end :left))
                         (:sdl-key-right (move-player *game* :end :right))))
        (:idle ()
               (step-game *game*)
               (display)

               ;; Start processing buffered OpenGL routines.
               (gl:flush)
               (sdl:update-display))))))
