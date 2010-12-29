(in-package #:option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defun text-console ()
  (format t "Welcome to Text Console~%")
  (let ((run-repl t))
    (loop while run-repl
       do
       (format t "> ")
       (finish-output)
       (let ((result (eval (read))))
         (format t "~%~S~%" result)
         (when (eq result :q)
           (setf run-repl nil))))
    (format t "Resuming game.~%")))

(defun display ()
  (gl:clear :color-buffer-bit)
  (render-game *game* `(.01 .01)))

(defun option-9 ()
  (with-game-init ("option-9.dat")
    (reset-score-to-zero *game*)
    (spawn-player *game*)
    (spawn-enemy *game*)
    (sdl:with-init ()
      (sdl:window 640 640
                  :title-caption "Game 2"
                  :icon-caption "Game 2"
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
                            (fires-ship (car (game-players *game*))))
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
