(in-package :option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

;; This will end up being the event handler that will dispatch events
;; from SDL into the game engine.

(defclass dispatcher () ())




#|
:quit

left joystick
:fly-left
:fly-right
:fly-forward
:fly-backward

right joystick

:start-charge-main-gun
:end-charge-main-gun
:shoot-main-gun
:start-charge-secondary-gun
:end-charge-secondary-gun
:shoot-secondary-gun
:pause
:drop-mine
|#

