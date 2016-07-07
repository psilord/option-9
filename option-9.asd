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

(defpackage #:option-9-asd
  (:use :cl :asdf))

(in-package #:option-9-asd)

(defun announce-and-push-feature (feature)
  (format t "~&Adding ~A to *features*~%" feature)
  (finish-output)
  (pushnew feature *features*))

#+sbcl (progn
         ;; (announce-and-push-feature :option-9-optimize-pvec)
         ;; (announce-and-push-feature :option-9-optimize-pmat)
         (announce-and-push-feature :option-9-debug))
#+ccl (progn
        (announce-and-push-feature :option-9-optimize-pvec)
        (announce-and-push-feature :option-9-optimize-pmat))
#-(or sbcl ccl) (error "Only SBCL and CCL are supported")

;; The defauls on SBCL are sometimes not good enough to expand the
;; optimization macro passes in certain files. So fix it here and apply
;; to each file as needed.
(defun using-better-limits (thunk)
  (let (#+sbcl (sb-ext::*inline-expansion-limit* 1024) )
    #+sbcl (format t "; Setting higher SBCL limits to compile this file...~%")
    (funcall thunk)))

(defsystem #:option-9
  :description "A simple game"
  :version "0.1"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "Apache License, Version 2.0"

  :serial t
  :depends-on (#:alexandria #:local-time #:cl-opengl #:sdl2)
  :components ((:file "package")
               (:file "constants")
               (:file "classes")
               (:file "generic-functions")
               (:file "pvec")
               (:file "pmat" :around-compile using-better-limits)
               (:file "pquat")
               (:file "pdquat")
               (:file "utils")
               (:file "field")
               (:file "methods")
               (:file "status")
               (:file "explode")
               (:file "damage")
               (:file "die")
               (:file "sparks")
               (:file "render")
               (:file "spawn")
               (:file "collide")
               (:file "perform-collide")
               (:file "absorbs")
               (:file "shoot")
               (:file "think")
               (:file "idea")
               (:file "increase-power")
               (:file "game")
               (:file "assets")
               (:file "active-step-once")
               (:file "passive-step-once")
               (:file "generate")
               (:file "events")
               (:file "turrets")
               (:file "scene-man")
               (:file "option-9")))
