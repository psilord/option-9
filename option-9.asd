(defpackage #:option-9-asd
  (:use :cl :asdf))

(in-package #:option-9-asd)

(defsystem #:option-9
  :description "A simple game"
  :version "0.0"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "Apache License, Version 2.0"


  :depends-on (#:cl-opengl #:lispbuilder-sdl)
  :components ((:file "package")
	       (:file "game"
		      :depends-on ("package"))
	       (:file "entity"
		      :depends-on ("game"))
               (:file "option-9"
                      :depends-on ("package" "entity" "game"))))
