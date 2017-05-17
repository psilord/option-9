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

(defpackage #:option-9
  (:use #:cl)
  (:export #:option-9))

(in-package #:option-9)

#+option-9-debug (declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defparameter *game* nil)
(defparameter *assets* nil)
(defparameter *id* nil)

(defun new-id ()
  (let ((id *id*))
    (incf *id*)
    id))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-accessor-symbol (prefix-symbol &rest args)
    "Create a symbol suitable for an accessor in with-p* macros used in
the math API. PREFIX-SYMBOL _must_ be a symbol so we can discover the
package it is from. ARGS can be anything else that will all end up
stringified into a symbol interned into the symbol package of PREFIX-SYMBOL."
    ;; Thank you pjb!
    (intern (format nil "~:@(~{~A~}~)" (cons prefix-symbol args))
            (symbol-package prefix-symbol))))


;; lifted from Graham's ANSI Common Lisp book
(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr)
                   expr
                   (expand-call type (binarize expr)))))

(defun expand-call (type expr)
  `(,(car expr) ,@(mapcar #'(lambda (a)
                              `(with-type ,type ,a))
                          (cdr expr))))

(defun binarize (expr)
  (if (and (nthcdr 3 expr) (member (car expr) '(+ - * /)))
      (destructuring-bind (op a1 a2 . rest) expr
        (binarize `(,op (,op ,a1 ,a2) ,@rest)))
      expr))

(defmacro as-double-float (expr)
  `(with-type double-float ,expr))
