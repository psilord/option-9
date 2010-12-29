(defpackage #:option-9
  (:use #:cl)
  (:export #:option-9))

(in-package #:option-9)

(defparameter *game* nil)
(defparameter *all-entities* nil)

;; From LOL
(defun group (lst n)
  (when (zerop n) (error "A zero group size is illegal"))
  (labels ((rec (lst acc)
             (let ((rst (nthcdr n lst)))
               (if (consp rst)
                   (rec rst (cons (subseq lst 0 n)
                                  acc))
                   (nreverse
                    (cons lst acc))))))
    (if lst (rec lst nil) nil)))
