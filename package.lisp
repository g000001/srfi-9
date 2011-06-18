;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-9
  (:use)
  (:export :define-record-type))

(defpackage :srfi-9-internal
  (:use :srfi-9 :cl :fiveam :mbe))

