;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-9
  (:use)
  (:export :define-record-type))

(defpackage :srfi-9-internal
  (:use :srfi-9 :cl :fiveam :mbe)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :loop))

