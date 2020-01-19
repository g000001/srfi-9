;;;; package.lisp

(cl:in-package #:cl-user)

(defpackage "https://github.com/g000001/srfi-9"
  (:use)
  (:export #:define-record-type))

(defpackage "https://github.com/g000001/srfi-9#internals"
  (:use "https://github.com/g000001/srfi-9"
        #:cl #:fiveam #:mbe))

