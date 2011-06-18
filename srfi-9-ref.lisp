;;;; srfi-9.lisp

(cl:in-package :srfi-9-internal)

(def-suite srfi-9)

(in-suite srfi-9)

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ***)
       predicate
       (field-tag accessor . more) ***)
     (progn
       (setf (symbol-value (quote type))
             (make-record-type 'type '(field-tag ***)))
       (setf (symbol-function (quote constructor))
             (record-constructor type '(constructor-tag ***)))
       (setf (symbol-function (quote predicate))
             (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ***))))

; An auxilliary macro for define field accessors and modifiers.
; This is needed only because modifiers are optional.

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (setf (symbol-function (quote accessor))
           (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (progn
       (setf (symbol-function 'accessor)
             (record-accessor type 'field-tag))
       (setf (symbol-function 'modifier)
             (record-modifier type 'field-tag))))))



; This implements a record abstraction that is identical to vectors,
; except that they are not vectors (VECTOR? returns false when given a
; record and RECORD? returns false when given a vector).  The following
; procedures are provided:
;   (record? <value>)                -> <boolean>
;   (make-record <size>)             -> <record>
;   (record-ref <record> <index>)    -> <value>
;   (record-set! <record> <index> <value>) -> <unspecific>
;
; These can implemented in R5RS Scheme as vectors with a distinguishing
; value at index zero, providing VECTOR? is redefined to be a procedure
; that returns false if its argument contains the distinguishing record
; value.  EVAL is also redefined to use the new value of VECTOR?.

; Define the marker and redefine VECTOR? and EVAL.

(defconstant record-marker
  (if (boundp 'record-marker)
      (symbol-value 'record-marker)
      (list 'record-marker)))

(defun real-vector? (vec)
  (vectorp vec))

(defun vector? (x)
  (and (real-vector? x)
       (or (= 0 (vector-length x))
	   (not (eq (vector-ref x 0)
                    record-marker)))))

; This won't work if ENV is the interaction environment and someone has
; redefined LAMBDA there.

#|(define eval
  (let ((real-eval eval))
    (lambda (exp env)
      ((real-eval `(lambda (vector?) ,exp))
       vector?))))|#

; Definitions of the record procedures.

(defun record? (x)
  (and (real-vector? x)
       (< 0 (vector-length x))
       (eq (vector-ref x 0)
           record-marker)))

(defun record-set! (record index value)
  (vector-set! record (+ index 1) value))

(defun record-ref (record index)
  (vector-ref record (+ index 1)))

(defun make-record (size)
  (let ((new (make-array (+ size 1))))
    (vector-set! new 0 record-marker)
    new))

;=> #(#1=(RECORD-MARKER) #2=#(#1# #2# :RECORD-TYPE (NAME FIELD-TAGS)) PARE (X Y))

;Record types

; We define the following procedures:
;
; (make-record-type <type-name <field-names>)    -> <record-type>
; (record-constructor <record-type<field-names>) -> <constructor>
; (record-predicate <record-type>)               -> <predicate>
; (record-accessor <record-type <field-name>)    -> <accessor>
; (record-modifier <record-type <field-name>)    -> <modifier>
;   where
; (<constructor> <initial-value> ***)         -> <record>
; (<predicate> <value>)                       -> <boolean>
; (<accessor> <record>)                       -> <value>
; (<modifier> <record> <value>)         -> <unspecific>

; Record types are implemented using vector-like records.  The first
; slot of each record contains the record's type, which is itself a
; record.

(defun record-type (record)
  (record-ref record 0))

;----------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;  (define-record-type :record-type
;    (make-record-type name field-tags)
;    record-type?
;    (name record-type-name)
;    (field-tags record-type-field-tags))
; As it is, we need to define everything by hand.

(defvar |:record-type| (make-record 3))
(record-set! |:record-type| 0 |:record-type|) ; Its type is itself.
(record-set! |:record-type| 1 ':record-type)
(record-set! |:record-type| 2 '(name field-tags))

; Now that :record-type exists we can define a procedure for making more
; record types.

(defun make-record-type (name field-tags)
  (let ((new (make-record 3)))
    (record-set! new 0 |:record-type|)
    (record-set! new 1 name)
    (record-set! new 2 field-tags)
    new))

; Accessors for record types.

(defun record-type-name (record-type)
  (record-ref record-type 1))

(defun record-type-field-tags (record-type)
  (record-ref record-type 2))

;----------------
; A utility for getting the offset of a field within a record.

(defun field-index (type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null tags)
           (error "record type has no such field" type tag))
          ((eq tag (car tags))
           i)
          (:else
           (loop (+ i 1) (cdr tags))))))

;----------------
; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(defun record-constructor (type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (mapcar (lambda (tag)
                        (field-index type tag))
                      tags)))
    (lambda (&rest args)
      (if (= (length args)
             arg-count)
          (let ((new (make-record (+ size 1))))
            (record-set! new 0 type)
            (mapc (lambda (arg i)
                    (record-set! new i arg))
                  args
                  indexes)
            new)
          (error "wrong number of arguments to constructor" type args)))))

(defun record-predicate (type)
  (lambda (thing)
    (and (record? thing)
         (eq (record-type thing)
             type))))

(defun record-accessor (type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
               (eq (record-type thing)
                    type))
          (record-ref thing index)
          (error "accessor applied to bad value" type tag thing)))))

(defun record-modifier (type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
               (eq (record-type thing)
                   type))
          (record-set! thing index value)
          (error "modifier applied to bad value" type tag thing)))))


