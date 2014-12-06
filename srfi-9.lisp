;;;; srfi-9.lisp

(cl:in-package :srfi-9-internal)

(def-suite srfi-9)

(in-suite srfi-9)

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor)
       predicate
       (field-tag accessor . more) ***)
     (progn
       (defstruct (type
                    (:constructor constructor ())
                    (:predicate predicate)
                    (:conc-name ""))
         accessor ***)
       (define-modifier accessor . more)
       ***))
    ((define-record-type type
       (constructor constructor-tag ***)
       predicate
       (field-tag accessor . more) ***)
     (progn
       (defstruct (type
                    (:constructor constructor (constructor-tag *** &aux (accessor field-tag) ***))
                    (:predicate predicate)
                    (:conc-name ""))
         accessor ***)
       (define-modifier accessor . more)
       ***))))

(define-syntax define-modifier
  (syntax-rules ()
    ((define-modifier accessor)
     nil)
    ((define-modifier accessor modifier)
     (setf (symbol-function 'modifier)
           (lambda (obj val)
             (setf (accessor obj) val))))))


(test define-record-type
  (define-record-type pare
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))
  (is-true (typep (kons 1 2) 'pare))
  (is-true (pare? (kons 1 2)))
  (let ((kons (kons 1 2)))
    (is (= 1 (kar kons)))
    (is (= 2 (kdr kons))))
  (let ((kons (kons 1 2)))
    (set-kar! kons 100)
    (is (= 100 (kar kons)))
    (is (= 2 (kdr kons))))
  ;; clean up
  (map nil #'unintern '(pare kons pare? kar set-kar! kdr)))
