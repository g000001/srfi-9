;;;; srfi-9.asd

(cl:in-package :asdf)

(defsystem :srfi-9
  :serial t
  :depends-on (:mbe :srfi-5 :srfi-23)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-9")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-9))))
  (load-system :srfi-9)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-9-internal :srfi-9))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

