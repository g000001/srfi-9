;;;; srfi-9.asd

(cl:in-package :asdf)

(defsystem :srfi-9
    :version "20200120"
    :description
    "SRFI 9 for CL: Defining Record Types"
    :long-description
    "SRFI 9 for CL: Defining Record Types
https://srfi.schemers.org/srfi-9/"
    :author "CHIBA Masaomi"
    :maintainer "CHIBA Masaomi"
    :license "Unlicense"
    :serial t
    :depends-on (:mbe :fiveam)
    :components ((:file "package")
                 (:file "utils")
                 (:file "srfi-9")))

(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-9))))
  (let ((name "https://github.com/g000001/srfi-9") (nickname :srfi-9))
    (if (and (find-package nickname)
             (not (eq (find-package nickname) (find-package name))))
        (warn "~A: A package with name ~A already exists."
              name nickname)
        (rename-package name name `(,nickname)))))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-9))))
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall (_ :fiveam :run)
                               (_ "https://github.com/g000001/srfi-9#internals" :srfi-9))))
          (funcall (_ :fiveam :explain!) result)
          (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
