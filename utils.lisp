(cl:in-package :srfi-9-internal)

(defun vector-length (vec)
  (declare (optimize (safety 3) (speed 3)))
  (declare (vector vec))
  (length vec))

(defun vector-ref (vec index)
  (declare (optimize (safety 3) (speed 3)))
  (declare (vector vec)
           (fixnum index))
  (aref vec index))

(defun vector-set! (vec index value)
  (declare (optimize (safety 3) (speed 3)))
  (declare (vector vec)
           (fixnum index))
  (setf (aref vec index) value))

