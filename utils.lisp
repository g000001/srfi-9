(cl:in-package "https://github.com/g000001/srfi-9#internals")

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

