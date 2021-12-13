(in-package #:aoc2021)

(defpackage #:aoc2021/day6
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day6)

(defparameter *filename-6-0* (merge-pathnames "input-6-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-6-1* (merge-pathnames "input-6-1.txt" (asdf:system-source-directory :aoc2021)))

(defclass school ()
  ((counts :accessor counts :initform (make-array '(9) :element-type 'integer :initial-element 0))
   (total :accessor total :initform 0)))

(defmethod print-object ((school school) stream)
  (print-unreadable-object (school stream :type t)
    (format stream "Total: ~d~%" (total school))
    (loop for i from 8 downto 0 do
          (format stream "~d: ~d~%" i (aref (counts school) i)))))

(defun read-input (filename)
  (let ((school (make-instance 'school)))
    (with-open-file (f filename)
      (let ((numbers (mapcar 'parse-integer (cl-ppcre:split "," (read-line f)))))
        (dolist (n numbers)
          (incf (total school))
          (incf (aref (counts school) n)))))
    school))

#||
(read-input *filename-6-0*)
||#

(defun step-school (school)
  (with-slots (counts total)
      school
    (let ((new-fishes (aref counts 0)))
      (loop for i from 1 upto 8
            do (setf (aref counts (1- i))
                     (aref counts i)))
      (setf (aref counts 8) new-fishes)
      (incf (aref counts 6) new-fishes)
      (incf total new-fishes))))

(defun day-6-1 (filename)
  (let ((school (read-input filename)))
    (dotimes (n 80)
      (step-school school))
    (total school)))

#||
(assert (= (day-6-1 *filename-6-0*) 5934))
(day-6-1 *filename-6-1*)
||#

(defun day-6-2 (filename)
  (let ((school (read-input filename)))
    (dotimes (n 256)
      (step-school school))
    (total school)))

#||
(assert (= (day-6-2 *filename-6-0*) 26984457539)))

(time (day-6-2 *filename-6-1*))
||#
