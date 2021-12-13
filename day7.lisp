(in-package #:aoc2021)

(defpackage #:aoc2021/day7
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day7)

(defparameter *filename-7-0* (merge-pathnames "input-7-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-7-1* (merge-pathnames "input-7-1.txt" (asdf:system-source-directory :aoc2021)))

#+nil
(defun read-input (filename)
  (with-open-file (f filename)
    (loop for num = (loop for num = 0 then (+ (* num 10) n)
                          for c = (read-char f nil)
                          while c
                          for n = (digit-char-p c)
                          while n
                          for found = nil then t
                          finally (return (and found num)))
          collect num)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((positions
           (mapcar 'parse-integer (cl-ppcre:split "," (read-line f)))))
      (multiple-value-bind (min max count)
          (loop for pos in positions
                maximizing pos into max
                minimizing pos into min
                counting pos into count
                finally (return (values min max count)))
        (values positions min max count)))))

(defun day-7-1 (filename)
  (multiple-value-bind (positions min max count)
      (read-input filename)
    (declare (ignore count))
    (loop for test-pos from min upto max
          for sum-moves = (loop for pos in positions
                                summing (abs (- pos test-pos)))
          minimizing sum-moves into min-sum
          finally (return min-sum))))


#||
(read-input *filename-7-0*)
(sort 
 (read-input *filename-7-0*) '<)
(assert (= (day-7-1 *filename-7-0*) 37))

(day-7-1 *filename-7-1*)
||#

(defun day-7-2 (filename)
  (multiple-value-bind (positions min max count)
      (read-input filename)
    (declare (ignore count))
    (let ((cache (make-hash-table)))
      (flet ((calc-score (n)
               (or (gethash n cache)
                   (let ((res (loop for i from 0 upto n
                                    summing i)))
                     (setf (gethash n cache) res)
                     res))))
        (loop for test-pos from min upto max
              for sum-moves = (loop for pos in positions
                                    summing (calc-score (abs (- pos test-pos))))
              minimizing sum-moves into min-sum
              finally (return min-sum))))))

#||
(assert (= (day-7-2 *filename-7-0*) 168))
(day-7-2 *filename-7-1*)
||#
