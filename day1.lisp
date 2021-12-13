(in-package #:aoc2021)

(defpackage #:aoc2021/day1
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day1)

(defparameter *filename-1-0* (merge-pathnames "input-1-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-1-1* (merge-pathnames "input-1-1.txt" (asdf:system-source-directory :aoc2021)))

(defun day-1-1 (filename)
  (with-open-file (f filename :direction :input)
    (loop for prev = (parse-integer (read-line f nil)) then next
          for line = (read-line f nil)
          for next = (and line (parse-integer line))
          while line
          summing (if (> next prev) 1 0))))

#||
(assert (= 7 (day-1-1 *filename-1-0*)))
(day-1-1 *filename-1-1*)
||#

(defun day-1-2 (filename)
  (with-open-file (f filename :direction :input)
    (flet ((get-next ()
             (let ((l (read-line f nil)))
               (and l (parse-integer l)))))
      (loop for prev = nil then next
            for a = (get-next) then b
            for b = (get-next) then c
            for c = (get-next)
            while c
            for next = (+ a b c)
            counting (and prev (> next prev))))))

#||
(assert (= (day-1-2 *filename-1-0*) 5))
(day-1-2 *filename-1-1*)
||#
