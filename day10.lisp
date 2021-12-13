(in-package #:aoc2021)

(defpackage #:aoc2021/day10
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day10)

(defparameter *filename-10-0* (merge-pathnames "input-10-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-10-1* (merge-pathnames "input-10-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defparameter *delimiters*
  '((#\( #\) 3 1) (#\[ #\] 57 2) (#\{ #\} 1197 3) (#\< #\> 25137 4)))

(defun start-char-p (char)
  (member char *delimiters* :test 'char= :key 'first))

(defun end-char-p (char)
  (member char *delimiters* :test 'char= :key 'second))

(defun not-matching-p (b e)
  (let ((triple (find e *delimiters* :test 'char= :key 'second)))
    (assert triple)
    (if (char= b (first triple))
      nil
      (third triple))))

(defun analyze-line/1 (line)
  (loop with stack = nil
        for c across line
        do (if (start-char-p c)
             (push c stack)
             (let ((not-match-cost (not-matching-p (first stack) c)))
               (if not-match-cost
                 (return-from analyze-line/1 not-match-cost)
                 (pop stack)))))
  nil)

(defun day-10-1 (filename)
  (let ((lines (read-input filename)))
    (loop for line in lines
          for cost = (analyze-line/1 line)
          when cost
          summing cost)))

#||
(assert (= (day-10-1 *filename-10-0*) 26397))
(day-10-1 *filename-10-1*)
||#

(defun analyze-line/2 (line)
  (let ((stack
         (loop with stack = nil
               for c across line
               do (if (start-char-p c)
                    (push c stack)
                    (let ((not-match-cost (not-matching-p (first stack) c)))
                      (if not-match-cost
                        (return-from analyze-line/2 nil)
                        (pop stack))))
               finally (return stack))))
    (when stack
      (loop for total = 0 then (+ (* total 5) cost)
            for char in stack
            while char
            for cost = (fourth (find char *delimiters* :test 'char= :key 'first))
            finally (return total)))))

(defun day-10-2 (filename)
  (let ((lines (read-input filename)))
    (let ((costs
           (loop for line in lines
                 for cost = (analyze-line/2 line)
                 when cost
                 collect cost)))
      (let ((costs/sorted (sort costs '<)))
        (nth (truncate (length costs) 2) costs/sorted)))))
          
#||
(assert (= (day-10-2 *filename-10-0*) 288957))
(day-10-2 *filename-10-1*)
||#