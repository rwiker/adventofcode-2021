(in-package #:aoc2021)

(defpackage #:aoc2021/day5
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day5)

(defparameter *filename-5-0* (merge-pathnames "input-5-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-5-1* (merge-pathnames "input-5-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line
          for segment = (cl-ppcre:register-groups-bind (('parse-integer x1 y1 x2 y2))
                            ("(\\d+),(\\d+) -> (\\d+),(\\d+)" line)
                          (list x1 y1 x2 y2))
          for (x1 y1 x2 y2) = segment
          maximizing (max x1 x2) into xmax
          maximizing (max y1 y2) into ymax
          collect segment into segments
          finally (return (values xmax ymax segments)))))

(defun build-grid-1 (filename)
  (multiple-value-bind (xmax ymax segments)
      (read-input filename)
    (let ((grid (make-array (list (1+ ymax) (1+ xmax)) :element-type 'integer :initial-element 0)))
      (loop for (x1 y1 x2 y2) in segments
            do (cond ((= x1 x2)
                      (loop for y from (min y1 y2) upto (max y1 y2)
                            do (incf (aref grid y x1))))
                     ((= y1 y2)
                      (loop for x from (min x1 x2) upto (max x1 x2)
                            do (incf (aref grid y1 x))))
                     (t
                      nil)))
      grid)))

(defun print-grid (grid &optional (stream *standard-output*))
  (loop for y below (array-dimension grid 0)
        do (progn
             (terpri stream)
             (loop for x below (array-dimension grid 1)
                   for count = (aref grid y x)
                   do (write-char (if (zerop count) #\. (code-char (+ (char-code #\0) count))) stream)))))


#||
(read-input *filename-5-0*)
(build-grid *filename-5-0*)
(print-grid (build-grid *filename-5-0*))
||#


(defun day-5-1 (filename)
  (let ((grid (build-grid-1 filename)))
  (loop for y below (array-dimension grid 0)
        summing (loop for x below (array-dimension grid 1)
                      counting (>= (aref grid y x) 2)))))


#||
(assert (= (day-5-1 *filename-5-0*) 5))
(day-5-1 *filename-5-1*)
||#

(defun build-grid-2 (filename)
  (multiple-value-bind (xmax ymax segments)
      (read-input filename)
    (let ((grid (make-array (list (1+ ymax) (1+ xmax)) :element-type 'integer :initial-element 0)))
      (loop for (x1 y1 x2 y2) in segments
            do (cond ((= x1 x2)
                      (loop for y from (min y1 y2) upto (max y1 y2)
                            do (incf (aref grid y x1))))
                     ((= y1 y2)
                      (loop for x from (min x1 x2) upto (max x1 x2)
                            do (incf (aref grid y1 x))))
                     ((= (abs (- x2 x1))
                         (abs (- y2 y1)))
                      (let ((y-step (if (> y2 y1) 1 -1))
                            (x-step (if (> x2 x1) 1 -1)))
                        (loop for x = x1 then (+ x x-step)
                              for y = y1 then (+ y y-step)
                              do (incf (aref grid y x))
                              until (= x x2))))
                     (t
                      nil)))
      grid)))

#||
(print-grid (build-grid-2 *filename-5-0*))
(print-grid (build-grid-2 *filename-5-1*))
||#

(defun day-5-2 (filename)
  (let ((grid (build-grid-2 filename)))
    (loop for y below (array-dimension grid 0)
          summing (loop for x below (array-dimension grid 1)
                        counting (>= (aref grid y x) 2)))))

#||
(assert (= (day-5-2 *filename-5-0*) 12))
(day-5-2 *filename-5-1*)
||#

