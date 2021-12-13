(in-package #:aoc2021)

(defpackage #:aoc2021/day9
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day9)

(defparameter *filename-9-0* (merge-pathnames "input-9-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-9-1* (merge-pathnames "input-9-1.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-9-2* (merge-pathnames "input-9-2.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((data 
           (loop for line = (read-line f nil)
                 while line
                 collect (map 'list 'digit-char-p line))))
      (make-array (list (length data) (length (first data)))
                  :element-type 'integer :initial-contents data))))
      
#||
(read-input *filename-9-0*)
(array-dimensions (read-input *filename-9-1*))

||#

(defun risk-point-p (data y x)
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1))
        (val (aref data y x)))
    (labels ((test-point (y x)
               (or (minusp y)
                   (minusp x)
                   (>= y ny)
                   (>= x nx)
                   (> (aref data y x) val))))
      (let ((res (and (test-point (1- y) x)
                      (test-point (1+ y) x)
                      (test-point y (1- x))
                      (test-point y (1+ x)))))
        res))))

(defun day-9-1 (filename)
  (let ((data (read-input filename)))
    (let ((ny (array-dimension data 0))
          (nx (array-dimension data 1)))
      (loop for y below ny
            summing (loop for x below nx
                          for val = (aref data y x)
                          when (risk-point-p data y x)
                          summing (1+ val))))))

#||

(assert (= (day-9-1 *filename-9-0*) 15))
(day-9-1 *filename-9-2*)
(defparameter *data-1* (read-input *filename-9-1*))

(day-9-1 *filename-9-1*)
||#


(defun find-basin (data y x)
  (let ((max-y (1- (array-dimension data 0)))
        (max-x (1- (array-dimension data 1))))
    (if (= (aref data y x) 9)
      0
      (progn
        (setf (aref data y x) 9)
        (let ((res 1))
          (when (> y 0)
            (incf res (find-basin data (1- y) x)))
          (when (< y max-y)
            (incf res (find-basin data (1+ y) x)))
          (when (> x 0)
            (incf res (find-basin data y (1- x))))
          (when (< x max-x)
            (incf res (find-basin data y (1+ x))))
          res)))))

(defun find-basins (data)
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
    (loop for y below ny
          nconc (loop for x below nx
                      for basin = (find-basin data y x)
                      when (plusp basin)
                      collect basin))))

(defun day-9-2 (filename)
  (let ((data (read-input filename)))
    (let ((basins
           (find-basins data)))
      (reduce '* (subseq 
                  (sort basins '>=)
                  0 3)))))

#||
(assert (= (day-9-2 *filename-9-0*) 1134))
(day-9-2 *filename-9-1*)
||#
