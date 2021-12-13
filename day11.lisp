(in-package #:aoc2021)

(defpackage #:aoc2021/day11
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day11)

(defparameter *filename-11-0* (merge-pathnames "input-11-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-11-1* (merge-pathnames "input-11-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((data 
           (loop for line = (read-line f nil)
                 while line
                 collect (map 'list 'digit-char-p line))))
      (make-array (list (length data) (length (first data)))
                  :element-type 'integer :initial-contents data))))

(defun print-data (data &optional (stream *standard-output*))
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
    (terpri stream)
    (loop for y below ny
          do (progn
               (loop for x below nx
                     do (write-char (code-char (+ (aref data y x) (char-code #\0))) stream))
               (terpri stream)))))

(defun one-step (data)
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1))
        (total-flashed 0))
    (labels ((increment-all ()
               (loop for i below (array-total-size data)
                     do (incf (row-major-aref data i))))
             (neighbours (y x)
               (loop for yy from (1- y) upto (1+ y)
                     when (and (>= yy 0) (< yy ny))
                     nconc (loop for xx from (1- x) upto (1+ x)
                                 when (and (>= xx 0) (< xx nx)
                                           (not (and (= yy y) (= xx x)))
                                           (not (zerop (aref data yy xx))))
                                 collect (list yy xx))))
             (flash (y x)
               (setf (aref data y x) 0)
               (incf total-flashed)
               (loop for (yy xx) in (neighbours y x)
                     do (progn
                          (unless (zerop (aref data yy xx))
                            (incf (aref data yy xx)))
                          (when (> (aref data yy xx) 9)
                            (flash yy xx))))))
      (increment-all)
      (loop for y below ny
            do (loop for x below nx
                     when (> (aref data y x) 9)
                     do (flash y x)))
      total-flashed)))
  
(defun day-11-1 (filename steps)
  (let ((data (read-input filename)))
    ;; (print-data data)
    (loop for i below steps
          for total-flashes = (one-step data)
          ;; do (print-data data)
          summing total-flashes)))



#||
(assert (= (day-11-1 *filename-11-0* 100) 1656))
(day-11-1 *filename-11-1* 100)
||#


(defun day-11-2 (filename)
  (let ((data (read-input filename)))
    (loop for i from 1
          while (/= (one-step data) (array-total-size data))
          finally (return i))))

#||
(assert (= (day-11-2 *filename-11-0*) 195))
(day-11-2 *filename-11-1*)
||#