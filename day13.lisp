(in-package #:aoc2021)

(defpackage #:aoc2021/day13
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day13)

(defparameter *filename-13-0* (merge-pathnames "input-13-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-13-1* (merge-pathnames "input-13-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((points
           (loop for line = (read-line f nil)
                 for point = (cl-ppcre:register-groups-bind (('parse-integer x y))
                                 ("^(\\d+),(\\d+)" line)
                               (cons y x ))
                 while point
                 collect point))
          (folds
           (loop for line = (read-line f nil)
                 for fold = (cl-ppcre:register-groups-bind (axis value)
                                ("^fold along ([xy])=(\\d+)" line)
                              (cons (intern (string-upcase axis)) (parse-integer value)))
                 while fold
                 collect fold)))
      (multiple-value-bind (max-y max-x)
          (loop for (y . x) in points
                maximizing y into max-y
                maximizing x into max-x
                finally (return (values max-y max-x)))
        (let ((grid (make-array (list (1+ max-y) (1+ max-x)) :initial-element nil)))
          (loop for (y . x) in points
                do (setf (aref grid y x) t))
          (values grid folds))))))

(defun print-grid (grid &optional (stream *standard-output*))
  (loop for y below (array-dimension grid 0)
        do (progn
             (terpri stream)
             (loop for x below (array-dimension grid 1)
                   do (write-char (if (aref grid y x) #\# #\.) stream))))
  (terpri stream))


    

#||
(multiple-value-bind (grid folds)
    (read-input *filename-13-0*)
  (print-grid grid)
  (print folds))

(multiple-value-bind (grid folds)
    (read-input *filename-13-1*)
  (print (array-dimensions grid))
  (print folds))
||#

(defun fold (grid axis value)
  (if (eq axis 'y)
    (let ((new-grid (make-array (list value (array-dimension grid 1)))))
      (loop for y below (array-dimension new-grid 0)
            do (loop for x below (array-dimension new-grid 1)
                     do (setf (aref new-grid y x)
                              (or (aref grid y x)
                                  (aref grid (- (array-dimension grid 0) y 1) x)))))
      new-grid)
    (let ((new-grid (make-array (list (array-dimension grid 0) value))))
      (loop for y below (array-dimension new-grid 0)
            do (loop for x below (array-dimension new-grid 1)
                     do (setf (aref new-grid y x)
                              (or (aref grid y x)
                                  (aref grid y (- (array-dimension grid 1) x 1))))))
      new-grid)))

(defun count-dots (grid)
  (loop for i below (array-total-size grid)
        counting (row-major-aref grid i)))
                   

(defun day-13-1 (filename)
  (multiple-value-bind (grid folds)
      (read-input filename)
    (destructuring-bind (axis . value)
        (first folds)
      (let ((new-grid (fold grid axis value)))
        (print (count-dots new-grid))))))

#||
(assert (= (day-13-1 *filename-13-0*) 17))
(day-13-1 *filename-13-1*)
||#

(defun day-13-2 (filename)
  (multiple-value-bind (grid folds)
      (read-input filename)
    (dolist (fold folds)
      (destructuring-bind (axis . value)
          fold
        (setf grid (fold grid axis value))))
    (print-grid grid)))
#||
(day-13-2 *filename-13-1*)
||#
