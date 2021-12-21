(in-package #:aoc2021)

(defpackage #:aoc2021/day20
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day20)

(declaim (optimize debug))

(defparameter *filename-20-0* (merge-pathnames "input-20-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-20-1* (merge-pathnames "input-20-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (labels ((map-char (c)
             (if (char= c #\#) 1 0)))
  (with-open-file (f filename)
    (let ((map (map 'bit-vector #'map-char (read-line f))))
      (read-line f)
      (let ((lines (loop for line = (read-line f nil)
                         while line
                         collect (map 'list #'map-char line))))
        (values (make-array (list (length lines) (length (first lines)))
                                :initial-contents lines) map))))))

#||
(read-input *filename-20-0*)
(read-input *filename-20-1*)
||#

(defun print-array (data &optional (stream *standard-output*))
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
  (fresh-line stream)
  (loop for y below ny
          do (progn
               (loop for x below nx
                     do (write-char (if (zerop (aref data y x)) #\. #\#)))
               (terpri stream)))))

#||
(print-array (read-input *filename-20-0*))
||#

(defun enhance (data map background)
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
    (flet ((get-index (y x)
             (let ((bits 
                    (loop for yy from (1- y) upto (1+ y)
                          nconc (loop for xx from (1- x) upto (1+ x)
                                      collect (if (and (<= 0 yy (1- ny))
                                                       (<= 0 xx (1- nx)))
                                                (aref data yy xx)
                                                background)))))
               (loop for weight = 256 then (ash weight -1)
                     for bit in bits
                     summing (if (plusp bit) weight 0)))))
      (let ((new-background (aref map (if (zerop background) 0 511)))
            (new-data
             (loop for y from -1 upto ny
                   collect (loop for x from -1 upto nx
                                 collect (aref map (get-index y x))))))
        (values (make-array (list (length new-data) (length (first new-data)))
                    :initial-contents new-data)
                new-background)))))


(defun count-bits-set (data)
  (loop for i below (array-total-size data)
        counting (plusp (row-major-aref data i))))

#||
(multiple-value-bind (data map)
    (read-input *filename-20-0*)
  (print-array data)
  (terpri)
  (loop for i below 2
          do (progn
               (setf data (enhance data map))
               (print-array data)
               (terpri)))
  (count-bits-set data))
||#                   

(defun day-20-1 (filename)
  (multiple-value-bind (data map)
      (read-input filename)
    (let ((background 0))
      (dotimes (i 2)
        (multiple-value-bind (new-data new-background)
            (enhance data map background)
          (setf data new-data
                background new-background)))
      ;; (print-array data)
      (count-bits-set data))))

#||
(assert (= (day-20-1 *filename-20-0*) 35))
(day-20-1 *filename-20-1*)
||#

(defun day-20-2 (filename)
  (multiple-value-bind (data map)
      (read-input filename)
    (let ((background 0))
      (dotimes (i 50)
        (multiple-value-bind (new-data new-background)
            (enhance data map background)
          (setf data new-data
                background new-background)))
      ;; (print-array data)
      (count-bits-set data))))

#||
(assert (= (day-20-2 *filename-20-0*) 3351))
(day-20-2 *filename-20-1*)
||#

