(in-package #:aoc2021)

(defpackage #:aoc2021/day14
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day14)

(defparameter *filename-14-0* (merge-pathnames "input-14-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-14-1* (merge-pathnames "input-14-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((template (read-line f))
          (rules (make-hash-table :test 'equal)))
      (assert (zerop (length (read-line f))))
      (loop for line = (read-line f nil)
            while line
            do (or (cl-ppcre:register-groups-bind (pair value)
                       ("^([A-Z]{2}) -> ([A-Z])" line)
                     (assert (null (gethash pair rules)))
                     (setf (gethash pair rules ) (char value 0)))
                   (error "Odd line: " line)))
      (values template rules))))

(defun apply-rules (template rules)
  (with-output-to-string (s)
    (loop for i below (1- (length template))
          for pair = (subseq template i (+ i 2))
          for new-char = (gethash pair rules)
          do (progn
               (assert (not (null new-char)))
               (write-char (char template i) s)
               (write-char new-char s))
          finally (write-char (char template (1- (length template))) s))))

#||
(multiple-value-bind (template rules)
    (read-input *filename-14-0*)
  (apply-rules template rules))
||#

(defun apply-rules/n (template rules n)
  (dotimes (i n)
    (setf template (apply-rules template rules)))
  template)

(defun find-counts (template)
  (let ((counts (make-hash-table)))
    (loop for c across template
          do (incf (gethash c counts 0)))
    (loop for v being the hash-value of counts
          minimizing v into min
          maximizing v into max
          finally (return (values min max)))))

(defun day-14-1 (filename &optional (steps 10))
  (multiple-value-bind (template rules)
      (read-input filename)
    (let ((template (apply-rules/n template rules steps)))
      (multiple-value-bind (min max) (find-counts template)
        (- max min)))))

#||
(assert (= (day-14-1 *filename-14-0*) 1588))
(day-14-1 *filename-14-1*)
||#


;;; does not work... combinatorial explosion
#+nil
(defun day-14-2 (filename &optional (steps 40))
    (multiple-value-bind (template rules)
      (read-input filename)
    (let ((template (apply-rules/n template rules steps)))
      (multiple-value-bind (min max) (find-counts template)
        (- max min)))))

#||
(day-14-2 *filename-14-0*)
||#

(defun split-template (template)
  (let ((map (make-hash-table :test 'equal)))
    (loop for i below (1- (length template))
          for pair = (subseq template i (+ i 2))
          do (incf (gethash pair map 0)))
    map))

(defun print-map (map)
  (maphash (lambda (k v)
             (format t "~&~a: ~d~%" k v))
           map)
  (terpri))
#||
(multiple-value-bind (template rules)
    (read-input *filename-14-0*)
  (let ((map 
         (split-template template)))
    (print-map map)))
||#
               
(defun apply-rules/map (map rules)
  (let ((new-map (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (let ((r (gethash k rules)))
                 (assert (not (null r)))
                 (let ((v1 (format nil "~c~c" (char k 0) r))
                       (v2 (format nil "~c~c" r (char k 1))))
                   (incf (gethash v1 new-map 0) v)
                   (incf (gethash v2 new-map 0) v))))
             map)
    new-map))

(defun apply-rules/map/n (map rules steps)
  (dotimes (i steps)
    (setf map (apply-rules/map map rules)))
  map)

(defun find-counts/map (map last-char)
  (let ((counts (make-hash-table)))
    (maphash (lambda (k v)
               (incf (gethash (char k 0) counts 0) v))
             map)
    (incf (gethash last-char counts 0))
    (print-map counts)
    (loop for v being the hash-value of counts
          minimizing v into min
          maximizing v into max
          finally (return (values min max)))))

(defun day-14-2 (filename &optional (steps 40))
  (multiple-value-bind (template rules)
      (read-input filename)
    (let ((last-char (char template (1- (length template))))
          (map (split-template template)))
      (print-map map)
      (let ((map (apply-rules/map/n map rules steps)))
        (print-map map)
        (multiple-value-bind (min max) (find-counts/map map last-char)
          (- max min))))))

#||
(assert (= (day-14-2 *filename-14-0* 10) 1588))
(day-14-2 *filename-14-1* 10)
(day-14-2 *filename-14-1* 40)

||#
