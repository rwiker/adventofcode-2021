(in-package #:aoc2021)

(defpackage #:aoc2021/day4
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day4)

(defparameter *filename-4-0* (merge-pathnames "input-4-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-4-1* (merge-pathnames "input-4-1.txt" (asdf:system-source-directory :aoc2021)))

(defclass card ()
  ((grid :accessor grid :initarg :grid)
   (row-hits :accessor row-hits)
   (col-hits :accessor col-hits)
   (sum-seen :accessor sum-seen :initform 0)))

(defmethod initialize-instance :after ((card card) &key)
  (let ((ny (array-dimension (grid card) 0))
        (nx (array-dimension (grid card) 1)))
    (setf (row-hits card) (make-array (list ny) :element-type 'integer :initial-element 0)
          (col-hits card) (make-array (list nx) :element-type 'integer :initial-element 0))))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((draws (mapcar 'parse-integer (cl-ppcre:split "," (read-line f))))
          (cards (loop while (read-line f nil)
                       for grid-as-list = (ignore-errors
                                            (loop for row below 5
                                                  for row-numbers = (mapcar 'parse-integer
                                                                            (remove ""
                                                                                    (cl-ppcre:split "\\s+" (read-line f))
                                                                                    :test 'string=))
                                                  collect row-numbers))
                       while grid-as-list
                       for grid = (make-array (list 5 5) :element-type 'integer :initial-contents grid-as-list)
                       collect (make-instance 'card :grid grid))))
      (values draws cards))))

#||
(multiple-value-bind (draws cards)
    (read-input *filename-4-0*)
  (setf *draws* draws
        *cards* cards))
||#

(defun sum-values (card)
  (with-slots (grid sum-seen)
      card
    (-
     (loop for y below (array-dimension grid 0)
           summing (loop for x below (array-dimension grid 1)
                         summing (aref grid y x)))
     sum-seen)))

#||
(sum-values (first *cards*))
|#

(defun mark-card (card number)
  (with-slots (grid row-hits col-hits sum-seen)
      card
    (loop for y below (array-dimension grid 0)
          do (loop for x below (array-dimension grid 1)
                   when (= (aref grid y x) number)
                   do (progn
                        (incf (aref row-hits y))
                        (incf (aref col-hits x))
                        (incf sum-seen number)
                        (when (or (= (aref row-hits y) (array-dimension grid 0))
                                  (= (aref col-hits x) (array-dimension grid 1)))
                          (return-from mark-card
                            (* number (sum-values card))))))))
  nil)

#||
(let ((card (first *cards*)))
  (assert (null (mark-card card 22)))
  (assert (null (mark-card card 13)))
  (assert (null (mark-card card 17)))
  (assert (null (mark-card card 11)))
  (format t "~&mark-card returned ~a (expect 0!)~%"
          (mark-card card 0))
  (print card))

||#

(defun day-4-1 (filename)
  (multiple-value-bind (draws cards)
      (read-input filename)
    (loop for draw in draws
          for nil = (format t "~&draw: ~d~%" draw)
          for scores = (loop for card in cards
                            for score = (mark-card card draw)
                            when score
                            collect score)
          until scores
          finally (return (apply 'max scores)))))

#||
(assert (= (day-4-1 *filename-4-0*) 4512))
(day-4-1 *filename-4-1*)
||#


(defun day-4-2 (filename)
  (multiple-value-bind (draws cards)
      (read-input filename)
    (loop for draw = (pop draws)
          for cards/l = cards then remaining-cards
          for nil = (format t "~&draw: ~d~%" draw)
          for remaining-cards = (loop for card in cards/l
                                      for score = (mark-card card draw)
                                      unless score
                                      collect card)
          until (null (cdr remaining-cards))
          finally (return (loop with card = (first remaining-cards)
                                for draw in draws
                                for nil = (format t "~&draw: ~d~%" draw)
                                for score = (mark-card card draw)
                                until score
                                finally (return score))))))

#||
(assert (= (day-4-2 *filename-4-0*) 1924))
(day-4-2 *filename-4-1*)
||#
