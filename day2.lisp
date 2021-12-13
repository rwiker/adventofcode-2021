(in-package #:aoc2021)

(defpackage #:aoc2021/day2
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day2)

(defparameter *filename-2-0* (merge-pathnames "input-2-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-2-1* (merge-pathnames "input-2-1.txt" (asdf:system-source-directory :aoc2021)))

(defun day-2-1 (filename)
  (let ((x-pos 0)
        (z-pos 0))
    (with-open-file (f filename)
      (loop for line = (read-line f nil)
            while line
            do (or (cl-ppcre:register-groups-bind (('parse-integer val))
                       ("^forward\\s+(\\d+)" line)
                     (incf x-pos val))
                   (cl-ppcre:register-groups-bind (('parse-integer val))
                       ("^down\\s+(\\d+)" line)
                     (incf z-pos val))
                   (cl-ppcre:register-groups-bind (('parse-integer val))
                       ("^up\\s+(\\d+)" line)
                     (decf z-pos val))
                   (error "Bad input: ~a" line))))
    (format t "~&x: ~d; z: ~d; prod: ~d~%" x-pos z-pos (* x-pos z-pos))
    (* x-pos z-pos)))


(defun day-2-2 (filename)
  (let ((aim 0)
        (x-pos 0)
        (z-pos 0))
    (with-open-file (f filename)
      (loop for line = (read-line f nil)
            while line
            do (or (cl-ppcre:register-groups-bind (('parse-integer val))
                       ("^forward\\s+(\\d+)" line)
                     (incf x-pos val)
                     (incf z-pos (* aim val)))
                   (cl-ppcre:register-groups-bind (('parse-integer val))
                       ("^down\\s+(\\d+)" line)
                     (incf aim val))
                   (cl-ppcre:register-groups-bind (('parse-integer val))
                       ("^up\\s+(\\d+)" line)
                     (decf aim val))
                   (error "Bad input: ~a" line))))
    (format t "~&x: ~d; z: ~d; prod: ~d~%" x-pos z-pos (* x-pos z-pos))
    (* x-pos z-pos)))


#||
(assert (= (day-2-1 *filename-2-0*) 150))
(day-2-1 *filename-2-1*)
(assert (= (day-2-2 *filename-2-0*) 900))
(day-2-2 *filename-2-1*)
||#