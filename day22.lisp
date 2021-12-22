(in-package #:aoc2021)

(defpackage #:aoc2021/day22
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day22)

(declaim (optimize debug))

(defparameter *filename-22-0* (merge-pathnames "input-22-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-22* (merge-pathnames "input-22-1.txt" (asdf:system-source-directory :aoc2021)))


(defclass urange ()
  ((lo :accessor lo :initarg :lo)
   (hi :accessor hi :initarg :hi)))

(defclass range (urange)
  ())

(defmethod print-object ((range urange) stream)
  (print-unreadable-object (range stream)
    (format stream "~d..~d" (lo range) (hi range))))

(defun make-range (lo hi)
  (make-instance 'range :lo (clamp lo)  :hi (clamp hi)))

(defun clamp (value &optional (min -50) (max 50))
  (if (< value min)
    min
    (if (> value max)
      max
      value)))

(defclass uop ()
  ((op :accessor op :initarg :op)
   (xrange :accessor xrange :initarg :xrange)
   (yrange :accessor yrange :initarg :yrange)
   (zrange :accessor zrange :initarg :zrange)))

(defclass op (uop)
  ())

(defmethod print-object ((op uop) stream)
  (print-unreadable-object (op stream :type t)
    (format stream "~a x=~a,y=~a,z=~a"
            (if (op op) "on" "off")
            (xrange op) (yrange op) (zrange op))))

(defun make-op (op xlo xhi ylo yhi zlo zhi)
  (when (and (>= xhi -50)
             (<= xlo 50)
             (>= yhi -50)
             (<= ylo 50)
             (>= zhi -50)
             (<= zlo 50))
    (make-instance 'op :op (string= op "on")
                   :xrange (make-range xlo xhi)
                   :yrange (make-range ylo yhi)
                   :zrange (make-range zlo zhi))))


(defun read-input (filename constructor)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line
          for op = (cl-ppcre:register-groups-bind (op ('parse-integer xlo xhi ylo yhi zlo zhi))
                           ("^(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)" line)
                         (funcall constructor op xlo xhi ylo yhi zlo zhi))
          when op
          collect op)))

#||
(mapcar 'print
        (read-input *filename-22-0* 'make-op))
||#

(defun 50+ (n)
  (+ 50 n))

(defmethod apply-op ((op op) data)
  (with-slots (op xrange yrange zrange)
      op
    (loop for z from (lo zrange) upto (hi zrange)
          do (loop for y from (lo yrange) upto (hi yrange)
                   do (loop for x from (lo xrange) upto (hi xrange)
                            do (setf (aref data (50+ z) (50+ y) (50+ x)) op))))))

(defun count-on (data)
  (loop for i below (array-total-size data)
        counting (row-major-aref data i)))

(defun day-22-1 (filename)
  (let ((ops (read-input filename 'make-op)))
    (let ((data (make-array (list 101 101 101) :initial-element nil)))
      (dolist (op ops)
        (apply-op op data))
      (count-on data))))

#||
(assert (= (day-22-1 *filename-22-0*) 590784))
(day-22-1 *filename-22-1*)
||#

(defun make-urange (lo hi)
  (make-instance 'range :lo lo :hi hi))

(defun make-uop (op xlo xhi ylo yhi zlo zhi)
  (make-instance 'uop :op (string= op "on")
                 :xrange (make-urange xlo xhi)
                 :yrange (make-urange ylo yhi)
                 :zrange (make-urange zlo zhi)))

(defmethod apply-op ((op uop) data)
  (error "Should not be called."))

(defmethod intersect ((a urange) (b urange))
  (cond ((<= (lo a) (lo b) (hi a))
         (make-urange (lo b) (min (hi a) (hi b))))
        ((<= (lo a) (hi b) (hi a))
         (make-urange (max (lo a) (lo b)) (hi b)))
        (t
         nil)))

(defmethod intersect ((a uop) (b uop))
  (unless (eq a b)
    (let ((xi (intersect (xrange a) (xrange b)))
          (yi (intersect (yrange a) (yrange b)))
          (zi (intersect (zrange a) (zrange b))))
      (when (and xi yi zi)
        (make-uop "on" (lo xi) (hi xi) (lo yi) (hi yi) (lo zi) (hi zi))))))

(defmethod contains ((a urange) (b urange))
  (and (<= (lo a) (lo b) (hi a))
       (<= (lo a) (hi b) (hi a))))

(defmethod contains ((a uop) (b uop))
  (and (not (eq a b))
       (eq (op a) (op b))
       (contains (xrange a) (xrange b))
       (contains (yrange a) (yrange b))
       (contains (zrange a) (zrange b))))
  

#||
(defparameter *ops*
  (read-input *filename-22-1* 'make-uop))

(loop for a in *ops*
      do (loop for b in *ops*
               if (contains a b)
                 do (format t "~&~a contains ~b~%" a b)))

(loop for a in *ops*
      do (loop for b in *ops*
               if (intersect a b)
                 do (format t "~&~a intersects ~b~%" a b)))

(loop for a in *ops*
       for intersects = (loop for b in *ops*
                              if (intersect a b)
                                collect b)
       unless intersects
         do (print a)
         counting intersects)

(length *ops*)

(length (find-if (lambda (x) (intersect (first *ops*) x))  (rest *ops*)))
||#
