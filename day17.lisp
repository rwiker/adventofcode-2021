(in-package #:aoc2021)

(defpackage #:aoc2021/day17
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day17)

(defparameter *filename-17-0* (merge-pathnames "input-17-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-17-1* (merge-pathnames "input-17-1.txt" (asdf:system-source-directory :aoc2021)))

(defclass probe ()
  ((xmin :accessor xmin :initarg :xmin)
   (xmax :accessor xmax :initarg :xmax)
   (ymin :accessor ymin :initarg :ymin)
   (ymax :accessor ymax :initarg :ymax)
   (xcur :accessor xcur :initform 0)
   (ycur :accessor ycur :initform 0)
   (xvel :accessor xvel :initarg :xvel)
   (yvel :accessor yvel :initarg :yvel)
   (max-height :accessor max-height :initform 0)))

(defun read-input (filename)
  (with-open-file (f filename)
    (cl-ppcre:register-groups-bind (('parse-integer xmin xmax ymin ymax))
        ("^target area: x=(-?\\d+)\.\.(-?\\d+), y=(-?\\d+)..(-?\\d+)" (read-line f))
      (make-instance 'probe :xmin xmin :xmax xmax :ymin ymin :ymax ymax))))

#||
(read-input *filename-17-0*)
(read-input *filename-17-1*)
||#


(defmethod in-target (probe)
  (and (<= (xmin probe) (xcur probe) (xmax probe))
       (<= (ymin probe) (ycur probe) (ymax probe))))

(defmethod missed-target (probe)
  (cond ((and (zerop (xvel probe))
              (< (xcur probe) (xmin probe)))
         'x-lo)
        ((> (xcur probe) (xmax probe))
         'x-hi)
        ((and (< (ycur probe) (ymin probe))
              (< (xcur probe) (xmin probe)))
         'y-lo)
        ((and (plusp (xcur probe))
              (< (ycur probe) (ymin probe)))
         'y-hi)
        (t
         nil)))

(defun solve-quadratic (a b c)
  (let ((d (sqrt (- (* b b) (* 4 a c)))))
    (list (/ (- (- b) d) (* 2.0 a))
          (/ (+ (- b) d) (* 2.0 a)))))

(defun min-positive (list)
  (reduce 'min (remove-if 'minusp list)))

(defmethod x-range (probe)
  (let ((min (ceiling (min-positive (solve-quadratic 0.5 1.0 (- (xmin probe))))))
        (max (xmax probe)))
    (loop for i from min upto max
            collect i)))

(defmethod simulate/1 (probe)
  (incf (xcur probe) (xvel probe))
  (incf (ycur probe) (yvel probe))
  ;; (format t "~&x=~d, y=~d~%" (xcur probe) (ycur probe))
  (when (> (ycur probe) (max-height probe))
    (setf (max-height probe) (ycur probe)))
  (when (plusp (xvel probe))
    (decf (xvel probe)))
  (decf (yvel probe)))

(defmethod simulate (probe xvel yvel)
  (setf (xcur probe) 0
        (ycur probe) 0
        (max-height probe) 0)
  ;; (format t "~&xvel=~d, yvel=~d~%" xvel yvel)
  (setf (xvel probe) xvel
        (yvel probe) yvel)
  (let ((ret
         (loop for missed-target = (missed-target probe)
               for in-target = (in-target probe)
               until (or in-target missed-target)
               do (simulate/1 probe)
               finally (return (if in-target (max-height probe)
                                 missed-target)))))
    ;; (format t "~&Ret: ~a~%" ret)
    ret))

#||
(let ((probe (read-input *filename-17-0*)))
  (simulate probe 6 12))
||#

(defun day-17-1 (filename)
  (let ((probe (read-input filename)))
    (let ((max-height 0)
          (best-vel nil))
      (loop for x-vel in (x-range probe) 
            do
              (loop for y-vel from 1 below 500
                    for sim-result = (simulate probe x-vel y-vel)
                    when (and (numberp sim-result)
                              (> sim-result max-height))
                      do (setf max-height sim-result
                               best-vel (list x-vel y-vel))
                    #+nil
                    until
                      #+nil (or (and (numberp sim-result)
                                     (< sim-result max-height))
                                (and best-vel (eq sim-result 'y-hi)))))
      (values max-height best-vel))))

#||
(assert (= (day-17-1 *filename-17-0*) 45))
(day-17-1 *filename-17-1*)
||# 

(defun day-17-2 (filename)
  (let ((probe (read-input filename)))
    (length 
     (loop for x-vel upto (xmax probe)
             nconc
             (loop for y-vel from -500 upto 500
                   for sim-result = (simulate probe x-vel y-vel)
                   when (numberp sim-result)
                     collect (list x-vel y-vel))))))

#||
(assert (= (day-17-2 *filename-17-0*) 112))
(day-17-2 *filename-17-1*)
||#
