(in-package #:aoc2021)

(defpackage #:aoc2021/day21
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day21)

(declaim (optimize debug))

(defparameter *filename-21-0* (merge-pathnames "input-21-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-21-1* (merge-pathnames "input-21-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (flet ((extract-pos (line)
           (or
            (cl-ppcre:register-groups-bind (('parse-integer pos))
                ("^Player \\d starting position: (\\d+)" line)
              pos)
            (error "unexpected input: ~a" line))))
    (with-open-file (f filename)
      (values (extract-pos (read-line f nil))
              (extract-pos (read-line f nil))))))

#||
(read-input *filename-21-0*)
(read-input *filename-21-1*)

||#

(defclass die ()
  ((val :accessor val :initform 0)
   (rolls :accessor rolls :initform 0)))

(defmethod roll ((die die))
  (let ((new-val (1+ (val die))))
    (when (> new-val 100)
      (setf new-val 1))
    (setf (val die) new-val)
    (incf (rolls die))
    new-val))

(defmethod roll/3 ((die die))
  (+ (roll die) (roll die) (roll die)))

(defclass player ()
  ((pos :accessor pos :initarg :pos)
   (score :accessor score :initarg :score :initform 0)))

(defmethod clone ((player player))
  (make-instance 'player :pos (pos player) :score (score player)))

(defmethod advance ((player player) roll)
  (let ((new-pos
         (1+ (mod (+ (pos player) roll -1) 10))))
    (setf (pos player) new-pos)
    (incf (score player) new-pos)
    player))

#||
(assert (= 3
           (let ((player (make-instance 'player :pos 6))
               (die (make-instance 'die)))
           (setf (val die) 87)
           (advance player (roll/3 die))
           (pos player))))
||#

(defmethod has-won ((player player))
  (>= (score player) 1000))

(defun day-21-1 (filename)
  (multiple-value-bind (player-1-start player-2-start)
      (read-input filename)
    (let ((player-1 (make-instance 'player :pos player-1-start))
          (player-2 (make-instance 'player :pos player-2-start))
          (die (make-instance 'die)))
      (loop (advance player-1 (roll/3 die))
            (when (has-won player-1)
              (return-from day-21-1 (* (score player-2) (rolls die))))
            (advance player-2 (roll/3 die))
            (when (has-won player-2)
              (return-from day-21-1 (* (score player-1) (rolls die))))))))

#||
(assert (= (day-21-1 *filename-21-0*) 739785))
(day-21-1 *filename-21-1*)
||#

(defparameter *qd-weights*
  (let ((vec (make-array '(10) :element-type 'integer :initial-element 0)))
    (dotimes (d1 3)
      (dotimes (d2 3)
        (dotimes (d3 3)
          (incf (aref vec (+ (1+ d1) (1+ d2) (1+ d3)))))))
    vec))

(defparameter *qd-values*
  (loop for i from 0
        for v across *qd-weights*
        when (plusp v)
          collect i))

(defun outcomes (roll)
  (svref *qd-weights* roll))

(defun compute-total-outcomes (rolls)
  (let ((res 1))
    (dolist (roll rolls)
      (setf res (* res (svref *qd-weights* roll))))
    res))

(defun r-play (player-1 player-2 rolls)
  (if (>= (score player-1) 21)
    (list (compute-total-outcomes rolls)
          0)
    (loop for roll in *qd-values*
          for new-player-2 = (advance (clone player-2) roll)
          for (p2-w p1-w) = (r-play new-player-2 player-1 (cons roll rolls))
          summing p2-w into player-2-wins
          summing p1-w into player-1-wins
          finally (return (list player-1-wins player-2-wins)))))

(defun day-21-2 (filename)
  (multiple-value-bind (player-1-start player-2-start)
      (read-input filename)
    (let ((player-1 (make-instance 'player :pos player-1-start))
          (player-2 (make-instance 'player :pos player-2-start)))
      (destructuring-bind (player-1-wins player-2-wins)
          (r-play player-2 player-1 nil)
        (values player-1-wins player-2-wins)))))

#||
(day-21-2 *filename-21-0*)
(time
(day-21-2 *filename-21-1*))
||#
  
