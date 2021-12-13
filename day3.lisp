(in-package #:aoc2021)

(defpackage #:aoc2021/day3
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day3)

(defparameter *filename-3-0* (merge-pathnames "input-3-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-3-1* (merge-pathnames "input-3-1.txt" (asdf:system-source-directory :aoc2021)))

(defun day-3-1 (filename)
  (with-open-file (f filename)
    (multiple-value-bind (ones zeroes)
        (loop with first-line = (read-line f nil)
            with num-bits = (length first-line)
            with ones = (make-array (list num-bits) :element-type 'integer :initial-element 0)
            with zeroes = (make-array (list num-bits) :element-type 'integer :initial-element 0)
            for line = first-line then (read-line f nil)
            while line
            for val = (parse-integer line :radix 2)
            do (loop for bit below num-bits
                     for v = val then (ash v -1)
                     for b-v = (logand v 1)
                     do (if (zerop b-v)
                          (incf (aref zeroes bit))
                          (incf (aref ones bit))))
            finally (return (values ones zeroes)))
      (let ((gamma 0)
            (epsilon 0))
        (loop with num-bits = (array-dimension ones 0)
              for bit below num-bits
              for b-v = 1 then (ash b-v 1)
              do (if (> (aref ones bit)
                        (aref zeroes bit))
                   (setf gamma (logior gamma b-v))
                   (setf epsilon (logior epsilon b-v))))
        (format t "~&gamma: ~12,'0B; epsilon: ~12,'0B~%" gamma epsilon)
        (* gamma epsilon)))))


(defun day-3-2 (filename)
  (with-open-file (f filename)
    (let ((lines
           (loop for line = (read-line f nil)
                 while line
                 collect line)))
      (labels ((occurrences (lines position)
                 (let ((ones 0)
                       (zeroes 0))
                   (loop for line in lines
                         for char = (char line position)
                         do (if (char= char #\0)
                              (incf zeroes)
                              (incf ones)))
                   (values ones zeroes)))
               (filter-values (lines position match)
                 (remove-if-not (lambda (x)
                                  (char= (char x position) match))
                                lines))
               (o2-gen-rating (lines)
                 (dotimes (pos (length (first lines)))
                   (multiple-value-bind (ones zeroes)
                       (occurrences lines pos)
                     (let ((match-char (if (>= ones zeroes)
                                         #\1
                                         #\0)))
                       (setf lines (filter-values lines pos match-char))
                       (when (null (cdr lines))
                         (return-from o2-gen-rating (parse-integer (car lines) :radix 2))))))
                 (error "Unable to reduce to single value"))
               (co2-scrubber-rating (lines)
                 (dotimes (pos (length (first lines)))
                   (multiple-value-bind (ones zeroes)
                       (occurrences lines pos)
                     (let ((match-char (if (<= zeroes ones)
                                         #\0
                                         #\1)))
                       (setf lines (filter-values lines pos match-char))
                       (when (null (cdr lines))
                         (return-from co2-scrubber-rating (parse-integer (car lines) :radix 2))))))
                 (error "Unable to reduce to single value")))
        (let ((o2-gen-rating (o2-gen-rating lines))
              (co2-scrubber-rating (co2-scrubber-rating lines)))
          (format t "~&O2: ~d; CO2: ~d~%" o2-gen-rating co2-scrubber-rating)
          (* o2-gen-rating co2-scrubber-rating))))))


#||
(assert (= (day-3-1 *filename-3-0*) 198))
(day-3-1 *filename-3-1*)
(assert (= (day-3-2 *filename-3-0*) 230))
(day-3-2 *filename-3-1*)
||#