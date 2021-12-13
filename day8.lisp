(in-package #:aoc2021)

(defpackage #:aoc2021/day8
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day8)

(defparameter *filename-8-0* (merge-pathnames "input-8-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-8-1* (merge-pathnames "input-8-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((res 
           (loop for line = (read-line f nil)
                 while line
                 collect (cl-ppcre:register-groups-bind (inputs outputs)
                             ("^\\s*(.*) \\| (.*)\\s*$" line)
                           (list (cl-ppcre:split " " inputs)
                                 (cl-ppcre:split " " outputs))))))
      (assert (not (find nil res)))
      res)))

(defun day-8-1 (filename)
  (let ((data (read-input filename)))
    (loop for (nil outputs . nil) in data
          summing (loop for output in outputs
                        for len = (length output)
                        counting (or (= len 2)
                                     (= len 3)
                                     (= len 4)
                                     (= len 7))))))

#||
(read-input *filename-8-0*)
(assert (= (day-8-1 *filename-8-0*)))
(day-8-1 *filename-8-1*)
||#

(defun day-8-2 (filename)
  (labels ((make-set (string)
             (assert (stringp string))
             (coerce string 'list))
           (set-equal (set1 set2)
             (null (set-exclusive-or set1 set2)))
           (of-length/1 (inputs n)
             (make-set (find n inputs :key 'length :test '=)))
           (of-length/n (inputs n)
             (mapcar #'make-set (remove-if-not (lambda (inp)
                                                 (= (length inp) n))
                                               inputs)))
           (decode (inputs)
             (let ((pattern-1 (of-length/1 inputs 2))
                   (pattern-7 (of-length/1 inputs 3))
                   (pattern-4 (of-length/1 inputs 4))
                   (pattern-8 (of-length/1 inputs 7))
                   (patterns/235 (of-length/n inputs 5))
                   (patterns/069 (of-length/n inputs 6)))
               (let ((pattern-6
                      (find-if (lambda (pattern)
                                 (not (subsetp pattern-1 pattern)))
                               patterns/069)))
                 (let ((patterns/09
                        (remove pattern-6 patterns/069 :test #'set-equal)))
                   (let ((pattern-9
                          (find-if (lambda (pattern)
                                     (subsetp pattern-4 pattern))
                                   patterns/09)))
                     (let ((pattern-0 (first (remove pattern-9 patterns/09 :test #'set-equal))))
                       (let ((pattern-3
                              (find-if (lambda (pattern)
                                         (subsetp pattern-1 pattern))
                                       patterns/235)))
                         (let ((patterns/25
                                (remove pattern-3 patterns/235 :test #'set-equal)))
                           (let ((pattern-2
                                  (let ((diff (set-difference pattern-8 pattern-9)))
                                    (find-if (lambda (pattern)
                                               (subsetp diff pattern))
                                             patterns/25))))
                             (let ((pattern-5 (first (remove pattern-2 patterns/25 :test #'set-equal))))
                               (list pattern-0 pattern-1 pattern-2 pattern-3 pattern-4
                                     pattern-5 pattern-6 pattern-7 pattern-8 pattern-9)))))))))))
           (convert (outputs patterns)
             (parse-integer
              (map 'string 'digit-char 
                   (loop for output in outputs
                         for pat = (make-set output)
                         for pos = (position pat patterns :test #'set-equal)
                         do (assert pos)
                         collect pos)))))
    (let ((data (read-input filename)))
      (loop for (inputs outputs . nil) in data
            for patterns = (decode inputs)
            for value = (convert outputs patterns)
            summing value))))

#||
(assert (= (day-8-2 *filename-8-0*) 61229))
(time (day-8-2 *filename-8-1*))
||#
    

