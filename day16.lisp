(in-package #:aoc2021)

(defpackage #:aoc2021/day16
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day16)

(defparameter *filename-16-0* (merge-pathnames "input-16-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-16-1* (merge-pathnames "input-16-1.txt" (asdf:system-source-directory :aoc2021)))

(defun day-16-1 (filename)
  (with-open-file (f filename)
    (parse-string-1 (read-line f))))

(defun parse-string-1 (string)
  (let ((bits (parse-integer string :radix 16)))
    (parse-bits-1 bits)))

(defun parse-bits-1 (bits)
  (let ((bit-pos (integer-length bits)))
    (unless (zerop (mod bit-pos 8))
      (incf bit-pos (- 8 (mod bit-pos 8))))
    (labels ((read-and-advance (num-bits)
               (prog1
                   (ldb (byte num-bits (- bit-pos num-bits)) bits)
                 (decf bit-pos num-bits)))
             (read-version ()
               (read-and-advance 3))
             (read-type ()
               (read-and-advance 3))
             (read-bit ()
               (read-and-advance 1))
             (read-literal ()
               (let ((chunks
                      (loop for bits-read from (+ 5 6) by 5
                            for more = (plusp (read-bit))
                            for chunk = (read-and-advance 4)
                            collect chunk
                            while more))
                     (res 0))
                 (loop for chunk in (nreverse chunks)
                       for pos from 0 by 4
                       do (setf res (dpb chunk (byte 4 pos) res)))
                 res))
             (read-operator ()
               (let ((length-type (read-bit)))
                 (if (zerop length-type)
                   (let ((length (read-and-advance 15)))
                     (let ((stop-pos (- bit-pos length)))
                       (loop while (> bit-pos stop-pos)
                           summing (parse-packet))))
                   (let ((num-sub-ops (read-and-advance 11)))
                     (loop for i below num-sub-ops
                           summing (parse-packet))))))
             (parse-packet ()
               (let ((version (read-version))
                     (type (read-type)))
                 (prog1
                     (cond ((= type 4)
                            (read-literal)
                            version)
                           (t
                            (+ version
                               (read-operator))))))))
      (loop while (> bit-pos 7)
            summing (parse-packet)))))




#||
(parse-string-1 "D2FE28")
(parse-string-1 "38006F45291200")
(parse-string-1 "EE00D40C823060")
(assert (= (parse-string-1 "8A004A801A8002F478") 16))
(assert (= (parse-string-1 "620080001611562C8802118E34") 12))
(parse-string-1 
(day-16-1 *filename-16-1*)
||#

(defun parse-bits-2 (bits)
  (let ((bit-pos (integer-length bits)))
    (unless (zerop (mod bit-pos 8))
      (incf bit-pos (- 8 (mod bit-pos 8))))
    (labels ((read-and-advance (num-bits)
               (prog1
                   (ldb (byte num-bits (- bit-pos num-bits)) bits)
                 (decf bit-pos num-bits)))
             (read-version ()
               (read-and-advance 3))
             (read-type ()
               (read-and-advance 3))
             (read-bit ()
               (read-and-advance 1))
             (read-literal ()
               (let ((chunks
                      (loop for bits-read from (+ 5 6) by 5
                            for more = (plusp (read-bit))
                            for chunk = (read-and-advance 4)
                            collect chunk
                            while more))
                     (res 0))
                 (loop for chunk in (nreverse chunks)
                       for pos from 0 by 4
                       do (setf res (dpb chunk (byte 4 pos) res)))
                 res))
             (read-operator ()
               (let ((length-type (read-bit)))
                 (if (zerop length-type)
                   (let ((length (read-and-advance 15)))
                     (let ((stop-pos (- bit-pos length)))
                       (loop while (> bit-pos stop-pos)
                             collecting (parse-packet))))
                   (let ((num-sub-ops (read-and-advance 11)))
                     (loop for i below num-sub-ops
                           collecting (parse-packet))))))
             (parse-packet ()
               (let ((version (read-version))
                     (type (read-type)))
                 (declare (ignore version))
                 (cond ((= type 4)
                        (read-literal))
                       (t
                        (let ((operands
                               (read-operator)))
                          (case type
                            (0 (reduce '+ operands))
                            (1 (reduce '* operands))
                            (2 (loop for op in operands minimizing op))
                            (3 (loop for op in operands maximizing op))
                            (5 (if (> (first operands) (second operands)) 1 0))
                            (6 (if (< (first operands) (second operands)) 1 0))
                            (7 (if (= (first operands) (second operands)) 1 0)))))))))
      (parse-packet))))

(defun day-16-2 (filename)
  (with-open-file (f filename)
    (parse-string-2 (read-line f))))

(defun parse-string-2 (string)
  (let ((bits (parse-integer string :radix 16)))
    (parse-bits-2 bits)))

#||
(day-16-2 *filename-16-1*)
||#