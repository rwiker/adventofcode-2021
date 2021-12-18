(in-package #:aoc2021)

(defpackage #:aoc2021/day18
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day18)

(declaim (optimize debug))

(defparameter *filename-18-0* (merge-pathnames "input-18-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-18-1* (merge-pathnames "input-18-1.txt" (asdf:system-source-directory :aoc2021)))

(defun parse-sailfish-number (string)
  (with-input-from-string (s string)
    (labels ((digits-or-pair ()
               (if (digit-char-p (peek-char nil s))
                 (parse-integer
                  (map 'string 'identity
                       (loop while (digit-char-p (peek-char nil s))
                             collect (read-char s))))
                 #+nil
                 (digit-char-p (read-char s))
                 (parse-pair)))
             (parse-pair ()
               (assert (char= (read-char s) #\[))
               (let ((left (digits-or-pair)))
                 (assert (char= (read-char s) #\,))
                 (let ((right (digits-or-pair)))
                   (assert (char= (read-char s) #\]))
                   (list left right)))))
      (parse-pair))))

#||
(parse-sailfish-number "[1,2]")
(parse-sailfish-number "[[1,2],3]")
(pprint (parse-sailfish-number "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"))
||#

(defun read-input (filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line
          collect (parse-sailfish-number line))))

#||
(read-input *filename-18-0*)
(read-input *filename-18-1*)
||#

(defun max-depth (sn)
  (if (numberp sn)
    0
    (1+ (max (max-depth (first sn))
             (max-depth (second sn))))))

(defun print-sn (sn &optional (stream *standard-output*))
  (labels ((print/i (sn)
             (if (numberp sn)
               (format stream "~d" sn)
               (progn
                 (write-char #\[ stream)
                 (print/i (first sn))
                 (write-char #\, stream)
                 (print/i (second sn))
                 (write-char #\] stream)))))
    (format stream "~&max depth: ~d~%" (max-depth sn))
    (print/i sn)
    (terpri stream)))
           

(defun add-sailfish-numbers (a b)
  (list a b))

(defun inc-right (sn n)
  (if (numberp sn)
    (+ sn n)
    (progn
      (setf (second sn)
            (inc-right (second sn) n))
      sn)))

(defun inc-left (sn n)
  (if (numberp sn)
    (+ sn n)
    (progn
      (setf (first sn)
            (inc-left (first sn) n))
      sn)))

(defun explode-sn/i (sn &optional (level 0))
  (cond ((numberp sn)
         (values nil nil nil))
        ((= level 4)
         (values (first sn) (second sn) t))
        (t
         (multiple-value-bind (left right changed)
             (explode-sn/i (first sn) (1+ level))
           (if (or left right changed)
             (progn
               (when (and left right)
                 (setf (first sn) 0))
               (when right
                 (setf (second sn)
                       (inc-left (second sn) right))
                 (setf right nil))
               (values left right changed))
             (multiple-value-bind (left right changed)
                 (explode-sn/i (second sn) (1+ level))
               (when (and left right)
                 (setf (second sn) 0))
               (when left
                 (setf (first sn)
                       (inc-right (first sn) left))
                 (setf left nil))
               (values left right changed)))))))

(defun explode-sn (sn)
  #+nil
  (progn
    (format t "~&in explode-sn; max-depth ~d~%" (max-depth sn))
    (print-sn sn))
  (multiple-value-bind (left right changed)
      (explode-sn/i sn)
    (declare (ignore left right))
    #+nil
    (progn
      (format t "~&in explode-sn (output); max-depth ~d~%" (max-depth sn))
      (print-sn sn))
    (and changed sn)))

#||
(max-depth (parse-sailfish-number "[[[[[9,8],1],2],3],4]"))

(list
 (explode-sn (parse-sailfish-number "[[[[[9,8],1],2],3],4]"))
 (explode-sn (parse-sailfish-number "[7,[6,[5,[4,[3,2]]]]]"))
 (explode-sn (parse-sailfish-number "[[6,[5,[4,[3,2]]]],1]"))
 (explode-sn (parse-sailfish-number "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
 (explode-sn (parse-sailfish-number "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))
||#

(defun split-sn/i (sn)
  (if (numberp sn)
    (if (< sn 10)
      nil
      (list (truncate sn 2) (- sn (truncate sn 2))))
    (let ((left (split-sn/i (first sn))))
      (if left
        (progn
          (setf (first sn) left)
          sn)
        (let ((right (split-sn/i (second sn))))
          (if right
            (progn
              (setf (second sn) right)
              sn)
            nil))))))

(defun split-sn (sn)
  (split-sn/i sn))

#||
(list (split-sn (parse-sailfish-number "[[[[0,7],4],[15,[0,13]]],[1,1]]"))
      (split-sn (parse-sailfish-number "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")))
||#



(defun reduce-sn/i (sn)
  (or (let ((res (explode-sn sn)))
        (when res
          #+nil
          (progn
            (format t "~&explode -> ")
            (print-sn res))
          res))
      (let ((res (split-sn sn)))
        (when res
          #+nil
          (progn
            (format t "~&split -> ")
            (print-sn res))
          res))))
      

(defun reduce-sn (sn)
  (loop for num = sn then next-num
        for next-num = (reduce-sn/i sn)
        while next-num
        finally (return num)))

#||
(max-depth '((((5 11) (13 0)) ((15 14) (14 0))) ((2 (11 0)) ((10 8) (8 0)))))

(explode-sn (split-sn (split-sn '((((5 11) (13 0)) ((15 14) (14 0))) ((2 (11 0)) ((10 8) (8 0)))))))

(reduce-sn (parse-sailfish-number "[[[[15,0],[[15,8],[0,13]]],[[0,[6,6]],[[7,7],[0,9]]]],[12,[[0,[7,6]],[[7,6],[4,7]]]]]"))

(let ((sn-1 (parse-sailfish-number "[[[[4,3],4],4],[7,[[8,4],9]]]"))
      (sn-2 (parse-sailfish-number "[1,1]")))
  (let ((sn-3 (add-sailfish-numbers sn-1 sn-2)))
    (reduce-sn sn-3)))
||#

(defun magnitude-sn (sn)
  (if (numberp sn)
    sn
    (+ (* 3 (magnitude-sn (first sn)))
       (* 2 (magnitude-sn (second sn))))))

#||
(assert (= (magnitude-sn (parse-sailfish-number "[[1,2],[[3,4],5]]")) 143))
(assert (= (magnitude-sn (parse-sailfish-number "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")) 3488))
||#

(defun  day-18-1 (filename)
  (let ((numbers (read-input filename)))
    (let ((sum (first numbers)))
      (dolist (num (rest numbers))
        (setf sum (add-sailfish-numbers sum num))
        (setf sum (reduce-sn sum)))
      (magnitude-sn sum))))


#||
(assert (= (day-18-1 *filename-18-0*) 4140))
(day-18-1 *filename-18-1*)
||#

(defun day-18-2 (filename)
  (let ((numbers-as-strings
         (with-open-file (f filename)
           (loop for line = (read-line f nil)
                 while line
                 collect line))))
    (loop for i from 0
          for a in numbers-as-strings
          maximizing (loop for j from 0
                           for b in numbers-as-strings
                           maximizing (if (= i j)
                                        0
                                        (magnitude-sn
                                         (reduce-sn
                                          (add-sailfish-numbers
                                           (parse-sailfish-number a)
                                           (parse-sailfish-number b)))))))))


#||
(assert (= (day-18-2 *filename-18-0*) 3993))
(day-18-2 *filename-18-1*)
||#
                           
