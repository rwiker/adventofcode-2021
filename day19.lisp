(in-package #:aoc2021)

(defpackage #:aoc2021/day19
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day19)

(declaim (optimize debug))

(defparameter *filename-19-0* (merge-pathnames "input-19-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-19-1* (merge-pathnames "input-19-1.txt" (asdf:system-source-directory :aoc2021)))

(defclass pos ()
  ((x :accessor pos-x :initarg :x)
   (y :accessor pos-y :initarg :y)
   (z :accessor pos-z :initarg :z)))

(defun make-pos (x y z)
  (make-instance 'pos :x x :y y :z z))

(defmethod print-object ((pos pos) stream)
  (print-unreadable-object (pos stream :type t)
    (with-slots (x y z)
        pos
      (format stream "~d,~d,~d" x y z))))

(defun pos-pos (pos)
  (with-slots (x y z)
      pos
    (list x y z)))

(defun pos- (a b)
  (make-pos (- (pos-x a) (pos-x b))
            (- (pos-y a) (pos-y b))
            (- (pos-z a) (pos-z b))))

(defun pos+ (a b)
  (make-pos (+ (pos-x a) (pos-x b))
            (+ (pos-y a) (pos-y b))
            (+ (pos-z a) (pos-z b))))

(defun pos-diff (a b)
  (list (- (pos-x a) (pos-x b))
        (- (pos-y a) (pos-y b))
        (- (pos-z a) (pos-z b))))

(defun pos= (a b)
  (and (= (pos-x a) (pos-x b))
       (= (pos-y a) (pos-y b))
       (= (pos-z a) (pos-z b))))

(defun pos< (a b)
  (or (< (pos-x a) (pos-x b))
      (and (= (pos-x a) (pos-x b))
           (or (< (pos-y a) (pos-y b))
               (and (= (pos-y a) (pos-y b))
                    (< (pos-z a) (pos-z b)))))))

(defclass scanner ()
  ((id :accessor id :initarg :id)
   (origin :accessor origin :initform nil)
   (permutator :accessor permutator :initform nil)
   (beacons :accessor beacons :initarg :beacons)))

(defun make-scanner (id beacons)
  (make-instance 'scanner :id id :beacons beacons))

(defmethod print-object ((scanner scanner) stream)
  (print-unreadable-object (scanner stream :type t)
    (format stream "~d: ~a" (id scanner) (origin scanner))))

(defun read-input (filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil)
          while line
          for scanner-id = (or (cl-ppcre:register-groups-bind (('parse-integer id))
                                   ("^--- scanner (\\d+) ---$" line)
                                 id)
                               (error "Invalid input line; expected scanner ~a" line))
          for beacons = (loop for line = (read-line f nil)
                              while (and line (plusp (length line)))
                              collect (or (cl-ppcre:register-groups-bind (('parse-integer x y z))
                                              ("^(-?\\d+),(-?\\d+),(-?\\d+)$" line)
                                            (make-pos x y z))
                                          (error "Invalid input ~a" line)))
          collect (make-scanner scanner-id beacons))))
  
#||
(defparameter *scanners* 
  (read-input *filename-19-0*))

(defparameter *scanners* 
  (read-input *filename-19-1*))



(first (beacons (first *scanners*)))
||#

;;; Permutations. Consider a dice; pick one face and place it down.
;;; With this face down, there are 4 possible rotations. In total, 24
;;; arrangements of the dice.

;;; original version:
#+nil
(defparameter *permutations*
  '((x y z)
    (x y (- z))
    (x (- y) z)
    (x (- y) (- z))
    
    ((- x) y z)
    ((- x) y (- z))
    ((- x) (- y) z)
    ((- x) (- y) (- z))
    
    (y x z)
    (y x (- z))
    (y (- x) z)
    (y (- x) (- z))

    ((- y) x z)
    ((- y) x (- z))
    ((- y) (- x) z)
    ((- y) (- x) (- z))

    (z x y)
    (z x (- y))
    (z (- x) y)
    (z (- x) (- y))

    ((- z) x y)
    ((- z) x (- y))
    ((- z) (- x) y)
    ((- z) (- x) (- y))))

(defun negate-form (form)
  (cond ((symbolp form)
         (list '- form))
        ((and (listp form)
              (eq (first form) '-)
              (symbolp (second form)))
         (second form))
        (t
         (error "Invalid form: ~a" form))))

(defun rotate-form/x (form)
  (destructuring-bind (x y z)
      form
    (list x (negate-form z) y)))

(defun rotate-form/y (form)
  (destructuring-bind (x y z)
      form
    (list z y (negate-form x))))

(defun rotate-form/z (form)
  (destructuring-bind (x y z)
      form
    (list (negate-form y) x z)))

#||
(rotate-form/x (rotate-form/x (rotate-form/x (rotate-form/x '(x y z)))))
||#

(defun create-permutations ()
  (let ((initial (list 'x 'y 'z))
        (permutations (make-hash-table :test 'equal)))
    (labels ((recurse (form)
               (unless (gethash form permutations)
                 (setf (gethash form permutations) t)
                 (recurse (rotate-form/x form))
                 (recurse (rotate-form/y form))
                 (recurse (rotate-form/z form)))))
      (recurse initial)
      (loop for k being the hash-keys of permutations
              collect k))))
                 
(defparameter *permutations*
  (create-permutations))

(defun make-permutator-form (index)
  ;; (assert (and (integerp index) (<= 0 index 23)))
  (let ((spec (nth index *permutations*))
        (g-pos (gensym "POS-")))
    (compile nil
             `(lambda (,g-pos)
                (with-slots (x y z)
                    ,g-pos
                  (make-pos ,@spec))))))

(defun make-permutator-form/debug (index)
  (assert (and (integerp index) (<= 0 index 23)))
  (let ((spec (nth index *permutations*))
        (g-pos (gensym "POS-")))
    `(lambda (,g-pos)
       (with-slots (x y z)
           ,g-pos
         (make-pos ,@spec)))))

(defun make-all-permutators/debug ()
  (loop for i below (length *permutations*)
        do (pprint (make-permutator-form/debug i))))
  
(defun make-all-permutators ()
  (map 'vector 'identity
       (loop for i below (length *permutations*)
             collect (make-permutator-form i))))

#||
(make-permutator-form 0)
(make-permutator-form 23)
(make-all-permutators)
(make-all-permutators/debug)
||#

(defparameter *permutators*
  (make-all-permutators))

(defun match-scanners-with-permutator (scanner-1 scanner-2 permutator)
  (let ((diffs (make-hash-table :test 'equal)))
    (loop for beacon-2 in (beacons scanner-2)
          for beacon-2/permuted = (funcall permutator beacon-2)
          do (loop for beacon-1 in (beacons scanner-1)
                   for diff = (pos- beacon-1 beacon-2/permuted)
                   do (incf (gethash (pos-pos diff) diffs 0))))
    (let ((max 0)
          (diff nil))
      (loop for v being the hash-value of diffs
              using (hash-key k)
            when (> v max)
              do (setf max v
                       diff k))
      (list max (apply 'make-pos diff)))))

(defun match-scanners (scanner-1 scanner-2)
  (loop for permutator across *permutators*
        for i from 0
        for (matches offset) = (match-scanners-with-permutator scanner-1 scanner-2 permutator)
        when (>= matches 12)
          do (progn
               (setf (origin scanner-2) offset)
               (setf (beacons scanner-2)
                     (mapcar (lambda (b)
                               (pos+ offset (funcall permutator b)))
                             (beacons scanner-2))))
        until (>= matches 12)
        finally (return (>= matches 12))))

(defun match-all-scanners (scanners)
  (let ((scanner-0 (find 0 scanners :key 'id :test '=))
        (queue))
    (setf (origin scanner-0) (make-pos 0 0 0))
    (setf (permutator scanner-0) 'identity)
    (push scanner-0 queue)
    (loop for scanner-1 = (pop queue)
          while scanner-1
          do (loop for scanner-2 in scanners
                   for matched = (unless
                                     (or (not (null (origin scanner-2)))
                                         (eq scanner-1 scanner-2))
                                   (match-scanners scanner-1 scanner-2))
                   when matched
                     do (push scanner-2 queue)))
    scanners))


(defun day-19-1 (filename)
  (let ((scanners (read-input filename)))
    (match-all-scanners scanners)
    (let ((map (make-hash-table :test 'equal)))
      (loop for scanner in scanners
              do (loop for beacon in (beacons scanner)
                       do (setf (gethash (pos-pos beacon) map) t)))
      (hash-table-count map))))

#||
(assert (= (day-19-1 *filename-19-0*) 79))
(day-19-1 *filename-19-1*)
||#

(defun manhattan-distance (a b)
  (let ((diff (pos- a b)))
    (reduce '+ (mapcar 'abs (pos-pos diff)))))
  

(defun day-19-2 (filename)
  (let ((scanners (read-input filename)))
    (match-all-scanners scanners)
    (loop for scanner-1 in scanners
            maximizing (loop for scanner-2 in scanners
                               maximizing (manhattan-distance (origin scanner-1) (origin scanner-2))))))

#||
(assert (= (day-19-2 *filename-19-0*) 3621))
(day-19-2 *filename-19-1*)
||#
