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

(defun pos (pos)
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
   (beacons :accessor beacons :initarg :beacons)))

(defun make-scanner (id beacons)
  (make-instance 'scanner :id id :beacons beacons))

(defmethod print-object ((scanner scanner) stream)
  (print-unreadable-object (scanner stream :type t)
    (format stream "~d" (id scanner))))

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
    ((- z) (- x) (- y))


    ;; additional
    ((- y) (- z) x)
    ((- y) z (- x))
    (y (- z) (- x))
    (y z x)
    
#||    
    ((- y) z (- x))
    (y z x)
    (x (- z) (- y))
||#

    ))

#+nil
(defparameter *permutations*
  '((x y z)
    (x y (- z))
    (x (- y) z)
    (x (- y) (- z))
    
    ((- x) z y)
    ((- x) (- z) y)
    ((- x) z (- y))
    ((- x) (- z) (- y))
    
    (y x z)
    (y x (- z))
    (y (- x) z)
    (y (- x) (- z))

    ((- y) z x)
    ((- y) (- z) x)
    ((- y) z (- x))
    ((- y) (- z) (- x))

    (z x y)
    (z x (- y))
    (z (- x) y)
    (z (- x) (- y))

    ((- z) y x)
    ((- z) (- y) x)
    ((- z) y (- x))
    ((- z) (- y) (- x))))

#+nil
(defparameter *permutations*
  '((x y z)
    (x z (- y))
    (x (- y) (- z))
    (x (- z) y)

    ((- x) y z)
    ((- x) z (- y))
    ((- x) (- y) (- z))
    ((- x) (- z) y)

    (y x z)
    (y z (- x))
    (y (- x) (- z))
    (y (- z) x)

    ((- y) x z)
    ((- y) z (- x))
    ((- y) (- x) (- z))
    ((- y) (- z) x)

    (z x y)
    (z y (- x))
    (z (- x) (- y))
    (z (- y) x)

    ((- z) x y)
    ((- z) y (- x))
    ((- z) (- x) (- y))
    ((- z) (- y) x)))

#-nil
(defparameter *permutations*
  (let ((basic-permutations '((x y z)
                              (x z y)
                              (y x z)
                              (y z x)
                              (z x y)
                              (z y x))))
    (loop for (x y z) in basic-permutations
          nconc (loop for ix below 8
                      collect (list (if (zerop (logand ix 4))
                                      x
                                      (list '- x))
                                    (if (zerop (logand ix 2))
                                      y
                                      (list '- y))
                                    (if (zerop (logand ix 1))
                                      z
                                      (list '- z)))))))


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
                   for diff = (pos-diff beacon-1 beacon-2/permuted)
                     do (incf (gethash diff diffs 0))))
    (let ((max 0)
          (diff nil))
      (loop for v being the hash-value of diffs
              using (hash-key k)
              when (> v max)
                do (setf max v
                         diff k))
      (list max (apply 'make-pos diff)))))

#+nil
(defun match-scanners-with-permutator (scanner-1 scanner-2 permutator)
  (let ((diffs (make-hash-table :test 'equal)))
    (loop for beacon-1 in (beacons scanner-1)
            do (loop for beacon-2 in (beacons scanner-2)
                       for diff = (pos-diff (funcall permutator beacon-1) beacon-2)
                       do (incf (gethash diff diffs 0))))
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
          for permutation in *permutations*
          for i from 0
          for (matches diff) = (match-scanners-with-permutator scanner-1 scanner-2 permutator)
          when (>= matches 12)
          collect (list scanner-1 scanner-2 permutator diff i permutation)))

(defun match-all-scanners (scanners)
  (loop for scanner-1 in scanners
        nconc (loop for scanner-2 in scanners
                    unless (eq scanner-1 scanner-2)
                      nconc (match-scanners scanner-1 scanner-2))))

#+nil
(defun match-all-scanners (scanners)
  (let ((queue (list (first scanners)))
        (scanners-seen (make-hash-table :test 'eq))
        (scanner-chain nil))
    (loop for scanner-1 = (pop queue)
          while scanner-1
          do (unless (gethash scanner-1 scanners-seen)
               (setf (gethash scanner-1 scanners-seen) t)
               (loop for scanner-2 in scanners
                     unless (gethash scanner-2 scanners-seen)
                       do (let ((match-data (match-scanners scanner-1 scanner-2)))
                            (when match-data
                              (assert (= (length match-data) 1))
                              (destructuring-bind (sc-1 sc-2 permutator diff i permutation)
                                  (first match-data)
                                (declare (ignore sc-1 permutator diff i permutation))
                                (push (first match-data) scanner-chain)
                                (unless (gethash sc-2 scanners-seen)
                                  (push sc-2 queue))))))))
    scanner-chain))
                              
                          
    

#||
(match-scanners (first *scanners*) (second *scanners*))
(length (match-all-scanners *scanners*))
(length (remove-if-not (lambda (x) (>= x 12))
                       (match-all-scanners *scanners*)))

(match-scanners (second *scanners*) (fifth *scanners*))
(match-scanners (fifth *scanners*) (second *scanners*))

(defparameter *foo*
  (match-scanners (first *scanners*) (second *scanners*)))
(beacons (second *scanners*))

(nth 21 *permutations*)

(defparameter *scanner-chain*
  (match-all-scanners *scanners*))

(reduce '+ (mapcar 'length (mapcar 'beacons *scanners*)))

(loop for (src dst perm diff perm-id perm-exp) in *scanner-chain*
        do (format t "~&~d -> ~d; perm = ~d: ~a~%"
                   (id src) (id dst) perm-id perm-exp))

(destructuring-bind (s-1 s-2 perm diff perm-id perm-expr)
    (first *scanner-chain*)
  (let ((munge (lambda (b) (pos+ (funcall perm b) diff))))
    (format t "~&~{~a~%~}~%" (sort (copy-seq (beacons s-1)) 'pos<))
    (format t "~&***~%")
    (format t "~&~{~a~%~}~%" (sort (mapcar munge (beacons s-2)) 'pos<))))


||#

(defun count-beacons (scanner-chain)
  (let ((scanners-seen (make-hash-table :test 'eq))
        (beacons-seen (make-hash-table :test 'equal)))
    (labels ((munge-all (beacon mungers)
               (loop for b = beacon then munged-b
                     for munger in mungers
                     for munged-b = (funcall munger b)
                     finally (return b)))
             (record-points (beacons mungers)
               (loop for beacon in beacons
                     for point = (pos (munge-all beacon mungers))
                     do (setf (gethash point beacons-seen) t)))
             (process-scanner (scanner mungers)
               (setf (gethash scanner scanners-seen) t)
               (format t "~&Scanner: ~a~%" (id scanner))
               (record-points (beacons scanner) mungers)
               (loop for (nil next-scanner permutator diff) in (remove-if-not (lambda (ch)
                                                                           (eq (first ch) scanner))
                                                                         scanner-chain)
                     for munger = (lambda (pos)
                                    (pos+ (funcall permutator pos) diff))
                     unless (gethash next-scanner scanners-seen)
                     do (process-scanner next-scanner (cons munger mungers)))))
      (process-scanner (first (first scanner-chain)) (list 'identity))
      (hash-table-count beacons-seen))))

#||
(loop for (dst src nil) in *scanner-chain*
      do (format t "~&~a - > ~d~%" (id src) (id dst)))

(loop for (src dst perm diff) in *scanner-chain*
        do (format t "~d -> ~d: ~a~%" (id src) (id dst)
                   (funcall perm (pos- (make-pos 0 0 0) diff))))

(count-beacons *scanner-chain*)
||#

(defun day-19-1 (filename)
  (let ((scanners (read-input filename)))
    (let ((scanner-chain (match-all-scanners scanners)))
      (count-beacons scanner-chain))))

#||
(assert (= (day-19-1 *filename-19-0*) 79))
(day-19-1 *filename-19-1*)
||#


(defun day-19-2 (filename)
  (let ((scanners (read-input filename)))
    (let ((scanner-chain (match-all-scanners scanners)))
      (let ((scanner-positions (make-hash-table :test 'eq)))
        (labels ((process-scanner (scanner ref-pos permutator perm-desc)
                   (format t "~&Scanner ~d, ref-pos ~a, perm ~a~%" (id scanner) ref-pos perm-desc)
                   (setf (gethash scanner scanner-positions) ref-pos)
                   (loop for (nil next-scanner next-permutator diff perm-id perm-desc)
                           in (remove-if-not (lambda (ch)
                                               (eq (first ch) scanner))
                                             scanner-chain)
                         unless (gethash next-scanner scanner-positions)
                           do (let ((next-pos (pos- ref-pos (funcall next-permutator diff))))
                                (format t "~&ref-pos: ~a; diff: ~a; permuted next-pos: ~a; perm: ~a~%"
                                        ref-pos diff next-pos permutator)
                                (process-scanner next-scanner next-pos next-permutator perm-desc)))))
          (destructuring-bind (scanner-1 scanner-2 permutator diff perm-id perm-desc)
              (first scanner-chain)
            (process-scanner scanner-1 (make-pos 0 0 0) permutator perm-desc))
          (break))))))

#||
(day-19-2 *filename-19-0*)
||#
