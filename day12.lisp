(in-package #:aoc2021)

(defpackage #:aoc2021/day12
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day12)

(defparameter *filename-12-0* (merge-pathnames "input-12-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-12-1* (merge-pathnames "input-12-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (loop with map = (make-hash-table :test 'equal)
          for line = (read-line f nil)
          while line
          do (or (cl-ppcre:register-groups-bind (a b)
                     ("^([^-]+)-([^-]+)" line)
                   (push b (gethash a map))
                   (push a (gethash b map))
                   t)
                 (error "unexpected input ~s" line))
          finally (return map))))
                 
#||
(let ((map (read-input *filename-12-0*)))
  (maphash (lambda (k v)
             (format t "~&~a -> ~a~%" k v))
           map))
||#

(defun day-12-1 (filename)
  (let ((map (read-input filename)))
    (labels ((visit (cave visited)
               (cond ((string= cave "end")
                      1)
                     ((and (every 'lower-case-p cave)
                           (member cave visited :test 'string=))
                      0)
                     (t
                      (loop for next in (gethash cave map)
                            for paths = (visit next (cons cave visited))
                            summing paths)))))
      (visit "start" nil))))

#||
(assert (= (day-12-1 *filename-12-0*) 226))
(day-12-1 *filename-12-1*)
||#

(defun day-12-2 (filename)
  (let ((map (read-input filename)))
    (labels ((visit (cave visited small-cave-visited-p)
               (cond ((string= cave "end")
                      1)
                     ((and small-cave-visited-p
                           (every 'lower-case-p cave)
                           (member cave visited :test 'string=))
                      0)
                     (t
                      (loop with scvp = (or small-cave-visited-p
                                            (and
                                             (every 'lower-case-p cave)
                                             (not (null (member cave visited :test 'string=)))))
                            for next in (remove "start" (gethash cave map) :test 'string=)
                            for paths = (visit next (cons cave visited) scvp)
                            summing paths)))))
      (visit "start" nil nil))))
#||
(assert (= (day-12-2 *filename-12-0*) 3509))
(day-12-2 *filename-12-1*)
||#